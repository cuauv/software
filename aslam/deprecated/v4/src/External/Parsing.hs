module External.Parsing where

import           Text.Parsec
import qualified Text.Parsec.Expr     as EX
import           Text.Parsec.Language
import           Text.Parsec.String
import qualified Text.Parsec.Token    as TK

import qualified Data.HashMap.Strict  as M
import qualified Data.Text            as T
import qualified Data.Vector          as V

import           Protocol.DSL
import           Utility.Control

bool :: Parser Expr
bool = (try (reserved "true") >> return (lift True)) <|> (try (reserved "false") >> return (lift False))

int :: Parser Expr
int = (lift . (fromIntegral :: Integer -> Int)) |<< integer

real :: Parser Expr
real = lift |<< float

var :: Parser Expr
var = (VarE . T.pack) |<< identifier

vec :: Parser Expr
vec = brackets $ do
  mem <- commaSep expr
  return $ VecE $ V.fromList mem

obj :: Parser Expr
obj = braces $ do
  fields <- commaSep field
  return $ ObjE $ M.fromList fields

field :: Parser (T.Text, Expr)
field = do
  n <- identifier
  fieldSep
  e <- expr
  return (T.pack n, e)

func :: Parser Expr
func = do
  reservedOp "\\"
  args <- many1 $ parens $ do
            n <- identifier
            whiteSpace
            reserved "::"
            whiteSpace
            t <- ptype
            return (T.pack n, t)
  arrow
  body <- expr
  return $ LamE (V.fromList args) body

apply :: Parser Expr
apply = do
  func <- var <|> parens expr <|> (reserved "gaussian" >> return (BuiltInE Gaussian))
  whiteSpace
  args <- many1 expr
  return $ AppE func (V.fromList args)

binary n f = EX.Infix (reservedOp n >> return (\x y -> AppE (BuiltInE f) (V.fromList [x, y]))) EX.AssocLeft
prefix n f = EX.Prefix (reservedOp n >> return (\x -> AppE (BuiltInE f) (V.singleton x)))

ind :: Parser Expr
ind = do
  var <- var <|> parens expr
  num <- brackets integer
  return $ IndE var (fromIntegral num)

mem :: Parser Expr
mem = do
  var <- var <|> parens expr
  reservedOp "."
  n <- identifier
  return $ MemE var (T.pack n)

table = [
  [binary "*" Mul, binary "/" Div, binary "%" Mod],
  [binary "+" Add, binary "-" Sub],
  [binary ">" Gt, binary ">=" Ge, binary "==" Eq, binary "<=" Le, binary "<" Lt],
  [prefix "-" Neg, prefix "abs" Abs],
  [prefix "sin" Sin, prefix "cos" Cos, prefix "tan" Tan, prefix "asin" Asin, prefix "acos" Acos, prefix "atan" Atan, binary "atan2" Atan2]
  ]

expr :: Parser Expr
expr = EX.buildExpressionParser table (try bool <|> try real <|> try int <|> try vec <|> try obj <|> try ind <|> try mem <|> try func <|> try var <|> try (parens expr) <|> try apply)

boolt :: Parser Type
boolt = reserved "bool" >> return BoolT

intt :: Parser Type
intt = reserved "int" >> return IntT

realt :: Parser Type
realt = reserved "real" >> return RealT

vect :: Parser Type
vect = do
  tys <- brackets $ commaSep $ ptype
  return $ VecT $ V.fromList tys

ptype :: Parser Type
ptype = EX.buildExpressionParser [] (try boolt <|> try intt <|> try realt <|> try vect)

obsC :: Parser Decl
obsC = do
  var <- var <|> parens expr
  whiteSpace
  reserved "~="
  whiteSpace
  exp <- expr
  return $ ObsConD var exp Nothing

obsR :: Parser Decl
obsR = do
  var <- T.pack |<< identifier
  whiteSpace
  reserved "+="
  whiteSpace
  exp <- expr
  return $ ObsRelD (VarE var) exp 0.1 Nothing -- TODO!

conD :: Parser Decl
conD = do
  var <- T.pack |<< identifier
  whiteSpace
  reserved "="
  whiteSpace
  exp <- expr
  return $ ConD var exp

varD :: Parser Decl
varD = do
  var <- T.pack |<< identifier
  whiteSpace
  reserved ":="
  whiteSpace
  init <- initial
  return $ VarD var init

initial :: Parser VarSpec
initial = try grid

grid :: Parser VarSpec
grid = do
  rs <- brackets $ commaSep $ parens $ do
          a <- float
          whiteSpace
          reserved "-"
          whiteSpace
          b <- float
          return (a, b)
  whiteSpace
  reserved "~"
  whiteSpace
  n <- integer
  return $ RealGrid (V.fromList rs) (fromIntegral n)

estArgmaxD :: Parser Decl
estArgmaxD = do
  reservedOp "@"
  whiteSpace
  expr <- expr
  return $ EvalD expr Argmax

estProbD :: Parser Decl
estProbD = do
  ea <- expr
  reserved "?="
  eb <- expr
  return $ EvalD ea (Prob eb)

estWMeanD :: Parser Decl
estWMeanD = do
  reservedOp "%"
  whiteSpace
  expr <- expr
  return $ EvalD expr WeightedMean

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

decl :: Parser Decl
decl = EX.buildExpressionParser [] $ try obsC <|> try obsR <|> try conD <|> try varD <|> try estArgmaxD <|> try estProbD <|> try estWMeanD

parseDecl :: String -> Either ParseError Decl
parseDecl s = parse (contents decl) "<STDIN>" s

{- Lexer -}

lexer :: TK.TokenParser ()
lexer = TK.makeTokenParser $ emptyDef {
  TK.commentLine = "#",
  TK.reservedOpNames = ["+", "-", "*", "/", "%", "==", ":=", "=", ">=", ">", "<", "<=", "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "\\", "@", "~"],
  TK.reservedNames = ["->", "true", "false", "~=", "+=", "?=", "gaussian"]
}

float :: Parser Double
float = TK.float lexer

integer :: Parser Integer
integer = TK.integer lexer

parens :: Parser a -> Parser a
parens = TK.parens lexer

brackets :: Parser a -> Parser a
brackets = TK.brackets lexer

braces :: Parser a -> Parser a
braces = TK.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = TK.commaSep lexer

identifier :: Parser String
identifier = TK.identifier lexer

reserved :: String -> Parser ()
reserved = TK.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = TK.reservedOp lexer

lexeme :: Parser a -> Parser a
lexeme = TK.lexeme lexer

whiteSpace = TK.whiteSpace lexer

colon = TK.colon lexer

fieldSep = whiteSpace >> colon >> whiteSpace

arrow = whiteSpace >> reserved "->" >> whiteSpace
