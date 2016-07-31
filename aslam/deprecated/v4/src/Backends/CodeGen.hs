{-# LANGUAGE ForeignFunctionInterface #-}

module Backends.CodeGen where

import           Control.Monad.State                hiding (lift)
import           Data.Function
import           Data.List
import qualified Data.Map                           as M

import qualified LLVM.General.AST                   as L hiding (Module,
                                                          callingConvention,
                                                          type')
import qualified LLVM.General.AST                   as AST (Module)
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant          as C
import qualified LLVM.General.AST.Global            as L

{- Framework - much thanks to http://www.stephendiehl.com/llvm/ -}

data CodeGenState = CodeGenState {
  currentBlock :: L.Name,
  blocks       :: M.Map L.Name BlockState,
  blockCount   :: Int,
  count        :: Word,
  symtab       :: M.Map String L.Operand,
  names        :: M.Map String Int
}

data BlockState = BlockState {
  idx   :: Int,
  stack :: [L.Named L.Instruction],
  term  :: Maybe (L.Named L.Terminator)
}

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a } deriving (Functor, Applicative, Monad, MonadState CodeGenState)

newtype LLVM a = LLVM { unLLVM :: State AST.Module a } deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: LLVM a -> AST.Module -> AST.Module
runLLVM = execState . unLLVM

emptyModule :: String -> AST.Module
emptyModule l = L.defaultModule { L.moduleName = l }

createBlocks :: CodeGenState -> [L.BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (L.Name, BlockState) -> L.BasicBlock
makeBlock (l, (BlockState _ s (Just t))) = L.BasicBlock l s t

sortBlocks :: [(L.Name, BlockState)] -> [(L.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

emptyCodeGen :: CodeGenState
emptyCodeGen = CodeGenState (L.Name entryBlockName) M.empty 1 0 M.empty M.empty

entryBlockName :: String
entryBlockName = "entry"

execCodeGen :: CodeGen a -> CodeGenState
execCodeGen m = execState (runCodeGen m) emptyCodeGen

{- Function Definition -}

addDef :: L.Definition -> LLVM ()
addDef d = modify (\s -> s { L.moduleDefinitions = L.moduleDefinitions s ++ [d] })

defineFunc :: L.Type -> String -> [(L.Type, L.Name)] -> [L.BasicBlock] -> LLVM ()
defineFunc retType funcName argTypes body = addDef $
  L.GlobalDefinition $ L.functionDefaults {
    L.name = L.Name funcName,
    L.parameters = (map (\(x, y) -> L.Parameter x y []) argTypes, False),
    L.returnType = retType,
    L.basicBlocks = body,
    L.callingConvention = CC.Fast -- http://llvm.org/docs/LangRef.html#callingconv
  }

externFunc x y z = defineFunc x y z []

defineConst :: L.Type -> String -> C.Constant -> LLVM ()
defineConst constType constName body = addDef $
  L.GlobalDefinition $ L.globalVariableDefaults {
    L.name = L.Name constName,
    L.type' = constType,
    L.initializer = Just body
  }

defineVariable :: L.Type -> String -> LLVM ()
defineVariable varType varName = addDef $
  L.GlobalDefinition $ L.globalVariableDefaults {
    L.name = L.Name varName,
    L.type' = varType,
    L.initializer = Nothing
  }

{- CodeGen Manipulation -}

entry :: CodeGen L.Name
entry = gets currentBlock

addBlock :: String -> CodeGen L.Name
addBlock n = do
  ind <- gets blockCount
  nms <- gets names
  let (qn, names') = uniqueName n nms
  modify $ \s -> s { blocks = M.insert (L.Name qn) (emptyBlock ind) (blocks s), blockCount = ind + 1, names = names' }
  return $ L.Name qn

setBlock :: L.Name -> CodeGen L.Name
setBlock n = do
  modify $ \s -> s { currentBlock = n }
  return n

getBlock :: CodeGen L.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> CodeGen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s) }

current :: CodeGen BlockState
current = do
  c <- gets currentBlock
  b <- gets blocks
  case M.lookup c b of
    Just x  -> return x
    Nothing -> error ("LLVM error: no such block - " ++ show c)

uniqueName :: String -> M.Map String Int -> (String, M.Map String Int)
uniqueName x y =
  case M.lookup x y of
    Just i  -> (x ++ show i, M.insert x (i + 1) y)
    Nothing -> (x, M.insert x 1 y)

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

fresh :: CodeGen Word
fresh = do
  i <- gets count
  let i' = i + 1
  modify $ \s -> s { count = i' }
  return i'

local :: L.Type -> L.Name -> L.Operand
local = L.LocalReference

externf :: L.Type -> L.Name -> L.Operand
externf x y = L.ConstantOperand $ C.GlobalReference x y

assign :: String -> L.Operand -> CodeGen ()
assign var x = modify $ \s -> s { symtab = M.insert var x $ symtab s }

getvar :: String -> CodeGen L.Operand
getvar var = do
  syms <- gets symtab
  case M.lookup var syms of
    Just op -> return op
    Nothing -> error $ "Local variable not in scope: \"" ++ var ++ "\"."

instr :: L.Instruction -> CodeGen L.Operand
instr ins = do
  n   <- fresh
  blk <- current
  let i   = stack blk
      ref = L.UnName n
  modifyBlock $ blk { stack = i ++ [ref L.:= ins] }
  return $ local undefined ref {- TODO -}

terminator :: L.Named L.Terminator -> CodeGen (L.Named L.Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

fadd :: L.Operand -> L.Operand -> CodeGen L.Operand
fadd a b = instr $ L.FAdd L.NoFastMathFlags a b []

fsub :: L.Operand -> L.Operand -> CodeGen L.Operand
fsub a b = instr $ L.FSub L.NoFastMathFlags a b []

fmul :: L.Operand -> L.Operand -> CodeGen L.Operand
fmul a b = instr $ L.FMul L.NoFastMathFlags a b []

fdiv :: L.Operand -> L.Operand -> CodeGen L.Operand
fdiv a b = instr $ L.FDiv L.NoFastMathFlags a b []

cons :: C.Constant -> L.Operand
cons = L.ConstantOperand

br :: L.Name -> CodeGen (L.Named L.Terminator)
br val = terminator $ L.Do $ L.Br val []

cbr :: L.Operand -> L.Name -> L.Name -> CodeGen (L.Named L.Terminator)
cbr cond tr fl = terminator $ L.Do $ L.CondBr cond tr fl []

ret :: L.Operand -> CodeGen (L.Named L.Terminator)
ret val = terminator $ L.Do $ L.Ret (Just val) []

call :: L.Operand -> [L.Operand] -> CodeGen L.Operand
call fn args = instr $ L.Call Nothing CC.C [] (Right fn) (map (\x -> (x, [])) args) [] []

alloca :: L.Type -> CodeGen L.Operand
alloca ty = instr $ L.Alloca ty Nothing 0 []

store :: L.Operand -> L.Operand -> CodeGen L.Operand
store ptr val = instr $ L.Store False ptr val Nothing 0 []

load :: L.Operand -> CodeGen L.Operand
load ptr = instr $ L.Load False ptr Nothing 0 []

getptr :: L.Operand -> [L.Operand] -> CodeGen L.Operand
getptr ptr inds = instr $ L.GetElementPtr True ptr inds []
