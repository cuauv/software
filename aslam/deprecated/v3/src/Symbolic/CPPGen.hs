{-
  C++ Generation.
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Symbolic.CPPGen where

import Auxiliary
import Protocol
import FFIAux
import Symbolic.Syntax

import Control.Monad
import Control.Concurrent
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Foreign.C as C
import Foreign.Ptr
import System.Plugins.Load
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import System.Random
import System.Posix.Directory
import System.Directory
import System.Environment

class ToC a where
  toC :: a -> String

data CType = CVoid | CInt | CDouble | CPMap | CTuple [CType] deriving (Eq)

instance ToC CType where
  toC CVoid = "void"
  toC CInt = "int"
  toC CDouble = "double"
  toC CPMap = "const PMap&"
  toC (CTuple ts) = "tuple<" ++ concat (intersperse ", " $ map toC ts) ++ ">"

data CFunc = CFunc String [(String, CType)] CBody

instance ToC CFunc where
  toC (CFunc n a (CBody t d e)) = unlines $
    ("static inline " ++ toC t ++ " " ++ n ++ "(" ++ concat (intersperse ", " $ map (\(s, t) -> toC t ++ " " ++ s) a) ++ ") {") :
    (map ((++) "\t" . toC) d ++ (if t /= CVoid then ["\treturn " ++ e ++ ";", "}"] else [e, "}"]))

data CBody = CBody CType [CDecl] CExpr

data CDecl = CDecl CType String CExpr

instance ToC CDecl where
  toC (CDecl t n e) = toC t ++ " " ++ n ++ " = " ++ e ++ ";"

type CExpr = String

data CompiledObject = CompiledObject {
  objType :: Type,
  objTimeDecay :: Maybe Double,
  objHandle :: Ptr (),
  objLock :: MVar ()
}

with l f = do
  takeMVar l
  res <- f
  putMVar l ()
  return res

data CompiledFunction = CompiledFunction {
  args :: M.HashMap String Type,
  rest :: Type,
  updateOn :: M.HashMap String [Wrapped] -> Repr -> Double -> IO () -- parameter name, object to use for update, expected, delta
}

data CompiledModel = CompiledModel {
  cmodelFunctions :: M.HashMap String CompiledFunction,
  cmodelObjects :: M.HashMap String CompiledObject,
  cmodelNorm :: CompiledObject -> IO (),
  cmodelArgmax :: CompiledObject -> IO Repr,
  cmodelExponentiate :: CompiledObject -> Double -> IO (),
  cmodelGet :: CompiledObject -> Int -> IO Double,
  cmodelGranularity :: Int
}

temporal :: CompiledModel -> IO ()
temporal (CompiledModel _ objs normf _ expf _ _) = 
  forever $ mapM_ (\o -> case objTimeDecay o of Just x -> expf o x >> normf o; Nothing -> return ()) (M.elems objs) >> threadDelay (round 1e6)

handle :: CompiledModel -> Action -> IO Response
handle (CompiledModel funcs objs norm argmax _ get g) (Observe (Observation f p v u)) = 
  case M.lookup f funcs of
    Just (CompiledFunction a r uf) -> do
      currv <- flip mapM (M.toList p) $ \(n, x) -> case x of
        PObject x' -> (\v -> (n, (x, trans v)) )|<< (argmax $ objs M.! x')
        PActual r -> return (n, (x, trans r))
      flip mapM currv $ \x -> case x of
        (n, (PObject x, _)) -> (with $ objLock $ objs M.! x) $ uf (M.insert n [WPtr $ objHandle $ objs M.! x] (M.fromList $ map (\x -> (fst x, snd $ snd x)) currv)) v u
        (_, (PActual _, _)) -> return ()
      return Success
    Nothing -> return $ Error "Function not found."
handle (CompiledModel funcs objs norm argmax _ get g) (Estimate n) = 
  case M.lookup n objs of
    Just x -> Estimated |<< argmax x
    Nothing -> return $ Error "Object not found."
handle (CompiledModel funcs objs norm argmax _ get g) (Sample n) = 
  case M.lookup n objs of
    Just o@(CompiledObject t _ _ _) -> 
      let flatten x = case x of
            VectorT ts -> concat $ map flatten ts
            o -> [o]
          flattened = flatten t
          ds = map (\x -> case x of (FiniteSetT o) -> length o; BoolT -> 2; BoundedRealT _ _ -> g) flattened
          total = foldl (*) 1 ds
          inds = replicateM 10000 $ randomRIO (0, total - 1)
          rv = reverseGen t g
      in inds >>= (\inds -> (Sampled . zip (map rv inds)) |<< mapM (get o) inds)
    Nothing -> return $ Error "Object not found."

trans :: Repr -> [Wrapped]
trans (VectorR x) = concat $ map trans x
trans (BoolR b) = return $ if b then WInt 1 else WInt 0
trans (RealR d) = [WDouble $ fromRational $ toRational d]
trans (FiniteSetR x) = undefined -- hmm

nameGen = (++) "Compiled" |<< (replicateM 10 $ randomRIO ('A', 'Z'))

compile :: Model -> IO (Either String CompiledModel)
compile (Model funcs objs g) = 
  case flip mapM (M.toList funcs) (\(n, (Function args body)) -> compileExpr args body >>| \e -> (n, (args, e))) of
    Right cfuncs ->
      let funcds = map (\(n, (args, (t, b))) -> mkFuncs g n args t b) cfuncs
          funcds' = map (concat . intersperse "\n" . map toC) funcds
          functy = M.fromList $ map (\(n, (_, (t, _))) -> (n, t)) cfuncs
          opmaps = map (\(x, Object t _) -> mkPMap x (size g t)) $ M.toList objs
          out = base ++ "\n/* OBJECTS */\n\n" ++ (concat $ intersperse "\n" opmaps) ++ "\n\n/* FUNCTIONS */\n\n" ++ (concat $ intersperse "\n" funcds') ++ "\n/* END */"
          link = mkLink (M.keys objs) (concat funcds)
          header = mkHeader (M.keys objs) (concat funcds)
      in do
        dir <- nameGen
        run $ "mkdir " ++ dir
        run $ "cp Symbolic/math.cpp " ++ dir
        run $ "cp FFIAux.hs " ++ dir
        writeFile (dir ++ "/gen.cpp") out
        writeFile (dir ++ "/link.h") header
        writeFile (dir ++ "/link.cpp") link
        writeFile (dir ++ "/GenFFI.hs") $ mkFFI (M.keys objs) (concat funcds) dir
        changeWorkingDirectory dir
        run $ "g++ link.cpp -c -o link.o -std=c++11 -Ofast -fPIC"
        -- run $ "ghc link.o GenFFI.hs -optl-lstdc++"
        
        -- setEnv "TMPDIR" "tmp"
        -- run $ "ld -r link.o GenFFI.o -o GenFFI.o"
        
        {- Initialize the GHC linker; load in the compiled C++ object file and bring the symbols into our namespace so that we can link FFI code. -}
        initLinker
        loadRawObject "./link.o"
        resolveObjs $ return ()

        {- temp -}
        
        --(LoadSuccess _ x) <- load ("GenFFI.o") [] [] "normalize" :: IO (LoadStatus (Ptr () -> IO ()))
        -- (Just x) <- loadFunction m "normalize" :: IO (Maybe (Ptr () -> IO ()))
        {- x <- pdynload_ "GenFFI.o" [] [] ["link.o"] "Ptr () -> IO ()" "normalize" :: IO (LoadStatus (Ptr () -> IO ()))
        case x of
          LoadSuccess _ _ -> print "success"
          LoadFailure s -> putStrLn $ "ERROR: " ++ unlines s -}
        {- this still doesn't work, but the below crashes for different models ? -}
        
        {- Now run a Haskell interpreter session to load in the generated FFI module. -}
        let get x = unsafeRunInterpreterWithArgs [] $ loadModules ["GenFFI"] >> setImports ["GenFFI", "Foreign.C", "Foreign.Ptr", "Prelude"] >> interpret x as in do

        objects <- mapM get (M.keys objs) :: IO [Either InterpreterError (Ptr ())]
        normfxn <- get "normalize" :: IO (Either InterpreterError (Ptr () -> IO ()))
        argmaxf <- get "argmax" :: IO (Either InterpreterError (Ptr () -> IO C.CInt))
        getf <- get "get" :: IO (Either InterpreterError (Ptr () -> C.CInt -> IO C.CDouble))
        expf <- get "exponentiate" :: IO (Either InterpreterError (Ptr () -> C.CDouble -> IO ()))
        upfuncs <- sequence |<< flip mapM (M.toList funcs) 
          (\(n, Function args _) -> (\x -> sequence x >>| M.fromList) |<< flip mapM (M.keys args) (\a -> get (n ++ "_" ++ a ++ "_") >>| (\x -> x >>| (,) a)))
          :: IO (Either InterpreterError [M.HashMap String ([Wrapped] -> IO ())])

        locks <- replicateM (M.size objs) $ newMVar ()

        changeWorkingDirectory ".."
        -- run $ "rm -r " ++ dir

        let full = do
              objects <- sequence objects 
              normfxn <- normfxn
              argmaxf <- argmaxf
              upfuncs <- upfuncs
              expf <- expf
              getf <- getf
              -- upfuncs <- (sequence . map sequence) upfuncs -- :: (Either InterpreterError [[Wrapped] -> IO ()])
              return $ CompiledModel {
                cmodelFunctions = M.fromList $ zipWith (\(n, Function args _) upf -> (n, CompiledFunction args (functy M.! n) $ 
                  \m r s -> (upf M.! (head $ M.keys $ M.filter (\x -> case x of [WPtr _] -> True; _ -> False) m) $ concat (M.elems m) ++ trans r ++ trans (RealR s)))) (M.toList funcs) upfuncs,
                cmodelObjects = M.fromList $ zipWith3 (\n p l -> let Object t d = objs M.! n in (n, CompiledObject t d p l)) (M.keys objs) objects locks,
                cmodelNorm = \(CompiledObject _ _ h l) -> with l $ normfxn h,
                cmodelArgmax = \(CompiledObject t _ h l) -> (reverseGen t g . fromIntegral) |<< (with l $ argmaxf h),
                cmodelExponentiate = \(CompiledObject _ _ h l) p -> with l $ expf h $ fromRational $ toRational p,
                cmodelGet = \(CompiledObject _ t h _) i -> (fromRational . toRational) |<< getf h (fromIntegral i),
                cmodelGranularity = g
              } in
          case full of
            Right x -> return $ Right x
            Left e -> return $ Left $ "ERROR: " ++ show e
    
    Left err -> return $ Left err

mkHeader objs funcs = 
  let hfuncs = flip map funcs $ \(CFunc n a (CBody t _ _)) -> toC t ++ " " ++ n ++ "(" ++ concat (intersperse ", " $ map (\(s, t) -> (if t == CPMap then "void* " else toC t ++ " ") ++ s) a) ++ ");"
      baseb = unlines ["/* ASLAM GENERATED CODE */", "", "#pragma once", "", "#ifdef __cplusplus", "extern \"C\"{", "#endif"]
      basee = unlines ["#ifdef __cplusplus", "}", "#endif", "", "/* END */"]
      argmf = "int argmax(void* x);"
      normf = "void normalize(void* x);"
      acccf = "double get(void* x, int i);"
      expnf = "void exponentiate(void* x, double pow);"
  in baseb ++ "\n" ++ (unlines $ map (\x -> "void* " ++ x ++ " ();") objs) ++ "\n" ++ argmf ++ "\n" ++ normf ++ "\n" ++ acccf ++ "\n" ++ expnf ++ "\n" ++ unlines hfuncs ++ "\n" ++ basee

mkFFI objs funcs dir =
  let baseb = unlines ["{- ASLAM GENERATED CODE -}", "", "{-# LANGUAGE ForeignFunctionInterface #-}", "", "module GenFFI where", "", "import Foreign.C", "import Foreign.Ptr", "import FFIAux"]
      basee = unlines ["type Wrapped = FFIAux.Wrapped", "", "{- END -}"]
      argmf = "foreign import ccall unsafe \"link.h\" argmax :: Ptr () -> IO CInt"
      normf = "foreign import ccall unsafe \"link.h\" normalize :: Ptr () -> IO ()"
      acccf = "foreign import ccall unsafe \"link.h\" get :: Ptr () -> CInt -> IO CDouble"
      expnf = "foreign import ccall unsafe \"link.h\" exponentiate :: Ptr () -> CDouble -> IO ()"
      funci = flip map funcs $ \(CFunc n a (CBody t _ _)) -> "foreign import ccall unsafe \"link.h\" " ++ n ++ " :: " ++ (concat $ intersperse " -> " $ map toHS (map snd a) ++ ["IO ()"])
      mfunc = flip map funcs $ \(CFunc n a (CBody t _ _)) -> n ++ "_" ++ " [" ++ concat (intersperse ", " $ map return $ take (length a) ['a' .. 'z']) ++ "] = " ++ n ++ " " ++ concat (intersperse " " $ map (\x -> "(unwrap " ++ [x] ++ ")") $ take (length a) ['a' .. 'z'])
      obji = flip map objs $ \n -> "foreign import ccall unsafe \"link.h\" " ++ n ++ " :: Ptr ()"
  in baseb ++ "\n" ++ unlines obji ++ "\n" ++ argmf ++ "\n" ++ normf ++ "\n" ++ acccf ++ "\n" ++ expnf ++ "\n" ++ unlines funci ++ "\n" ++ unlines mfunc ++ "\n\n" ++ basee

mkLink objs funcs =
  let baseb = unlines ["/* ASLAM GENERATED CODE */", "", "#include \"link.h\"", "#include \"gen.cpp\"", "", "using namespace std;"]
      basee = unlines ["/* END */"]
      argmf = "int argmax(void* x) {\n\treturn ((PMap*) x)->argmax();\n}\n"
      normf = "void normalize(void* x) {\n\t((PMap*) x)->normalize();\n}\n"
      acccf = "double get(void* x, int i) {\n\treturn ((PMap*) x)->data[i];\n}\n"
      expnf = "void exponentiate(void* x, double pow) {\n\t((PMap*) x)->exponentiate(pow);\n}\n"
      objps = map (\x -> "void* " ++ x ++ " () { return &_" ++ x ++ "; }") objs
      trans = flip map funcs $ \(CFunc n a (CBody t _ _)) -> toC t ++ " " ++ n ++ "(" ++ concat (intersperse ", " $ map (\(s, t) -> (if t == CPMap then "void* " else toC t ++ " ") ++ s) a) ++ ") {\n\t" ++ n ++ "(" ++ concat (intersperse ", " $ map (\(s, t) -> if t == CPMap then "* (PMap*) " ++ s else s) a) ++ ");\n}\n"
  in baseb ++ "\n" ++ unlines objps ++ "\n" ++ argmf ++ "\n" ++ normf ++ "\n" ++ acccf ++ "\n" ++ expnf ++ "\n" ++ unlines trans ++ "\n\n" ++ basee

base = unlines [
  "/* ASLAM GENERATED CODE */",
  "",
  "#include <tuple>",
  "#include \"math.cpp\"",
  "",
  "using namespace std;"]

toHS CVoid = "()"
toHS CInt = "CInt"
toHS CDouble = "CDouble"
toHS CPMap = "Ptr ()"

flattenType t = 
  let flatten x = case x of
        VectorT ts -> concat $ map flatten ts
        o -> [o]
      flattened = flatten t
      ctypes = map typeX flattened
  in ctypes

flattenRepr r =
  let flatten x = case x of
        VectorR rs -> concat $ map flatten rs
        o -> [o]
      flattened = flatten r
  in r

genSplit n t = 
  let types = flattenType t
      names = map ((++) n . (:) '_' . return) $ ['a' .. 'z']
  in zip names types

genRecombine n t =
  let flatten x = case x of
        VectorT ts -> concat $ map flatten ts
        o -> [o]
      types = flattenType t
      names = map ((++) n . (:) '_' . return) $ ['a' .. 'z']
      gen x = case x of
        VectorT ts -> \n ->
          let lens = map (length . flatten) ts
              lens' = zipWith (\x i -> n + foldl (+) 0 (take i lens)) lens [0..]
          in "make_tuple(" ++ concat (intersperse ", " $ zipWith (\x y -> gen x y) ts lens') ++ ")"
        x -> (!!) names
  in CDecl (typeX t) n $ gen t 0 

mkPMap n s = "PMap _" ++ n ++ " (" ++ show s ++ ");"

size g t = 
  let flatten x = case x of
        VectorT ts -> concat $ map flatten ts
        o -> [o]
      flattened = flatten t
      ds = map (\x -> case x of (FiniteSetT o) -> length o; BoolT -> 2; BoundedRealT _ _ -> g) flattened
  in foldl (*) 1 ds

reverseGen :: Type -> Int -> (Int -> Repr)
reverseGen t g = 
  let flatten x = case x of
        VectorT ts -> concat $ map flatten ts
        o -> [o]
      flattened = flatten t
      ds = map (\x -> case x of (FiniteSetT o) -> length o; BoolT -> 2; BoundedRealT _ _ -> g) flattened
      divisors = map (foldl (*) 1) (init $ inits ds)
      revmap t = case t of
        BoolT -> \n -> BoolR $ if n `mod` 2 == 1 then True else False
        BoundedRealT xmin xmax -> \n -> RealR $ xmin + ((xmax - xmin) * fromIntegral n) / fromIntegral g
        FiniteSetT o -> \n -> FiniteSetR $ o !! n
        VectorT _ -> error "ASLAM internal error; attempted to reverse transform a VectorT."
      comb = zipWith3 (\t d s -> let f = revmap t in \n -> f $ mod (n `quot` d) s) flattened divisors ds
      unflatten (x:[]) = x
      unflatten xs = VectorR xs
  in \n -> unflatten $ map (flip ($) n) comb
    
genTransform t g n = 
  let flatten x = case x of
        VectorT ts -> concat $ map flatten ts
        o -> [o]
      flattened = flatten t
      ds = map (\x -> case x of (FiniteSetT o) -> length o; BoolT -> 2; BoundedRealT _ _ -> g) flattened
      total = foldl (*) 1 ds
      relative x = case x of
        BoolT -> \n -> "(" ++ n ++ " % 2 == 1 ? true : false)"
        BoundedRealT xmin xmax -> \n -> "(" ++ show xmin ++ " + (" ++ show xmax ++ " - " ++ show xmin ++ ") * (static_cast<double>(" ++ n ++ " % " ++ show g ++ ") / " ++ show g ++ "))"
        FiniteSetT o -> \n -> "(" ++ n ++ " % " ++ show (length o) ++ ")"
        VectorT _ -> error "ASLAM internal error; attempted to generate tranform for VectorT."
      {- Per IEEE spec, C++ integer division is floored. -}
      divisors = map (foldl (*) 1) (init $ inits ds)
      comb = zipWith ($) (map relative flattened) (map (\x -> "(" ++ n ++ " / " ++ show x ++ ")") divisors)
      gen x = case x of
        VectorT ts -> \n -> 
          let lens = map (length . flatten) ts
              lens' = zipWith (\x i -> n + foldl (+) 0 (take i lens)) lens [0..]
          in "make_tuple(" ++ concat (intersperse ", " $ zipWith (\x y -> gen x y) ts lens') ++ ")"
        x -> (!!) comb
  in gen t 0

typeX BoolT = CInt
typeX (BoundedRealT _ _) = CDouble
typeX (FiniteSetT _) = CInt
typeX (VectorT ts) = CTuple $ map typeX ts

mkGaussian' CDouble ex n = "gaussian(get<" ++ show n ++ ">(obs), sig, " ++ ex ++ ")"

mkGaussian CDouble ex = "gaussian(obs, sig, " ++ ex ++ ")"
mkGaussian (CTuple ts) ex = foldl1 (\acc x -> acc ++ " * " ++ x) $ map (\(t, n) -> mkGaussian' t ("get<" ++ show n ++ ">(" ++ ex ++ ")") n) $ zip ts [0..]

mkFuncs g n t rt (CBody ty dec expr) = 
  let base = zip (replicate (M.size t) t) (M.keys t)
  in flip map base $ \(vars, name) ->
    let cty = vars M.! name
        vrt = flip (++) [("obs", rt), ("sig", BoundedRealT 0 1)] $ M.toList vars
        xdc = concat $ map (\(n, t) -> if n == name then [] else [genRecombine n t]) vrt
        var = concat $ map (\(n, t) -> if n == name then [("temp", CPMap)] else genSplit n t) vrt
        xfm = CDecl (typeX cty) name $ genTransform cty g "i"
    in CFunc (n ++ "_" ++ name) var $ CBody CVoid xdc $
      "\tfor (int i = 0; i < temp.size; i++) {\n\t\t" ++ toC xfm ++ "\n" ++ unlines (map (\x -> "\t\t" ++ toC x) dec) ++ "\t\t" ++ "temp.data[i] *= " ++ mkGaussian ty expr ++ ";\n\t}"

applyInfix :: String -> CExpr -> CExpr -> CExpr
applyInfix x y z = "(" ++ y ++ " " ++ x ++ " " ++ z ++ ")"

apply2Arg :: String -> CExpr -> CExpr -> CExpr
apply2Arg x y z = x ++ "(" ++ y ++ ", " ++ z ++ ")"

apply1Arg :: String -> CExpr -> CExpr
apply1Arg x y = x ++ "(" ++ y ++ ")"

compileExpr :: M.HashMap String Type -> Expr -> Either String (Type, CBody)
compileExpr t (AddE x y) = dualArith t (applyInfix "+") x y $ \xmin xmax ymin ymax -> (xmin + ymin, xmax + ymax)
compileExpr t (SubE x y) = dualArith t (applyInfix "-") x y $ \xmin xmax ymin ymax -> (xmin - ymax, xmax - ymin)
compileExpr t (MulE x y) = dualArith t (applyInfix "*") x y $ \xmin xmax ymin ymax -> let o = [x * y | x <- [xmin, xmax], y <- [ymin, ymax]] in (foldl1 min o, foldl1 max o)
compileExpr t (DivE x y) = dualArith t (applyInfix "/") x y $ \_ _ _ _ -> (-1/0, 1/0)
compileExpr t (ModE x y) = dualArith t (applyInfix "%") x y $ \_ _ _ ymax -> (0, ymax)
compileExpr t (PowE x y) = dualArith t (apply2Arg "pow") x y $ \xmin xmax ymin ymax -> let o = [x ** y | x <- [xmin, xmax], y <- [ymin, ymax]] in (foldl1 min o, foldl1 max o)
compileExpr t (NegE x) = singleArith t (apply1Arg "-") x $ \xmin xmax -> (-xmax, -xmin)

compileExpr t (GtE x y) = dualComp t (applyInfix ">") x y
compileExpr t (GeE x y) = dualComp t (applyInfix ">=") x y
compileExpr t (EqE x y) = dualComp' t (applyInfix "==") x y
compileExpr t (LeE x y) = dualComp t (applyInfix "<=") x y
compileExpr t (LtE x y) = dualComp t (applyInfix "<") x y

compileExpr t (SinE x) = singleArith t (apply1Arg "sin") x $ \_ _ -> (-1, 1)
compileExpr t (CosE x) = singleArith t (apply1Arg "cos") x $ \_ _ -> (-1, 1)
compileExpr t (TanE x) = singleArith t (apply1Arg "tan") x $ \_ _ -> (-1/0, 1/0)
compileExpr t (ATan2E x y) = dualArith t (apply2Arg "atan2") x y $ \_ _ _ _ -> (-pi, pi)

compileExpr t (VecE xs) = do
  (ts, bs) <- unzip |<< mapM (compileExpr t) xs
  let decs = foldl (\acc (CBody _ dy _) -> acc ++ dy) [] bs in
    return (VectorT ts, CBody (CTuple $ map (\(CBody t _ _) -> t) bs) decs ("make_tuple(" ++ concat (intersperse ", " $ map (\(CBody _ _ e) -> e) bs) ++ ")"))

compileExpr t (IndE x i) = do
  (xt, CBody _ xd xe) <- compileExpr t x
  case xt of
    VectorT ts | length ts - 1 >= i -> return (ts !! i, CBody (typeX (ts !! i)) xd ("get<" ++ show i ++ ">(" ++ xe ++ ")"))
    _ -> Left "Can only index into a vector of sufficient length."

compileExpr t (CaseE cs) = undefined

compileExpr t (LetE x y z) = do
  (yt, CBody yt' yd ye) <- compileExpr t y
  (zt, CBody zt' zd ze) <- compileExpr (M.insert x yt t) z
  return (zt, CBody zt' (yd ++ zd ++ [CDecl yt' x ye]) ze)

compileExpr t (VarE x) = 
  case M.lookup x t of
    Just xt -> return (xt, CBody (typeX xt) [] x)
    Nothing -> Left "Variable not in scope."

compileExpr _ (LitE (BoolR b)) = return (BoolT, CBody CInt [] (if b then "true" else "false"))
compileExpr _ (LitE (RealR d)) = return (BoundedRealT d d, CBody CDouble [] (show d))
compileExpr _ (LitE (FiniteSetR _)) = Left "Cannot insert a literal for an unknown finite set. You should not need to do this."
compileExpr _ (LitE (VectorR xs)) = Left "Cannot insert vector literal; use VecE instead. You should not need to do this."

dualArith t f x y b = do
  (xt, CBody _ xd xe) <- compileExpr t x
  (yt, CBody _ yd ye) <- compileExpr t y
  case (xt, yt) of
    (BoundedRealT xmin xmax, BoundedRealT ymin ymax) -> 
      return (uncurry BoundedRealT $ b xmin xmax ymin ymax, CBody CDouble (xd ++ yd) (f xe ye))
    _ -> Left "Can only apply arithmetic on numeric type."

singleArith t f x b = do
  (xt, CBody _ xd xe) <- compileExpr t x
  case xt of
    BoundedRealT xmin xmax -> return (uncurry BoundedRealT $ b xmin xmax, CBody CDouble xd (f xe))
    _ -> Left "Can only apply arithmetic on numeric type."

dualComp t f x y = do
  (xt, CBody _ xd xe) <- compileExpr t x
  (yt, CBody _ yd ye) <- compileExpr t y
  case (xt, yt) of
    (BoundedRealT _ _, BoundedRealT _ _) ->
      return (BoolT, CBody CInt (xd ++ yd) (f xe ye))
    _ -> Left "Can only apply comparision operator to numeric type."

dualComp' t f x y = do
  (xt, CBody _ xd xe) <- compileExpr t x
  (yt, CBody _ yd ye) <- compileExpr t y
  case (xt, yt) of
    (BoundedRealT _ _, BoundedRealT _ _) -> return (BoolT, CBody CInt (xd ++ yd) (f xe ye))
    (FiniteSetT x', FiniteSetT y') | x' == y' -> return (BoolT, CBody CInt (xd ++ yd) (f xe ye))
    _ -> Left "Can only apply comparision operator to numeric type or identical finite set."
