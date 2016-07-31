module Backends.LLVMJIT where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Except                    hiding (lift)
import qualified Data.HashMap.Strict                     as M
import           Data.Maybe
import           Data.Random.Normal
import qualified Data.Set                                as S
import qualified Data.Text                               as T
import qualified Data.Vector                             as V
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Generics

import qualified LLVM.General.Analysis                   as L
import qualified LLVM.General.AST                        as L hiding (Module, callingConvention,
                                                               type')
import qualified LLVM.General.AST                        as AST (Module)
import qualified LLVM.General.AST.Attribute              as A
import qualified LLVM.General.AST.CallingConvention      as CC
import qualified LLVM.General.AST.Constant               as C
import qualified LLVM.General.AST.Float                  as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Global                 as L
import qualified LLVM.General.Context                    as L
import qualified LLVM.General.ExecutionEngine            as EE
import qualified LLVM.General.Module                     as L
import qualified LLVM.General.PassManager                as L
import qualified LLVM.General.Transforms                 as L

import           Backends.CodeGen
import           Execution.Eval
import           Execution.PhaseSpace
import           Execution.Typecheck
import           Protocol.DSL
import           Utility.Control

llvmJIT :: BackendT
llvmJIT = BackendT $ do
  var <- newMVar emptyState
  return $ (handle var, return ())

emptyState :: State
emptyState = State {
  stateModule = emptyModule "ASLAM",
  stateTypes  = M.empty
}

data State = State {
  stateModule :: AST.Module,
  stateTypes  :: M.HashMap T.Text Type
} deriving (Show, Eq, Generic)

handle :: MVar State -> Decl -> IO Resp
handle s (ConD n e) = do
  let transform = runLLVM (generateConstant n e)
  modifyMVar s $ \(State m t) -> do
    (Right ((), newMod)) <- withJIT (transform m) (\_ _ -> return ())
    return (State newMod t, RespSuccess)
handle s (VarD n p) = do
  let transform = runLLVM (generateVariable n p)
  modifyMVar s $ \(State m t) -> do
    (Right ((), newMod)) <- withJIT (transform m) (\_ _ -> return ())
    return (State newMod t, RespSuccess)
handle s (EvalD (VarE n) Argmax) = do
  modifyMVar s $ \s@(State m t) -> do
    let varType   = VecT $ V.singleton RealT
        transform = runLLVM (estFunc varType n)
    (Right (v, _)) <- withJIT (transform m) $ \mod exec -> do
      EE.withModuleInEngine exec mod $ \ee -> do
        Just func <- EE.getFunction ee (L.Name "est")
        res <- lift |<< (run func :: IO Double)
        return res
    return (s, RespExpr v)

toPhaseSpace (IntGrid x) = apriori $ multPS $ V.map (\(s, e) -> V.map lift $ V.enumFromTo s e) x
toPhaseSpace (RealGrid x n) = apriori $ multPS $ V.map (\(s, e) -> let step = ((e - s) / fromIntegral n) in V.map (\n -> s + (step * (0.5 + fromIntegral n))) $ V.enumFromTo 0 (n - 1)) x
toPhaseSpace (DirectPS x) = x

multPS vecs = if
  | V.length vecs <= 1 -> V.map (lift . V.singleton) $ V.head vecs
  | otherwise ->
      let h = V.head vecs
          r = multPS $ V.tail vecs in
      concatV $ V.map (\x -> V.map (lift . V.cons x . unlift) r) h

vars :: Expr -> S.Set T.Text
vars (VarE x) = S.singleton x
vars (VecE x) = V.foldl S.union S.empty $ V.map vars x
vars (ObjE x) = V.foldl S.union S.empty $ V.map vars $ V.fromList $ M.elems x
vars (AppE x y) = V.foldl S.union (vars x) (V.map vars y)
vars (LamE x y) = S.filter (not . flip V.elem (V.map fst x)) $ vars y
vars (LetE x y z) = S.filter ((/=) x) $ (vars y `S.union` vars z)
vars (IndE x _) = vars x
vars (MemE x _) = vars x
vars _ = S.empty
{-# INLINE vars #-}

withJIT :: AST.Module -> (L.Module -> EE.MCJIT -> IO a) -> IO (Either String (a, AST.Module))
withJIT mod func = do
  L.withContext $ \context -> do
    mcJIT context $ \exec -> do
      runExceptT $ L.withModuleFromAST context mod $ \m -> do
        L.withPassManager passSpec $ \pm -> do
          {- Debugging -}
          ll <- L.moduleLLVMAssembly m
          putStrLn ll

          {- JIT Optimize -}
          -- runPassManager pm m

          {- Grab New AST -}
          opt <- L.moduleAST m

          res <- func m exec

          return (res, opt)

mcJIT :: L.Context -> (EE.MCJIT -> IO a) -> IO a
mcJIT c = EE.withMCJIT c (Just 2) Nothing Nothing Nothing

--foreign import ccall "dynamic" haskFun :: FunPtr (IO (Ptr a)) -> IO (Ptr a)

--run :: (Storable a) => FunPtr () -> IO a
--run = (=<<) peek . haskFun . castFunPtr

--foreign import ccall "dynamic" haskFun :: FunPtr (IO (Ptr a)) -> IO (Ptr a)

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

--run :: (Storable a) => FunPtr () -> IO a
--run = (=<<) peek . haskFun . castFunPtr

run :: FunPtr a -> IO Double
run f = haskFun (castFunPtr f :: FunPtr (IO Double))

passSpec :: L.PassSetSpec
passSpec = L.defaultCuratedPassSetSpec { L.optLevel = Just 3 }

{-

  Code generation strategy:
  Split language into simplication, evaluation components.
  JIT should just evaluate expressions.

  1. Extract changeable #s (observation parameters).
  2. Create update function ==> JIT ==> cache.
  3. Call update function.

  -- How to implement closures?

-}

estFunc ty n =
  let assocTy = llvmType ty
      spaceTy = VecT $ V.fromList [ty, RealT]
      conName = L.Name $ T.unpack n in
  defineFunc assocTy "est" [] $
    createBlocks $ execCodeGen $ do
      entry <- addBlock entryBlockName
      forLoop <- addBlock "for.loop"
      forExit <- addBlock "for.exit"

      setBlock entry

      max  <- alloca $ llvmType RealT
      i    <- alloca $ llvmType IntT
      sind <- exprGen (IntE 0) undefined
      find <- exprGen (IntE 100) undefined
      step <- exprGen (IntE 1) undefined
      store i sind
      assign "ind" i
      br forLoop

      setBlock forLoop

      {-
      ifThen <- addBlock "if.then"
      ifElse <- addBlock "if.else"
      ifExit <- addBlock "if.exit"
      setBlock ifThen
      -}

      ival <- load i
      mval <- load =<< getptr (externf (llvmType spaceTy) conName) [ival, ival, ival]
      --mval <- exprGen (RealE 2) RealT
      store max mval
      --inxt <- fadd ival step
      --store i inxt

      --cond <- undefined
      --test <- exprGen (IntE 1) IntT
      --cbr test forLoop forExit
      br forExit

      setBlock forExit
      ret =<< load max

mainFunc = defineFunc (llvmType RealT) "main" [] $
  createBlocks $ execCodeGen $ do
    entry <- addBlock entryBlockName
    setBlock entry
    assign "var" $ externf (llvmType RealT) (L.Name "var")
    exprGen (VarE "var") RealT >>= ret

test :: Decl -> IO ()
test decl = do
  L.withContext $ \context -> do
    let ast = runLLVM (declGen decl >> mainFunc) (emptyModule "test")
    mcJIT context $ \exec -> do
      liftError $ L.withModuleFromAST context ast $ \m -> do
        ll <- L.moduleLLVMAssembly m
        putStrLn ll

        EE.withModuleInEngine exec m $ \ee -> do
          (Just func) <- EE.getFunction ee (L.Name "main")
          res <- lift |<< (run func :: IO Double)
          print res

  return ()

liftError = runExceptT >=> either fail return

{- AST to LLVM -}

declGen :: Decl -> LLVM ()
declGen = undefined

generateVariable n spec = do
  let conName = T.unpack n
      (PhaseSpace space) = toPhaseSpace spec
      cardinality = V.length space
      pointType   = singularTypecheck $ fst $ V.head space
      joinedType  = VecT $ V.fromList [pointType, RealT]
      totalType   = VecT $ V.replicate cardinality joinedType
  defineConst (llvmType totalType) conName $ constVal (phaseSpaceGen space)

generateConstant x y = do
  let conName = T.unpack x
  case singularTypecheck y of
    LamT tys rty -> do
      let (LamE xs res) = y
          blocks = createBlocks $ execCodeGen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM xs $ \(name, ty) -> do
              let n = T.unpack name
                  t = llvmType ty
              var <- alloca t
              store var (local t (L.Name n))
              assign n var
            exprGen res rty >>= ret
      defineFunc (llvmType rty) conName (V.toList $ V.map (\(x, y) -> (llvmType y, L.Name $ T.unpack x)) xs) blocks
    ty -> do
      let constType  = llvmType ty
          constValue = constVal y
      defineConst constType conName constValue

phaseSpaceGen :: V.Vector (Expr, Double) -> Expr
phaseSpaceGen vec = VecE $ V.map (\(x, y) -> VecE $ V.fromList [x, RealE y]) vec

exprGen :: Expr -> Type -> CodeGen L.Operand
exprGen (BoolE x) _ = return $ cons $ C.Int 1 $ if x then 1 else 0
exprGen (IntE x) _ = return $ cons $ C.Int 32 $ fromIntegral x
exprGen (RealE x) _ = return $ cons $ C.Float (F.Double x)
exprGen (IndE x y) ty = do
  x' <- exprGen x undefined
  y' <- exprGen (IntE y) undefined
  instr $ L.GetElementPtr True x' [y'] []
exprGen (VarE x) _ = getvar (T.unpack x) >>= load
exprGen (AppE (VarE x) y) ty = do
  y' <- mapM (flip exprGen undefined) y
  call (externf (llvmType ty) (L.Name $ T.unpack x)) (V.toList y')
exprGen (AppE (BuiltInE Add) y) _ = dualA fadd y
exprGen (AppE (BuiltInE Div) y) _ = dualA fdiv y
exprGen (AppE (BuiltInE Sub) y) _ = dualA fsub y
exprGen (AppE (BuiltInE Mul) y) _ = dualA fmul y

dualA func args = do
  x <- exprGen (args V.! 0) undefined
  y <- exprGen (args V.! 1) undefined
  func x y

constVal (BoolE True) = C.Int 32 1
constVal (BoolE False) = C.Int 32 0
constVal (IntE x) = C.Int 32 $ fromIntegral x
constVal (RealE x) = C.Float $ F.Double x
--constVal (VecE xs) = C.Vector $ V.toList $ V.map constVal xs
constVal (VecE xs) =
  if V.length xs == 1 then constVal $ V.head xs else C.Array (llvmType $ singularTypecheck $ V.head xs) $ V.toList $ V.map constVal xs

llvmType BoolT = llvmType IntT
llvmType IntT = L.IntegerType 32
llvmType RealT = L.FloatingPointType 64 L.IEEE
llvmType (EnumT _) = llvmType IntT -- Translated to index in set.
--llvmType (VecT ty) = L.VectorType (fromIntegral $ V.length ty) (llvmType $ V.head ty)
llvmType (VecT ty) =
  if V.length ty == 1 then llvmType $ V.head ty else L.ArrayType (fromIntegral $ V.length ty) (llvmType $ V.head ty)

singularTypecheck x =
  case typecheck M.empty x of
    Right ty -> ty
    Left err -> error $ T.unpack err
