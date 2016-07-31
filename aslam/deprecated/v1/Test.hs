import Types
import HaskellImpl

import Control.Concurrent
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as UV

m = Model 
  (M.fromList [("scalar", [BoundedReal 0 10])])
  (M.fromList [("add", ([("scalar", "x"), ("scalar", "y")], AddE (VarE "x") (VarE "y")))])
  (M.fromList [("test", "scalar"), ("test2", "scalar")])
  []

o = Observation "add" ["test", "test2"] (RealR 5) 0.1

main = do
  (Right s) <- compile m
  x <- readMVar (fst $ s M.! "test")
  putStrLn $ show x
  r <- update m s o
  putStrLn $ show r
  x <- readMVar (fst $ s M.! "test")
  putStrLn $ show x
  readMVar (fst $ s M.! "test2")
