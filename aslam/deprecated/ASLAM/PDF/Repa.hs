-- ASLAM Probability Density Function Implementation (Repa)
-- Christopher Goes, 2014

{-# LANGUAGE TemplateHaskell, MagicHash, BangPatterns, ExistentialQuantification #-}

module PDF.Repa (PDF(PDF), mulPDF, normalizePDF, mapPDF, sumPDF, centroidPDF, uniformMDArr) where

import Language.Haskell.TH
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V (replicate)

data PDF a = (R.Shape a) => PDF { dSize :: Int, dScale :: [Double], array :: R.Array R.U a Double }

instance (Show a) => Show (PDF a) where
    show (PDF ds dsc a) = "{ Dimension Size: " ++ show ds ++ ", Dimension Scale: " ++ show dsc ++ ", Array: " ++ show a ++ " }"

mulPDF (PDF ds1 s1 a1) (PDF ds2 s2 a2) = mulMDArr a1 a2 >>= \a -> return $ PDF ds1 s1 a

normalizePDF (PDF ds s a) = normalizeMDArr a >>= \a' -> return $ PDF ds s a'

-- f :: [dim1, dim2, ..] -> relPDF
mapPDF f (PDF ds s a) = let cind = (fromIntegral ds) / 2.0 - 0.5 in 
    mapMDArr (\d -> f $ zipWith (\s d -> s * (d - cind)) s ((map fromIntegral d) :: [Double])) a >>= \a' -> return $ PDF ds s a'

-- TODO: Return variance?
centroidPDF p@(PDF ds s a) = sequence $ map ((\(!x) -> x) . (centroidPDF' p)) [0..((R.rank $ R.extent a) - 1)]

centroidPDF' p d = mapPDF (\dims -> dims !! d) p >>= sumPDF 

sumPDF :: (R.Shape a) => PDF a -> IO Double
sumPDF (PDF ds s a) = R.sumAllP a

uniformMDArr :: Int -> Int -> Int -> Q Exp
uniformMDArr nDim dSize initV =
    let f = \x y -> AppE (AppE (ConE '(R.:.)) x) y in
    let dims = replicate nDim [| dSize :: Int |] in
    fmap (\dims -> AppE (AppE (VarE 'R.fromUnboxed) (foldl f (ConE 'R.Z) dims)) (AppE (AppE (VarE 'V.replicate) (LitE (IntegerL $ toInteger $ dSize ^ nDim))) (LitE (IntegerL (fromIntegral initV))))) (sequence dims)

mulMDArr :: (R.Shape a) => R.Array R.U a Double -> R.Array R.U a Double -> IO (R.Array R.U a Double)
mulMDArr a b = R.computeP $ a R.*^ b

normalizeMDArr :: (R.Shape a) => R.Array R.U a Double -> IO (R.Array R.U a Double)
normalizeMDArr a = R.sumAllP a >>= \ (!t) -> R.computeP (R.map (/t) a)

mapMDArr :: (R.Shape a) => ([Int] -> Double) -> R.Array R.U a Double -> IO (R.Array R.U a Double)
mapMDArr f a = R.computeP $ R.traverse a id (\fi sh -> (fi sh) * (f $ R.listOfShape sh))
