{-# LANGUAGE FlexibleContexts #-}

module Data.AdaptiveCoordinateDescent.Internal
  ( normalize
  , perturb
  , makePerturbed
  , fromVector
  , generate
  , plus
  , minus
  , sphere
  , rosen )
  where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.Foldable
import Data.Traversable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Numeric.LinearAlgebra hiding ( toList )
import Pipes hiding ( for )
import System.Random.MWC

normalize :: Matrix Double -> Matrix Double
normalize mat = runST $ do
  stds <- VM.replicate columns (0, 0)
  for_ [0..columns-1] $ \column -> do
    for_ [0..rows-1] $ \row -> do
      (avg, std) <- VM.read stds column
      VM.write stds column (avg + mat `atIndex` (row, column),
                            std + (mat `atIndex` (row, column))**2)
    (avg, std) <- VM.read stds column
    VM.write stds column (avg / fromIntegral rows
                         ,sqrt $ (std / fromIntegral rows) - (avg / fromIntegral rows)**2)

  fstds <- V.unsafeFreeze stds

  return $ (rows><columns) $ generate (rows*columns) $ \offset ->
             let row = offset `div` columns
                 column = offset `mod` columns

                 (avg, _var) = fstds V.! column -- variance ignored for now

              in ((mat `atIndex` (row, column)) - avg)
 where
  (rows, columns) = size mat

perturb :: (PrimMonad m, Traversable f)
        => f Double
        -> Gen (PrimState m)
        -> m (f Double)
perturb thing rng = for thing $ \val -> do
  perturbance <- uniformR (-0.1, 0.1) rng
  return $ val + perturbance

makePerturbed :: (PrimMonad m, Traversable f)
              => f Double
              -> Gen (PrimState m)
              -> Int
              -> Producer (f Double) m ()
makePerturbed params rng num_items =
  replicateM_ num_items $ do
    new_values <- lift $ perturb params rng
    yield new_values

fromVector :: (Container Vector a, Traversable f) => Vector a -> f void -> f a
fromVector vec structure =
  flip evalState 0 $ for structure $ \_ -> do
    idx <- get
    put (idx+1)
    return $ vec `atIndex` idx
{-# INLINE fromVector #-}

generate' :: Int -> (Int -> a) -> [a]
generate' 0 _ = []
generate' n fun = fun (n-1):generate' (n-1) fun

generate :: Int -> (Int -> a) -> [a]
generate x fun = reverse $ generate' x fun

plus :: (Num a, Traversable f) => f a -> f a -> f a
plus tr1 tr2 = flip evalState (toList tr2) $ for tr1 $ \item -> do
  (x:rest) <- get
  put rest
  return $ item + x

minus :: (Num a, Traversable f) => f a -> f a -> f a
minus tr1 tr2 = flip evalState (toList tr2) $ for tr1 $ \item -> do
  (x:rest) <- get
  put rest
  return $ item - x

sphere :: [Double] -> Double
sphere [x, y] = sqrt $ x*x + y*y
sphere _ = error "sphere takes two arguments."

rosen :: [Double] -> Double
rosen [x, y] = (1 - x)**2 + 100*(y - x**2)**2
rosen _ = error "rosen takes two arguments."

