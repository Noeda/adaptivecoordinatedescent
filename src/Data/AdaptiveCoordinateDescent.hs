{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Data.AdaptiveCoordinateDescent
  ( adaptiveCoordinateDescent )
  where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Foldable
import Data.List ( sortBy )
import Data.Ord
import Data.Traversable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Numeric.LinearAlgebra hiding ( toList )
import Pipes hiding ( for )
import qualified Pipes.Prelude as P
import System.Random.MWC

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

{-
sphere :: [Double] -> Double
sphere [x, y] = sqrt $ x*x + y*y

rosen :: [Double] -> Double
rosen [x, y] = (1 - x)**2 + 100*(y - x**2)**2
-}

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

adaptiveCoordinateDescent :: (MonadIO m, Traversable f, Eq (f Double))
                          => (f Double -> m Double)
                          -> f Double
                          -> Producer (f Double, Double) m (f Double, Double)
adaptiveCoordinateDescent evaluate initial_params = do
  score <- lift $ evaluate initial_params
  yield (initial_params, score)

  loop_it initial_params initial_params score (toColumns $ ident $ length $ toList initial_params) [] (replicate (length $ toList initial_params) 1.0) []
 where
  k_succ = 2.0 :: Double
  k_unsucc = 0.5 :: Double

  loop_it original_params params score principal_components last_n_params step_sizes new_step_sizes | length last_n_params < 5 = do
    rng <- liftIO createSystemRandom
    items <- liftIO $ P.toListM (makePerturbed params rng 5)
    scored_items <- lift $ for (initial_params:items) $ \item -> (,) item <$> evaluate item
    loop_it original_params params score principal_components scored_items step_sizes new_step_sizes

  loop_it original_params params score (principal_component:principal_components) last_n_params (step_size':step_sizes) new_step_sizes = do
    let step_size = max 0.000001 step_size'
        scaled_pcomponent = cmap (*step_size) principal_component
        top_candidate    = params `plus` fromVector scaled_pcomponent initial_params
        bottom_candidate = params `minus` fromVector scaled_pcomponent initial_params

    top_score    <- lift $ evaluate top_candidate
    bottom_score <- lift $ evaluate bottom_candidate

    if | top_score < score
         -> do yield (top_candidate, top_score)
               loop_it original_params top_candidate top_score principal_components ((top_candidate, top_score):last_n_params) step_sizes (step_size*k_succ:new_step_sizes)
       | bottom_score < score
         -> do yield (bottom_candidate, bottom_score)
               loop_it original_params bottom_candidate bottom_score principal_components ((bottom_candidate, bottom_score):last_n_params) step_sizes (step_size*k_succ:new_step_sizes)
       | otherwise
         -> loop_it original_params params score principal_components last_n_params step_sizes (step_size*k_unsucc:new_step_sizes)

  loop_it original_params params score [] _last_n_params _step_sizes new_step_sizes
    | original_params == params &&
      all (\x -> x < 0.000001) new_step_sizes = yield (params, score) >> return (params, score)
  loop_it _original_params params score [] last_n_params _step_sizes new_step_sizes = do
    let num_coordinates = length (toList params)
        sorted_items = sortBy (comparing snd) last_n_params
        picked_items = take 10 sorted_items

        items_mat = normalize $
                    ( (length picked_items><num_coordinates)
                      (mconcat $ fmap (toList . fst) picked_items) )

    let (_evec, mat2) = rightSV items_mat
    loop_it params params score (toColumns mat2) picked_items (reverse new_step_sizes) []

  loop_it _ _ _ _ _ _ _ = error "adaptiveCoordinateDescent: impossible case."

