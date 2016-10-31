{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Data.AdaptiveCoordinateDescent
  ( adaptiveCoordinateDescent )
  where

import Control.Monad.State.Strict
import Data.AdaptiveCoordinateDescent.Internal
import Data.Foldable
import Data.List ( sortBy )
import Data.Ord
import Data.Traversable
import Numeric.LinearAlgebra hiding ( toList )
import Pipes hiding ( for )
import qualified Pipes.Prelude as P
import System.Random.MWC

-- | Performs adaptive coordinate descent.
adaptiveCoordinateDescent :: (MonadIO m, Traversable f, Eq (f Double))
                          => (f Double -> m Double)  -- ^ Cost function. Smaller cost = better model.
                          -> f Double                -- ^ Initial parameters from which to start optimization.
                          -> Double                  -- ^ Tolerance on how small changes are allowed.
                                                     --   If no changes larger
                                                     --   than this value are
                                                     --   found, then this
                                                     --   descent function
                                                     --   returns. You probably
                                                     --   want something small
                                                     --   like 0.0000001
                          -> Int                     -- ^ How many best N candidates to keep for PCA phase.
                                                     --   Good values are
                                                     --   probably around the
                                                     --   size of number of
                                                     --   your parameters.
                                                     --   Maybe more if you
                                                     --   only have like 2
                                                     --   parameters. Probably.
                                                     --   We don't know.
                          -> Producer (f Double, Double) m (f Double, Double)
adaptiveCoordinateDescent evaluate initial_params tolerance how_many_to_keep = do
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
    let step_size = max tolerance step_size'
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
      all (\x -> x <= tolerance) new_step_sizes = yield (params, score) >> return (params, score)
  loop_it _original_params params score [] last_n_params _step_sizes new_step_sizes = do
    let num_coordinates = length (toList params)
        sorted_items = sortBy (comparing snd) last_n_params
        picked_items = take how_many_to_keep sorted_items

        items_mat = normalize $
                    ( (length picked_items><num_coordinates)
                      (mconcat $ fmap (toList . fst) picked_items) )

    let (_evec, mat2) = rightSV items_mat
    loop_it params params score (toColumns mat2) picked_items (reverse new_step_sizes) []

  loop_it _ _ _ _ _ _ _ = error "adaptiveCoordinateDescent: impossible case."

