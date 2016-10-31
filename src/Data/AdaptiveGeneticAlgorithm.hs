{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.AdaptiveGeneticAlgorithm
  ( adaptiveGeneticAlgorithm )
  where

import Control.Monad.State.Strict
import Data.AdaptiveCoordinateDescent.Internal
import Data.List ( sortBy )
import Data.Ord
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import Numeric.LinearAlgebra hiding ( toList )
import Pipes hiding ( for )
import qualified Pipes.Prelude as P
import System.Random.MWC

data GeneticAlgorithmState f = GeneticAlgorithmState
  { principalComponents :: (Vector Double, [Vector Double])
  , populationPool :: [f Double]
  , rng :: GenIO }

generateCauchy :: GenIO -> Double -> Double -> IO Double
generateCauchy rng mean scale = do
  r <- uniformR (0, 1) rng
  return $ (tan (pi * (r - 0.5)) * scale) + mean

principalComponentsFromPopulation :: Foldable f => [f Double] -> (Vector Double, [Vector Double])
principalComponentsFromPopulation population =
  let mat = normalize $ (length population><num_coordinates)
                        (mconcat $ fmap toList population)
      (evec, mat2) = rightSV mat

   in (evec, toColumns mat2)
 where
  num_coordinates = length $ toList $ head population

sortByFitness :: Monad m
              => (f Double -> m Double)
              -> [f Double]
              -> m [(f Double, Double)]
sortByFitness evaluate items = do
  evaluated_items <- for items $ \item -> (,) item <$> evaluate item
  return $ sortBy (comparing snd) evaluated_items

adaptiveGeneticAlgorithm :: (MonadIO m, Traversable f)
                         => (f Double -> m Double) -- ^ Cost function. Smaller cost = better model.
                         -> f Double
                         -> Int
                         -> Producer (f Double, Double) m void
adaptiveGeneticAlgorithm evaluate initial_params how_many_to_keep = do
  rrng <- liftIO createSystemRandom

  initial_items <- liftIO $ P.toListM (makePerturbed initial_params rrng (how_many_to_keep*2))

  best_initial_items' <- lift $ fmap (take how_many_to_keep) $ sortByFitness evaluate initial_items

  yield (head best_initial_items')

  let best_initial_items = fmap fst best_initial_items'
      pcomponents = principalComponentsFromPopulation best_initial_items

  evalStateT piper (GeneticAlgorithmState { principalComponents = pcomponents
                                          , rng = rrng
                                          , populationPool = best_initial_items })
 where
  piper = do
    current_pool <- V.fromList . populationPool <$> get
    (pcomponent_scale_vec, pcomponents) <- principalComponents <$> get
    for_ (zip [0..] pcomponents) $ \(index, pcomponent) -> do
      let pcomponent_scale = pcomponent_scale_vec `atIndex` index
      parent_index <- randomI (0, V.length current_pool-1)
      let parent = current_pool V.! parent_index
      scalef <- randomCauchy
      let final_scale = scalef * pcomponent_scale
      addCandidate (plus parent (fromVector (final_scale `scale` pcomponent) parent))

    current_candidates <- populationPool <$> get
    best_candidates <- lift $ lift $ sortByFitness evaluate current_candidates

    let truncated_candidates = take how_many_to_keep best_candidates
        pcomponents = principalComponentsFromPopulation $ fmap fst truncated_candidates

    modify $ \old -> old { principalComponents = pcomponents
                         , populationPool = fmap fst truncated_candidates }

    lift $ yield (head best_candidates)

    piper

  randomCauchy = do
    rrng <- rng <$> get
    liftIO $ generateCauchy rrng 0 1

  randomI range = do
    rrng <- rng <$> get
    liftIO $ uniformR range rrng

  addCandidate item = do
    st <- get
    put $ st { populationPool = item:populationPool st }

