Adaptive Coordinate Descent
---------------------------

This package implements a type of adaptive coordinate descent:

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

This function takes 1) cost function from parameters to a score 2) initial
model from which to start optimizing 3) and 4) some hyperparameters on how to
do it.

The function yields parameters that successively have smaller scores, until it
can't find any better parameters in which case it will return, with the best
parameters and their score it could find.

Internally the optimization uses coordinate descent that adapts to the
objective function and tries to do descent on directions that look "promising".
It uses PCA on last N best candidates for parameters and then tries to do
descent in the directions of the principal components.

