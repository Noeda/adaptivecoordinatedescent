Adaptive Coordinate Descent
---------------------------

This package implements a type of adaptive coordinate descent:

    adaptiveCoordinateDescent :: (MonadIO m, Traversable f, Eq (f Double))
                              => (f Double -> m Double)
                              -> f Double
                              -> Producer (f Double, Double) m (f Double, Double)

This function takes 1) cost function from parameters to a score 2) initial
model from which to start optimizing.

The function yields parameters that successively have smaller scores, until it
can't find any better parameters in which case it will return, with the best
parameters and their score it could find.

Internally the optimization uses coordinate descent that adapts to the
objective function and tries to do descent on directions that look "promising".
It uses PCA on last N best candidates for parameters and then tries to do
descent in the directions of the principal components.

