{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, AllowAmbiguousTypes #-}

module ProblemState where
class ProblemState s a | s -> a, a -> s where
    successors :: s -> [(a, s)]
    sameType :: a -> a -> Bool
    createFinal :: s -> s
    isGoal :: s -> Bool
    reverseAction :: (a, s) -> (a, s)