module Utility where

{-|
    Given a function f, this function repeatedly applies f until
    it hits a fixed point where f x = x.

    This is useful for simplification as this function allows a simple simplifier
    to proceed until it gets stuck.

    This function is different from 'Data.Function.fix' which is based around a
    different type of fix point.
-}
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = go
    where go current
            | next == current = next
            | otherwise       = go next
            where next = f current
