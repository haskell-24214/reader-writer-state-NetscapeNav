module Tier0.Writer (Tree (..), sumAndTraceInOrder) where

import Control.Monad.Writer

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving Eq

sumAndTraceInOrder :: Num a => Tree a -> Writer [a] a
sumAndTraceInOrder (Leaf x) = writer (x, [x])
sumAndTraceInOrder (Branch left x right) = do
  sumLeft <- sumAndTraceInOrder left
  tell [x]
  sumRight <- sumAndTraceInOrder right
  return (sumLeft + x + sumRight)