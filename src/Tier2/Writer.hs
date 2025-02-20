module Tier2.Writer (collectAndSumInOrder) where

import Control.Monad.Writer
import Tier0.Writer (Tree (..))
import Data.Monoid (Sum (..))

collectAndSumInOrder :: Num a => Tree a -> Writer (Sum a) [a]
collectAndSumInOrder (Leaf x) = do
    tell (Sum x) 
    return [x]
collectAndSumInOrder (Branch left x right) = do
    leftResult <- collectAndSumInOrder left
    tell (Sum x)
    rightResult <- collectAndSumInOrder right
    return (leftResult ++ [x] ++ rightResult)