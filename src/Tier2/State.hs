module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

defaultRegisters :: Registers
defaultRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = state $ \(Registers a b bl acc) -> (a + b, Registers a b False (a + b))

minus :: Calculation
minus = state $ \(Registers a b bl acc) -> (a - b, Registers a b False (a - b))

productS :: Calculation
productS = state $ \(Registers a b bl acc) -> (a * b, Registers a b False (a * b))

divS :: Calculation
divS = state $ \(Registers a b bl acc) -> if b == 0 then (0, defaultRegisters) else (a `div` b, Registers a b False (a `div` b))

swap :: Calculation
swap = state $ \(Registers a b bl acc) -> (0, Registers b a bl acc)

blinkS :: Calculation
blinkS = state $ \(Registers a b bl acc) -> (0, Registers a b (not bl) acc)

accS :: Calculation
accS = state $ \(Registers a b bl acc) -> if bl then (0, Registers a acc (not bl) acc) else (0, Registers acc b (not bl) acc)

number :: Int -> Calculation
number x = state $ \(Registers a b bl acc) -> if bl then (0, Registers a x (not bl) acc) else (0, Registers x b (not bl) acc)

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "/" -> divS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\acc x -> acc >>= \_ -> commandToCalculation x) (return 0) xs

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) defaultRegisters