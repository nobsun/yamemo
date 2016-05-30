module Main where

import Data.Function.YaMemo
import Data.Map (Map)

fibF :: (Num a, Eq a, Num b) => (a -> b) -> a -> b
fibF _ 0 = 0
fibF _ 1 = 1
fibF f n = f (n-2) + f (n-1)

fib :: Int -> Integer
fib = memo (undefined :: Memo Map Int Integer) fibF

main :: IO ()
main = print $ fib 1000
