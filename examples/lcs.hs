module Main where

import System.Environment (getArgs)

import Control.Applicative (Applicative(..),(<$>))
import Control.Arrow ((***))
import Control.Monad.Identity (Identity(..))
import Data.Function (fix)
import Data.Function.YaMemo (Memo,memo',emptyMemoTable)
import Data.Map (Map)

-- Sized list
type SzList a = (Int,[a])
  
fromList :: [a] -> SzList a
fromList =  flip (,) <*> length

toList :: SzList a -> [a]
toList = snd

cons :: a -> SzList a -> SzList a
cons x (k,xs) = (k+1,x:xs)

fconv   ::  ((SzList a,SzList a) -> SzList a) -> ([a] -> [a] -> [a])
fconv f =   (toList .) . (f .) . curry (fromList *** fromList)

-- Functional for lcs
lcsF :: (Applicative m, Ord a)
     => ((SzList a, SzList a) -> m (SzList a)) -> (SzList a, SzList a) -> m (SzList a)
lcsF _ ((0,_),_) = pure (0,[])
lcsF _ (_,(0,_)) = pure (0,[])
lcsF f ((m,xxs@(x:xs)),(n,yys@(y:ys)))
  | x == y    = (cons x) <$> f ((m-1,xs),(n-1,ys))
  | otherwise = max <$> (f ((m-1,xs),(n,yys))) <*> (f ((m,xxs),(n-1,ys)))

-- LCS computation with memoisation
lcs' :: Ord a => Map (SzList a, SzList a) (SzList a)
              -> (SzList a, SzList a) -> (Map (SzList a, SzList a) (SzList a),SzList a)
lcs' =  memo' lcsF

lcs :: String -> String -> String
lcs = fconv (snd . lcs' emptyMemoTable)

-- Example

main :: IO ()
main =  do
  { args <- getArgs
  ; if length args /= 2 then usage else runLCS args
  }

runLCS :: [String] -> IO ()
runLCS [s,s'] = putStrLn (lcs s s')

-- Tree recursive version
lcs0' :: Ord a => (SzList a, SzList a) -> (SzList a)
lcs0' = runIdentity . fix lcsF

lcs0 :: String -> String -> String
lcs0 = fconv lcs0'

runForComparison :: [String] -> IO ()
runForComparison [s,s']  =  do
  { putStrLn ("Computing LCS of "++ show s ++ " and " ++ show s' ++ " with memoisation")
  ; putStrLn ("LCS: "++show (lcs s s'))
  ; putStrLn "Now computing without memoisation... Wait or interrupt."
  ; putStrLn ("LCS: "++show (lcs0 s s'))
  }

usage :: IO ()
usage =  putStrLn "Usage: memo-lcs <string> <string>"
