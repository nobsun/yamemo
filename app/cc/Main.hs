{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Control.Applicative
import Data.Function.YaMemo
import Data.Map (Map)

-- Functional for cc (Change Coins)

coinsUS  ::  [Int]
coinsUS  =   reverse [1,5,10,25,50]
coinsGB  ::  [Int]
coinsGB  =   reverse [1,2,5,10,20,50,100,200]

memoInfo  ::  (Memo Map) ([Int], Int) Integer
memoInfo  =   undefined

ccF :: (Monad m, Applicative m) => (([Int], Int) -> m Integer) -> ([Int], Int) -> m Integer
ccF _ (_,   n)  | n < 0          = return 0
ccF _ ([],   _)                  = return 0
ccF _ ([c] , n) | n `mod` c == 0 = return 1
                | otherwise      = return 0
ccF f (ccs@(c:cs), n)            = (+) <$> f (cs,n) <*> f (ccs,n - c)

cc :: ([Int], Int) -> Integer
cc = memo memoInfo ccF

main :: IO ()
main = print $ cc (coinsGB, 2000)
