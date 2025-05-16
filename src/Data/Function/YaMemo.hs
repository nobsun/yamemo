-- # Data.Function.YaMemo
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Data.Function.YaMemo (
    -- * Module
      module Data.Function.YaMemo.MemoTableClasses
    -- * Type
    , Memo
    -- * Function
    , memo
    , memo'
    ) where

import Control.Monad.State
import Data.Function
import Data.Function.YaMemo.MemoTableClasses
import Data.Function.YaMemo.MemoTableInstances ()
import Data.Function.YaMemo.NumInstances ()
import Data.Tuple

type Memo t a b = a -> State (t a b) b

memoise :: (MemoTable t, Ord a) => Memo t a b -> Memo t a b
memoise mf x = do prev <- find x
                  case prev of
                    Just y  -> return y
                    Nothing -> do y    <- mf x
                                  ins x y
                                  return y
               where find k  = get >>= return . lookupMemoTable k
                     ins k v = get >>= put . insertMemoTable k v

evalMemo :: (MemoTable t, Ord a) => (Memo t) a b -> a -> b
evalMemo m v = evalState (m v) emptyMemoTable

runMemo :: (MemoTable t, Ord a) => t a b -> (Memo t) a b -> a -> (b, t a b)
runMemo tb m v = runState (m v) tb

gfun :: (b -> c) -> (c -> b) -> c
gfun = (fix .) . (.)

memoising :: (Ord a, MemoTable t)
	  => ((a -> State (t a b) b) -> Memo t a b) -> a -> State (t a b) b
memoising = gfun memoise

-- | makes memo function from functional specified by the second argument.
--   The first argument is only for imforming type of memo table will be used.
memo :: (MemoTable t, Ord a)
     => (a -> State (t a b) b)
     -> ((a -> State (t a b) b) -> Memo t a b)
     -> (a -> b)
memo g f = evalMemo (asTypeOf (memoising f) g)

-- | makes memo function which also takes and returns memo table
-- , which can be reused.
memo' :: (MemoTable t, Ord a)
     => ((a -> State (t a b) b) -> Memo t a b)
     -> t a b
     -> (a -> (t a b, b))
memo' = ((swap .) .) . flip runMemo . memoising


