{-# LANGUAGE MultiParamTypeClasses #-}
-- | Memo table classes
module Data.Function.YaMemo.MemoTableClasses (
  -- * Class
    MemoTable(..)
   ,MemoTableT(..)
    ) where

class MemoTable t where
  emptyMemoTable  :: Ord a => t a b
  lookupMemoTable :: Ord a => a -> t a b -> Maybe b
  insertMemoTable :: Ord a => a -> b -> t a b -> t a b

class (Monad m) => MemoTableT t m where
  emptyMemoTableT  :: Ord a => t a (m b)
  lookupMemoTableT :: Ord a => a -> t a (m b) -> Maybe (m b)
  insertMemoTableT :: Ord a => a -> m b -> t a (m b) -> t a (m b)
