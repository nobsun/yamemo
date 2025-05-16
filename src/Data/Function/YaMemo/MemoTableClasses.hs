-- # Data.Function.YaMemo.MemoTableClasses
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
{- | Memo table class -}
module Data.Function.YaMemo.MemoTableClasses
    ( -- * class
      MemoTable (..)
    , MemoTableT (..)
    ) where

class MemoTable t where
    emptyMemoTable  :: Ord a => t a b
    lookupMemoTable :: Ord a => a -> t a b -> Maybe b
    insertMemoTable :: Ord a => a -> b -> t a b -> t a b

class (Monad m) => MemoTableT t m where
    emptyMemoTableT  :: Ord a => t a (m b)
    lookupMemoTableT :: Ord a => a -> t a (m b) -> Maybe (m b)
    insertMemoTableT :: Ord a => a -> m b -> t a (m b) -> t a (m b)
  