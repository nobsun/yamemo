{-# LANGUAGE FlexibleInstances #-}
-- | Utility instances for memoisation
module Data.Function.YaMemo.NumInstances where

import Control.Applicative

instance (Applicative f, Num a) => Num (f a) where
  (+) = (<*>) . ((+) <$>)
  (-) = (<*>) . ((-) <$>)
  (*) = (<*>) . ((*) <$>)
  negate = (negate <$>)
  signum = (signum <$>)
  abs    = (abs    <$>)
  fromInteger = pure . fromInteger
