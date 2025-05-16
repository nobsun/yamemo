-- # Data.Function.YaMemo.NumInstances
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
module Data.Function.YaMemo.NumInstances
      where

import Control.Applicative

instance (Applicative f, Num a) => Num (f a) where
    (+) = (<*>) . ((+) <$>)
    (-) = (<*>) . ((-) <$>)
    (*) = (<*>) . ((*) <$>)
    negate = (negate <$>)
    signum = (signum <$>)
    abs    = (abs    <$>)
    fromInteger = pure . fromInteger
