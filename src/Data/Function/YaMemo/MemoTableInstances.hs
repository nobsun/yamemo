-- # Data.Function.YaMemo.MemoTableInstances
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
module Data.Function.YaMemo.MemoTableInstances
      where

import Data.Function.YaMemo.MemoTableClasses
import Data.Map as M

instance MemoTable M.Map where
    emptyMemoTable  = M.empty
    lookupMemoTable = M.lookup
    insertMemoTable = M.insert

instance MemoTableT M.Map [] where
    emptyMemoTableT  = M.empty
    lookupMemoTableT = M.lookup
    insertMemoTableT = M.insert