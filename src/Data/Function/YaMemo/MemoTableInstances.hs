{-# LANGUAGE MultiParamTypeClasses #-}
-- | Memo table instances
module Data.Function.YaMemo.MemoTableInstances where

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
