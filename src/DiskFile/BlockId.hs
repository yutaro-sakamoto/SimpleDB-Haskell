module DiskFile.BlockId
  ( BlockId(..)
  , BlockString
  , BlockInt
  , blockIntNumBytes
  ) where

import Data.Text as T
import Data.Word (Word64)
import Data.Text as T

type BlockString = T.Text
type BlockInt = Word64
blockIntNumBytes :: Int
blockIntNumBytes = 8

data BlockId = BlockId
  { fileName :: T.Text
  , blockNum :: Word64
  } deriving(Show)