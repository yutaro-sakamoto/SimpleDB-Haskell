module DiskFile.Page
  ( Page
  , createNewPage
  , writeBlockInt
  , readBlockInt
  , stToIO
  , pageSize
  , byteArray
  ) where

import DiskFile.BlockId
import Data.Word (Word8)
import Control.Monad (forM_, forM)
import Data.Bits(shiftR, shiftL, (.&.), (.|.))
import Data.Array.ST
import Data.Array.IO
import Control.Monad.ST

-- | Page describes a mutable byte array
data Page = Page 
  { byteArray :: IO (IOArray Int Word8)
  , size :: Int
  }

-- | returns the size of the page (byte)
pageSize :: Page -> Int
pageSize = size

-- | create a new byte array
createNewPage :: Int -> IO Page
createNewPage size = return $ Page { byteArray = newArray (0, size - 1) 0, size = size }

-- | write an integer into a page and return the page.
writeBlockInt :: Int -> BlockInt -> Page -> IO Page
writeBlockInt index val page = do
  arr <- byteArray page
  forM_ [0..(blockIntNumBytes-1)] $ \i -> do
    let b = fromIntegral $ (val `shiftR` (i * 8)) .&. 0xFF
    writeArray arr (index + i) b
  return page

-- | read an integer from a page
readBlockInt :: Int -> Page -> IO Int
readBlockInt index page = do
  arr <- byteArray page
  bytes <- forM [0..(blockIntNumBytes-1)] $ \i ->
    readArray arr (index + i)
  return $ bytesToBlockInt 0 0 bytes
  where
    bytesToBlockInt _ val [] = val
    bytesToBlockInt n val (x:xs) =
      let newVal = (val .|. (fromIntegral x `shiftL` (n * 8)))
      in bytesToBlockInt (n + 1) newVal xs