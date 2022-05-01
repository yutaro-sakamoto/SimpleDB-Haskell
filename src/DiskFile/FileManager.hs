{-|
Module     : DiskFile.FileManager
Description: FileManager handles opening, reading, writing and closing files.
License    : MIT
Maintainer : yutaro-sakamoto@yutaro-sakamoto.com
Stability  : experimental
-}

module DiskFile.FileManager
  ( FileManager
  , MonadFile
  , newFileManager
  , readFileToPage
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List
import Data.Word (Word8, Word64)
import Data.Text (unpack)
import Data.Array.ST

import System.IO
import System.Directory
import System.FilePath ((</>))
import Control.Monad.State.Strict

import Control.Monad
import Control.Monad.ST(RealWorld)
import Control.Exception (finally)

import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr)

import Data.Primitive.Ptr

import DiskFile.BlockId
import DiskFile.Page

-- | File manager indicates
data FileManager = FileManager
  { 
  -- | the directory specified by directory stores all files
    directoryPath :: FilePath
  -- | unit block size
  , blockSize :: Int
  -- | openFiles stores files opened by getFile
  , openFiles :: M.Map String Handle
  }

type MonadFile = StateT FileManager IO

-- | Create a new FileManager in a directory.
-- | If the directory exists, temporary files will be removed.
newFileManager :: FilePath -> Int -> MonadFile ()
newFileManager dbDirectory blockSize = do
  directoryExists <- lift $ doesDirectoryExist dbDirectory

  if directoryExists
    -- remove leftover temporary tables
    then do
      listFiles <- lift $ listDirectory dbDirectory
      lift $ forM_ (filter (isPrefixOf "temp") listFiles) $ \tempFile -> do
        removeFile tempFile
    -- create the directory if the database is new
    else lift $ createDirectory dbDirectory

  put $ FileManager
    { directoryPath = dbDirectory
    , blockSize     = blockSize
    , openFiles     = M.empty
    }

getFile :: String -> MonadFile Handle
getFile fileName = do
  fileManager <- get
  case M.lookup fileName (openFiles fileManager) of
    Just handle -> return handle
    Nothing -> do
      handle <- lift $ openFile (directoryPath fileManager </> fileName) ReadWriteMode
      lift $ hSetBinaryMode handle True
      put $ fileManager { openFiles = M.insert fileName handle (openFiles fileManager) }
      return handle

-- | read data from a file and write data to the page.
readFileToPage :: BlockId -> Page -> MonadFile Bool
readFileToPage blockId page = do
  fileHandle <- getFile (unpack $ fileName blockId)
  fileManager <- get
  let offset = (toInteger $ blockNum blockId) * (toInteger $ blockSize fileManager)
  lift (do
    -- read from file
    hSeek fileHandle AbsoluteSeek offset
    buf <- mallocBytes (pageSize page) :: IO (Ptr Word8)
    -- copy data to Array
    finally
      (do
        bytes <- hGetBufSome fileHandle buf (pageSize page)
        if bytes < (pageSize page)
          then return False
          else readPtr buf page >> return True)
      (free buf))

readPtr :: Ptr Word8 -> Page -> IO ()
readPtr ptr page = do
  arr <- byteArray page
  forM_ [0..(pageSize page - 1)] $ \i ->
    writeArray arr i (indexOffPtr ptr i)