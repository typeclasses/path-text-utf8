-- | Read and write UTF-8 text files.
module Path.Text.UTF8
  ( -- * Reading
    readFile,
    tryReadFile,
    ReadError (..),

    -- * Writing
    writeFile,
    tryWriteFile,
    WriteError,

    -- * Re-exports
    IOError,
    UnicodeException (DecodeError),
    parseAbsFile,
    parseRelFile,
  )
where

import Control.Exception.Safe qualified as Exception
import Data.ByteString qualified as BS
import Data.Either (Either (..))
import Data.Functor ((<$>))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error (UnicodeException (..))
import Path (Path, parseAbsFile, parseRelFile)
import Path qualified
import System.IO (IO)
import System.IO.Error (IOError)

data ReadError
  = ReadErrorIO IOError
  | ReadErrorDecode UnicodeException

type WriteError = IOError

-- | Read the contents of a UTF-8 encoded text file
--
-- May throw 'IOError' or 'UnicodeException'. To handle these errors in 'Either'
-- instead, use 'tryReadFile'.
readFile :: Path base Path.File -> IO Text
readFile path =
  f <$> BS.readFile (Path.toFilePath path)
  where
    f bs = let !text = TextEncoding.decodeUtf8 bs in text

-- | Read the contents of a UTF-8 encoded text file
--
-- Any 'IOError' or 'UnicodeException' that occurs is caught and returned as a
-- 'ReadError' on the 'Left' side of the 'Either'. To throw these exceptions
-- instead, use 'readFile'.
tryReadFile :: Path base Path.File -> IO (Either ReadError Text)
tryReadFile path =
  f <$> Exception.tryIO (BS.readFile (Path.toFilePath path))
  where
    f (Left e) = Left (ReadErrorIO e)
    f (Right bs) = first ReadErrorDecode (TextEncoding.decodeUtf8' bs)

-- | Write text to a file in a UTF-8 encoding
--
-- May throw 'IOError'. To handle this error in 'Either' instead, use
-- 'tryWriteFile'.
writeFile :: Path base Path.File -> Text -> IO ()
writeFile path text =
  BS.writeFile (Path.toFilePath path) (TextEncoding.encodeUtf8 text)

-- | Write text to a file in a UTF-8 encoding
--
-- Any 'IOError' that occurs is caught and returned on the 'Left' side of the
-- 'Either'. To throw the exception instead, use 'writeFile'.
tryWriteFile :: Path base Path.File -> Text -> IO (Either WriteError ())
tryWriteFile path text = Exception.tryIO (writeFile path text)

first :: (a -> a') -> Either a b -> Either a' b
first f (Left x) = Left (f x)
first _ (Right x) = Right x
