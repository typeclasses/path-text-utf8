-- | Read and write UTF-8 text files.
module OsPath.Text.UTF8
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
    encodeUtf,
  )
where

import Control.Exception.Safe qualified as Exception
import Data.Either (Either (..))
import Data.Functor ((<$>))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as TextEncoding.Lazy
import Data.Text.Encoding.Error (UnicodeException (..))
import System.OsPath (OsPath, encodeUtf)
import System.File.OsPath qualified as OsPath.IO
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
readFile :: OsPath -> IO Text
readFile path =
  f <$> OsPath.IO.readFile' path
  where
    f bs = let !text = TextEncoding.decodeUtf8 bs in text

-- | Read the contents of a UTF-8 encoded text file
--
-- Any 'IOError' or 'UnicodeException' that occurs is caught and returned as a
-- 'ReadError' on the 'Left' side of the 'Either'. To throw these exceptions
-- instead, use 'readFile'.
tryReadFile :: OsPath -> IO (Either ReadError Text)
tryReadFile path =
  f <$> Exception.tryIO (OsPath.IO.readFile' path)
  where
    f (Left e) = Left (ReadErrorIO e)
    f (Right bs) = first ReadErrorDecode (TextEncoding.decodeUtf8' bs)

-- | Write text to a file in a UTF-8 encoding
--
-- May throw 'IOError'. To handle this error in 'Either' instead, use
-- 'tryWriteFile'.
writeFile :: OsPath -> Text -> IO ()
writeFile path text =
  OsPath.IO.writeFile path (TextEncoding.Lazy.encodeUtf8 (Text.Lazy.fromStrict text))

-- | Write text to a file in a UTF-8 encoding
--
-- Any 'IOError' that occurs is caught and returned on the 'Left' side of the
-- 'Either'. To throw the exception instead, use 'writeFile'.
tryWriteFile :: OsPath -> Text -> IO (Either WriteError ())
tryWriteFile path text = Exception.tryIO (writeFile path text)

first :: (a -> a') -> Either a b -> Either a' b
first f (Left x) = Left (f x)
first _ (Right x) = Right x
