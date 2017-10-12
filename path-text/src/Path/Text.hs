{-# LANGUAGE NoImplicitPrelude #-}

module Path.Text
  ( readFile'orThrow
  , readFile'either
  ) where

-- base
import Control.Applicative (pure)
import Data.Either         (Either (..))
import Data.Function       (($))
import System.IO           (IO)
import System.IO.Error     (IOError)

-- safe-exceptions
import qualified Control.Exception.Safe as Exception

-- bytestring
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- text
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as TextEncoding
import           Data.Text.Encoding.Error (UnicodeException)

-- path
import           Path (Path)
import qualified Path

data PathTextError
  = PathTextError'IO IOError
  | PathTextError'Encoding UnicodeException

readFile'orThrow :: Path base Path.File -> IO Text
readFile'orThrow path =
  do
    bs <- BS.readFile (Path.toFilePath path)
    pure (TextEncoding.decodeUtf8 bs)

readFile'either :: Path base Path.File -> IO (Either PathTextError Text)
readFile'either path =
  do
    x <- Exception.tryIO (BS.readFile (Path.toFilePath path))
    pure $ case x of
      Left e -> Left (PathTextError'IO e)
      Right bs -> decode'either bs

decode'either :: ByteString -> Either PathTextError Text
decode'either bs =
  case TextEncoding.decodeUtf8' bs of
    Left e  -> Left (PathTextError'Encoding e)
    Right t -> Right t
