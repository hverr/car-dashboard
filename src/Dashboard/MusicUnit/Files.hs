-- | Module for handling music files.
module Dashboard.MusicUnit.Files (
  -- * File Extensions
  FileExtension
, fileExtension
) where

import Control.Lens (Prism', prism', (^?))
import Data.Aeson (ToJSON, FromJSON, Value(..),
                   toJSON, parseJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Text as Text

-- | A valid 'FileExtension'.
--
-- Use 'fileExtension' to convert to and from 'String'.
data FileExtension = FileExtension String

-- | Convert between 'String' and 'FileExtension'
fileExtension :: Prism' String FileExtension
fileExtension = prism' conv mConv
    where conv (FileExtension s) = s
          mConv s = if all test s then Just (FileExtension s)
                                  else Nothing
          test x = isAscii x && isAlphaNum x

-- | Convert 'FileExtension' to JSON.
instance ToJSON FileExtension where
    toJSON (FileExtension s) = toJSON s

-- | Convert JSON to 'FileExtension'.
instance FromJSON FileExtension where
    parseJSON (String text)
        | Just fe <- Text.unpack text ^? fileExtension = return fe
        | otherwise = fail "invalid characters in file extension"
    parseJSON invalid = typeMismatch "FileExtension" invalid
