-- | Module for handling music files.
module Dashboard.MusicUnit.Files (
  -- * File Extensions
  FileExtension
, fileExtension
) where

import Control.Lens (Prism', prism')
import Data.Char (isAscii, isAlphaNum)

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
