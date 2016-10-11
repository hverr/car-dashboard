-- | Module that contains server error handling
module Dashboard.Server.Errors (
  -- * Server Errors
  ServerErr(..)
, invalidRoute
, errResponse
, errStatus

  -- * Re-exported functions
, throwError
) where

import Control.Monad.Except (throwError)

import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (Text)

import Network.HTTP.Types (Status, hContentType, internalServerError500, notFound404)
import Network.Wai (Response, responseLBS)

-- | Possible server errors.
data ServerErr = ErrInternal (Maybe Text)
               | ErrNotFound
               deriving (Show)

-- | Error to use when an invalid route was matched.
invalidRoute :: ServerErr
invalidRoute = ErrInternal $ Just "Invalid Route"

-- | Convert errors to WAI responses.
errResponse :: ServerErr -> Response
errResponse e = responseLBS (errStatus e) hs . pack . (++ "\n") . show $ e
    where hs = [(hContentType, "text/plain; charset=utf-8")]

-- | Convert errors to the correct HTTP status.
errStatus :: ServerErr -> Status
errStatus (ErrInternal _) = internalServerError500
errStatus  ErrNotFound    = notFound404
