-- | Contains routes for serving static files
module Dashboard.Server.Routes.Static (
  routes
, serveNodeModule
, serveStaticApp
) where

import Data.List (isPrefixOf)
import Data.Text (intercalate, pack, unpack)

import Network.HTTP.Types (hContentType, ok200)
import Network.Mime (defaultMimeLookup)
import Network.Wai (Response, pathInfo, responseFile)

import System.Directory (doesFileExist)

import Dashboard.Paths (getHtmlFile, getNodeModuleFile)
import Dashboard.Server.Errors (ServerErr(..), invalidRoute, throwError)
import Dashboard.Server.Monad (ServerT, liftIO)
import Dashboard.Server.Routes (Route(..))

-- | All installed routes
routes :: [Route]
routes = [serveNodeModule, serveStaticApp]

-- | Serve a static file from the node modules.
serveNodeModule :: Route
serveNodeModule = Route pred' f
  where
    pred' = (["dist"] `isPrefixOf`) . pathInfo
    f req | ("dist":fp) <- pathInfo req = (getNodeModuleFile . unpack . intercalate "/") fp >>= sendFile
          | otherwise = throwError invalidRoute

-- | Serve a static file from the application.
serveStaticApp :: Route
serveStaticApp = Route pred' f
  where
    pred' = const True
    f req | [] <- pathInfo req = f' ["index.html"]
          | otherwise = f' $ pathInfo req
    f' fp = (getHtmlFile . unpack . intercalate "/") fp >>= sendFile

-- | Respond with the contents of a file
sendFile :: FilePath -> ServerT IO Response
sendFile fp = do
    exists <- liftIO $ doesFileExist fp
    if exists then return $ responseFile ok200 hs fp Nothing
              else throwError ErrNotFound
  where
    hs = [(hContentType, defaultMimeLookup $ pack fp)]
