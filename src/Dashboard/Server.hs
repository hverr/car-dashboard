{-# LANGUAGE Rank2Types #-}
-- | Main server module.
module Dashboard.Server (
  startServer
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)

import Network.HTTP.Types (statusCode, statusMessage)
import Network.Wai (Application, Response, Request, responseStatus, requestMethod, pathInfo)
import Network.Wai.Handler.Warp (Port, run)

import Dashboard.Server.Errors (ServerErr(..), errResponse, throwError)
import Dashboard.Server.Logger (noticeS)
import Dashboard.Server.Monad (ServerT, runServer)
import Dashboard.Server.Routes (match)
import Dashboard.Server.Routes.All (routes)
import Dashboard.Settings (Settings)

-- | Start the main HTTP server.
startServer :: Port -> Settings -> IO ()
startServer port = run port . webserver

-- | The mian HTTP server.
webserver :: Settings -> Application
webserver settings request respond = either errResponse id <$> runServer settings (handleRequest request) >>= respond'
  where respond' x = do noticeS "server" $ showRequest request ++ " -> " ++ showResponse x
                        respond x
        showRequest x = show (requestMethod x) ++ " /" ++ (intercalate "/" . map unpack . pathInfo) x
        showResponse x = show (statusCode $ responseStatus x) ++ " " ++ show (statusMessage $ responseStatus x)

-- | Handle an HTTP request.
handleRequest :: Request -> ServerT IO Response
handleRequest req = fromMaybe (throwError ErrNotFound) $ match routes req <*> pure req
