-- | Collect all application routes.
module Dashboard.Server.Routes.All where

import Dashboard.Server.Routes (Route(..))

import qualified Dashboard.Server.Routes.Static as Static

routes :: [Route]
routes = concat [Static.routes]
