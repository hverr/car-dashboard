-- | Collect all application routes.
module Dashboard.Server.Routes.All where

import Dashboard.Server.Routes (Route(..))

import qualified Dashboard.Server.Routes.Api as Api
import qualified Dashboard.Server.Routes.Static as Static

-- | A list of all routes that are installed
--
-- Order is significant, as the search for the correct route for a
-- request is linear.
routes :: [Route]
routes = Api.routes ++ Static.routes
