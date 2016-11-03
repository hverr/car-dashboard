-- | Module with data structures for settings.
module Dashboard.Settings where

-- | Application wide settings.
data Settings = Settings { nodeModulesRoot :: FilePath -- ^ Root directory for node modules
                         , htmlRoot :: FilePath        -- ^ Root directory for HTML files
                         , musicCacheDir :: FilePath   -- ^ Directory where music files can be cached
                         }

-- | Default application wide settings.
defaultSettings :: Settings
defaultSettings = Settings { nodeModulesRoot = "./node_modules"
                           , htmlRoot = "./data/html"
                           , musicCacheDir = "/run/shm/car-dashboard" }
