-- | Module with data structures for settings.
module Dashboard.Settings where

import Control.Arrow (first)
import Data.Char (isAlpha, isAscii)

-- | Application wide settings.
data Settings = Settings { nodeModulesRoot :: FilePath -- ^ Root directory for node modules
                         , htmlRoot :: FilePath        -- ^ Root directory for HTML files
                         , musicCacheDir :: FilePath   -- ^ Directory where music files can be cached
                         , playMusicCmd :: [PartialArgument] -- ^ Command used to play music
                         }

-- | Default application wide settings.
defaultSettings :: Settings
defaultSettings = Settings { nodeModulesRoot = "./node_modules"
                           , htmlRoot = "./data/html"
                           , musicCacheDir = "/run/shm/car-dashboard"
                           , playMusicCmd = defaultPlayMusicCmd }

-- | The default command to play music.
defaultPlayMusicCmd :: [PartialArgument]
defaultPlayMusicCmd = [ SArg "./car-dashboard-stream-music"
                      , SArg "--file"  , SPlaceholder "file"
                      , SArg "--title" , SPlaceholder "title"
                      , SArg "--artist", SPlaceholder "artist" ]

-- | Command to simulate playing music.
simulatePlayMusicCmd :: [PartialArgument]
simulatePlayMusicCmd = defaultPlayMusicCmd ++ [SArg "--simulate"]

-- | A sum type representing an executable command with argument placeholders.
--
-- This type can be parsed and shown.
data PartialArgument = SArg String
                     | SPlaceholder String

-- | Convert a 'PartialArgument' to a 'String'.
instance Show PartialArgument where
    show (SArg x) = x
    show (SPlaceholder x) = "%" ++ x ++ "%"

-- | Parse a 'String' to a 'PartialArgument'.
instance Read PartialArgument where
    readsPrec d (' ':xs) = readsPrec d xs
    readsPrec _ ('%':xs) = case break (== '%') xs of
        (_, []) -> []
        (v, _:xs') -> [(SPlaceholder v, xs') | all (\x -> isAscii x && isAlpha x) v]
    readsPrec d xs = map (first SArg) (readsPrec d xs)
