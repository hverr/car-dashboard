-- | Module with data structures for settings.
module Dashboard.Settings where

import Control.Arrow (first)
import Data.Char (isAlpha, isAscii)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup ((<>))

-- | Application wide settings.
data Settings = Settings { nodeModulesRoot :: FilePath -- ^ Root directory for node modules
                         , htmlRoot :: FilePath        -- ^ Root directory for HTML files
                         , musicCacheDir :: FilePath   -- ^ Directory where music files can be cached
                         , playMusicCmd :: NonEmpty PartialArgument -- ^ Command used to play music
                         }

-- | Type class to get settings quickly
class Monad m => HasSettings m where
    askSettings :: m Settings

-- | Default application wide settings.
defaultSettings :: Settings
defaultSettings = Settings { nodeModulesRoot = "./node_modules"
                           , htmlRoot = "./data/html"
                           , musicCacheDir = "/run/shm/car-dashboard"
                           , playMusicCmd = defaultPlayMusicCmd }

-- | The default command to play music.
defaultPlayMusicCmd :: NonEmpty PartialArgument
defaultPlayMusicCmd = SArg "./car-dashboard-stream-music" :|
                      [ SArg "--file"  , SPlaceholder "file"
                      , SArg "--title" , SPlaceholder "title"
                      , SArg "--artist", SPlaceholder "artist" ]

-- | Command to simulate playing music.
simulatePlayMusicCmd :: NonEmpty PartialArgument
simulatePlayMusicCmd = defaultPlayMusicCmd <> (SArg "--simulate" :| [])

-- | A sum type representing an executable command with argument placeholders.
--
-- This type can be parsed and shown.
data PartialArgument = SArg String
                     | SPlaceholder String

-- | Instantiate a 'PartialArgument'.
completePartialArgument :: (String -> Maybe String) -> PartialArgument -> Maybe String
completePartialArgument _ (SArg v) = Just v
completePartialArgument f (SPlaceholder x) = f x

-- | Instantiate a list of 'PartialArgument'.
completePartialArguments :: (String -> Maybe String) -> [PartialArgument] -> Maybe [String]
completePartialArguments f a = complete' a []
  where
    complete' [] w = Just $ reverse w
    complete' (x:xs) w
        | Just v <- completePartialArgument f x = complete' xs (v:w)
        | otherwise = Nothing

-- | Instantiate a non-empty list of 'PartialArgument'.
completePartialArgumentsNonEmpty :: (String -> Maybe String) -> NonEmpty PartialArgument -> Maybe (NonEmpty String)
completePartialArgumentsNonEmpty f (x :| xs) = do
    h <- completePartialArgument f x
    t <- completePartialArguments f xs
    return (h :| t)

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
