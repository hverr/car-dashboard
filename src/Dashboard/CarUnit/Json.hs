{-# LANGUAGE DeriveGeneric #-}
-- | Module to convert 'CarUnit' data structures to JSON with Aeson.
module Dashboard.CarUnit.Json where

import Data.Aeson (ToJSON)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

-- | A car data point, without dimensions.
data P a = P { dim :: String
             , value :: a }
             deriving (Generic)

-- | Convert A 'P'-value to JSON.
instance ToJSON a => ToJSON (P a) where

-- | Data structure containing all available information, without dimensions.
data CarData' = CarData' { dataTimestamp :: UTCTime
                         , engineCoolantTemperature :: P Double
                         , engineFuelRate :: P Double
                         , engineRPM :: P Double
                         , intakeAirTemperature :: P Double
                         , intakeManifoldAbsolutePressure :: P Double
                         , massAirFlowRate :: P Double
                         , throttlePosition :: P Double
                         , vehicleSpeed :: P Double }
                         deriving (Generic)

-- | Convert 'CarData'' to JSON.
instance ToJSON CarData' where
