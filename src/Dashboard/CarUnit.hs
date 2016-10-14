{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- | Module that ensures connection with the car
module Dashboard.CarUnit (
  CarData(..)
, startFetchingData
) where

import qualified Prelude as P

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Monad (forever, when)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, toJSON, toEncoding)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Numeric.Units.Dimensional.Prelude hiding (error)

import System.Clock (Clock(Monotonic), TimeSpec(sec, nsec), getTime)
import System.Log.Logger (infoM)

import System.Hardware.ELM327.Car (Car)
import System.Hardware.ELM327.Errors (OBDError)
import qualified System.Hardware.ELM327.Car as Car

-- | A car data point.
type P a = Either OBDError a


-- | Data structure containing all available information.
data CarData = CarData { dataTimestamp :: UTCTime
                       , engineCoolantTemperature :: P (ThermodynamicTemperature Double)
                       , engineFuelRate :: P (VolumeFlow Double)
                       , engineRPM :: P (Frequency Double)
                       , intakeAirTemperature :: P (ThermodynamicTemperature Double)
                       , intakeManifoldAbsolutePressure :: P (Pressure Double)
                       , massAirFlowRate :: P (MassFlow Double)
                       , throttlePosition :: P Double
                       , vehicleSpeed :: P (Velocity Double) }

instance ToJSON CarData where
    toJSON = toJSON . toCarData'
    toEncoding = toEncoding . toCarData'

-- | Start fetching car data.
startFetchingData :: Int                  -- ^ The delay between requests in milliseconds
                  -> Car IO               -- ^ The car to get the data from
                  -> TVar (Maybe CarData) -- ^ A 'TVar' that will be used to write the data to
                  -> IO ()
startFetchingData msDelay car chan = forever fetch
  where
    fetch = ensureDuration $ (Just <$> getData) >>= (atomically . writeTVar chan)
    getData = CarData <$> getCurrentTime
                      <*> Car.engineCoolantTemperature car
                      <*> Car.engineFuelRate car
                      <*> Car.engineRPM car
                      <*> Car.intakeAirTemperature car
                      <*> Car.intakeManifoldAbsolutePressure car
                      <*> Car.massAirFlowRate car
                      <*> Car.throttlePosition car
                      <*> Car.vehicleSpeed car
    ensureDuration action = do
        begin <- getTime Monotonic
        v <- action
        end <- getTime Monotonic
        let ms = fromIntegral $ (sec end P.- sec begin) P.* 1000 P.+ (nsec end P.- nsec begin) `div` 1000000
        infoM "carunit" $ "Updated car data in " ++ show ms ++ "ms"
        when (ms P.< msDelay) $ threadDelay $ (msDelay P.- ms) P.* 1000
        return v

-- | A car data point, without dimensions.
data P' a = P' { error :: Maybe String
               , value :: Maybe a
               , dim :: Maybe String }
               deriving (Generic)
instance ToJSON a => ToJSON (P' a) where

-- | Convert 'P' to 'P''
toP' :: String -> P a ->P' a
toP' _    (Left e ) = P' { error = Just (show e),  value = Nothing, dim = Nothing   }
toP' dim' (Right v) = P' { error = Nothing,        value = Just v,  dim = Just dim' }

-- | Data structure containing all available information, without dimensions.
data CarData' = CarData' { dataTimestamp' :: UTCTime
                         , engineCoolantTemperature' :: P' Double
                         , engineFuelRate' :: P' Double
                         , engineRPM' :: P' Double
                         , intakeAirTemperature' :: P' Double
                         , intakeManifoldAbsolutePressure' :: P' Double
                         , massAirFlowRate' :: P' Double
                         , throttlePosition' :: P' Double
                         , vehicleSpeed' :: P' Double }
                         deriving (Generic)
instance ToJSON CarData' where

-- | Convert 'CarData' to 'CarData''
toCarData' :: CarData -> CarData'
toCarData' c = CarData' { dataTimestamp' = dataTimestamp c
                        , engineCoolantTemperature' = toP' "deg. C" $ toDegreeCelsiusAbsolute <$> engineCoolantTemperature c
                        , engineFuelRate' = toP' "L/h" $ (/~ (liter / hour)) <$> engineFuelRate c
                        , engineRPM' = toP' "rpm" $ (P.* 60) . (/~ hertz) <$> engineRPM c
                        , intakeAirTemperature' = toP' "deg. C" $ toDegreeCelsiusAbsolute <$> intakeAirTemperature c
                        , intakeManifoldAbsolutePressure' = toP' "kPa" $ (/~ kilo pascal) <$> intakeManifoldAbsolutePressure c
                        , massAirFlowRate' = toP' "g/s" $ (/~ (gram / second)) <$> massAirFlowRate c
                        , throttlePosition' = toP' "%" $ throttlePosition c
                        , vehicleSpeed' = toP' "km/h" $ (/~ (kilo meter / hour)) <$> vehicleSpeed c }
