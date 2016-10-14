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

import Data.Time.Clock (UTCTime, getCurrentTime)

import Numeric.Units.Dimensional.Prelude

import System.Clock (Clock(Monotonic), TimeSpec(sec, nsec), getTime)
import System.Log.Logger (infoM)

import System.Hardware.ELM327.Car
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
