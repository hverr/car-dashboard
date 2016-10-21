{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
-- | Module that ensures connection with the car
module Dashboard.CarUnit (
  P
, CarData(..)
, startFetchingData
) where

import qualified Prelude as P

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Control.Lens ((^.))
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson (ToJSON, toJSON, toEncoding)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Numeric.Units.Dimensional.Prelude hiding (error)

import System.Clock (Clock(Monotonic), TimeSpec(sec, nsec), getTime)
import System.Log.Logger (debugM, infoM)

import System.Hardware.ELM327.Car (Car, CarT, flushCache)
import qualified System.Hardware.ELM327.Car as Car

import Dashboard.CarUnit.Json (P(..), CarData'(CarData'))
import qualified Dashboard.CarUnit.Json as Json

-- | Data structure containing all available information.
data CarData = CarData { dataTimestamp :: UTCTime
                       , engineCoolantTemperature :: ThermodynamicTemperature Double
                       , engineFuelRate :: VolumeFlow Double
                       , engineRPM :: Frequency Double
                       , intakeAirTemperature :: ThermodynamicTemperature Double
                       , intakeManifoldAbsolutePressure :: Pressure Double
                       , massAirFlowRate :: MassFlow Double
                       , throttlePosition :: Double
                       , vehicleSpeed :: Velocity Double }
                       deriving (Show)

instance ToJSON CarData where
    toJSON = toJSON . toCarData'
    toEncoding = toEncoding . toCarData'

-- | Start fetching car data.
startFetchingData :: MonadIO m
                  => Int                  -- ^ The delay between requests in milliseconds
                  -> Car m                -- ^ The car to get the data from
                  -> TVar (Maybe CarData) -- ^ A 'TVar' that will be used to write the data to
                  -> CarT m ()
startFetchingData msDelay car chan =
    forever . ensureDuration $ do
        v <- fetch car
        liftIO . atomically $ writeTVar chan (Just v)
  where
    ensureDuration :: MonadIO m => m a -> m a
    ensureDuration action = do
        begin <- liftIO $ getTime Monotonic
        v <- action
        end <- liftIO $ getTime Monotonic
        let ms = fromIntegral $ (sec end P.- sec begin) P.* 1000 P.+ (nsec end P.- nsec begin) `div` 1000000
        liftIO . infoM "carunit" $ "Updated car data in " ++ show ms ++ "ms"
        when (ms P.< msDelay) $ liftIO . threadDelay $ (msDelay P.- ms) P.* 1000
        return v


-- | Try fetching the car data
fetch :: MonadIO m => Car m -> CarT m CarData
fetch car = do
    flushCache
    v <- CarData <$> liftIO getCurrentTime
                 <*> car ^. Car.engineCoolantTemperature
                 <*> car ^. Car.engineFuelRate
                 <*> car ^. Car.engineRPM
                 <*> car ^. Car.intakeAirTemperature
                 <*> car ^. Car.intakeManifoldAbsolutePressure
                 <*> car ^. Car.massAirFlowRate
                 <*> car ^. Car.throttlePosition
                 <*> car ^. Car.vehicleSpeed
    liftIO . debugM "carunit" $ show v
    return v

-- | Convert 'CarData' to 'CarData''
toCarData' :: CarData -> CarData'
toCarData' c = CarData' { Json.dataTimestamp = dataTimestamp c
                        , Json.engineCoolantTemperature = P "deg. C" $ toDegreeCelsiusAbsolute $ engineCoolantTemperature c
                        , Json.engineFuelRate = P "L/h" $ (/~ (liter / hour)) $ engineFuelRate c
                        , Json.engineRPM = P "rpm" $ (P.* 60) . (/~ hertz) $ engineRPM c
                        , Json.intakeAirTemperature = P "deg. C" $ toDegreeCelsiusAbsolute $ intakeAirTemperature c
                        , Json.intakeManifoldAbsolutePressure = P "kPa" $ (/~ kilo pascal) $ intakeManifoldAbsolutePressure c
                        , Json.massAirFlowRate = P "g/s" $ (/~ (gram / second)) $ massAirFlowRate c
                        , Json.throttlePosition = P "%" $ throttlePosition c
                        , Json.vehicleSpeed = P "km/h" $ (/~ (kilo meter / hour)) $ vehicleSpeed c }
