module Time (newClock, clockDelta) where

import Data.IORef
import System.Time

-- |Returns the current system time in pico seconds.
timeInPicoSeconds :: IO Integer
timeInPicoSeconds = (\(TOD s p) -> s * ps + p) `fmap` getClockTime
  where
    ps :: Integer
    ps = 10 ^ 12

-- |Converts pico seconds to seconds.
picoToSeconds :: Integer -> Double
picoToSeconds = (* 1.0e-12) . fromInteger

data Clock = Clock { clockRef :: IORef Integer }

newClock :: IO Clock
newClock = Clock `fmap` (timeInPicoSeconds >>= newIORef)

-- |Returns the number of seconds since last call to clockDelta.
clockDelta :: Clock -> IO Double
clockDelta = deltaTime . clockRef

deltaTime :: IORef Integer -> IO Double
deltaTime ref = do
  oldTime <- readIORef ref
  newTime <- timeInPicoSeconds
  writeIORef ref newTime
  return $ picoToSeconds $ newTime - oldTime
