module ForceGraph.Time (Time, newClock, clockDelta) where

import Data.IORef
import System.Time

type Time = Double

ps :: Num a => a
ps = 10 ^ (12 :: Integer)

-- |Returns the current system time in pico seconds.
timeInPicoSeconds :: IO Integer
timeInPicoSeconds = (\(TOD s p) -> s * ps + p) `fmap` getClockTime

-- |Converts pico seconds to seconds.
picoToSeconds :: Integer -> Double
picoToSeconds = (/ ps) . fromInteger

newtype Clock = Clock { clockRef :: IORef Integer }

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
