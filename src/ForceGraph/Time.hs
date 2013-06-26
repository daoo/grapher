module ForceGraph.Time (newClock, clockDelta) where

import Data.IORef
import Data.Time

newtype Clock = Clock { clockRef :: IORef UTCTime }

newClock :: IO Clock
newClock = Clock `fmap` (getCurrentTime >>= newIORef)

-- |Returns the number of seconds since last call to clockDelta.
clockDelta :: Clock -> IO Double
clockDelta = deltaTime . clockRef

deltaTime :: IORef UTCTime -> IO Double
deltaTime ref = do
  oldTime <- readIORef ref
  newTime <- getCurrentTime
  writeIORef ref newTime
  return $ realToFrac $ diffUTCTime newTime oldTime
