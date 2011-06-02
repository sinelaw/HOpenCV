module Rate where
import Data.IORef
import Data.Time.Clock
import Text.Printf

trackRate :: IO (IO String)
trackRate = do numFrames <- newIORef 0
               oldRate <- newIORef ""
               startTime <- getCurrentTime >>= newIORef
               let reportRate = 
                     do t <- getCurrentTime
                        s <- readIORef startTime
                        let dt = realToFrac $ diffUTCTime t s :: Float
                            msg = printf "%.2f" (30.0 / dt)
                        writeIORef startTime t
                        writeIORef numFrames 0
                        writeIORef oldRate msg
                        return msg
                   moveAlong n = writeIORef numFrames (n+1) >> readIORef oldRate
               return $ do n <- readIORef numFrames
                           if n == 29 then reportRate else moveAlong n

perfMon :: IO (IO (), IO String, IO ())
perfMon = do numFrames <- newIORef 0
             oldRate <- newIORef ""
             startTime <- getCurrentTime >>= newIORef
             totalTime <- newIORef (0::Double)
             let start = getCurrentTime >>= writeIORef startTime
                 stop = do t <- getCurrentTime
                           s <- readIORef startTime
                           let dt = realToFrac $ diffUTCTime t s :: Double
                           modifyIORef totalTime ((+ dt) $!)
                           n <- readIORef numFrames
                           if n == 29 
                             then do oy <- readIORef totalTime
                                     msg <- formatMsg `fmap` readIORef totalTime
                                     writeIORef numFrames 0
                                     writeIORef totalTime 0
                                     writeIORef oldRate msg
                             else writeIORef numFrames (n+1)
             return (start, readIORef oldRate, stop)
  where formatMsg = printf "%d" . (round::Double->Int) . (30.0 /)
