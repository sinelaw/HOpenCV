module Rate where
import Data.IORef
import Data.Time.Clock
import Text.Printf

trackRate :: IO (IO String)
trackRate = do numFrames <- newIORef 0
               oldRate <- newIORef ""
               startTime <- getCurrentTime >>= newIORef
               return $ do n <- readIORef numFrames
                           if n == 29 then
                             do t <- getCurrentTime
                                s <- readIORef startTime
                                let dt = realToFrac $ diffUTCTime t s :: Float
                                    msg = printf "%.2f" (30.0 / dt)
                                writeIORef startTime t
                                writeIORef numFrames 0
                                writeIORef oldRate msg
                                return msg
                           else
                             do writeIORef numFrames (n+1)
                                readIORef oldRate
