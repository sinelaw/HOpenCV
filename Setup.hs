import Data.List (intercalate)
import Distribution.Simple
import System.Cmd
import System.Exit
import System.FilePath

libs :: [String]
libs = ["opencv_core", "opencv_imgproc", "opencv_highgui", "opencv_video"]

runHsc2hs :: FilePath -> IO ExitCode
runHsc2hs f = system $ "hsc2hs "++f++" "++libs'
  where libs' = intercalate " " $ map ("-L -l"++) libs

srcPath :: FilePath
srcPath = "src/OpenCV"

main :: IO ()
main = do mapM_ (runHsc2hs . (srcPath </>) . flip addExtension "hsc")
                ["ArrayOps", "FloodFill", "Drawing"]
          defaultMain
