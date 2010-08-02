-- A simple script for generating hsc2hs #enum syntax from a series of
-- C #define statements. Copy the relevant #define lines into the file
-- cconv.txt, then run this script which will output the hsc2hs macro
-- syntax to cconv.hsc.
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE

convertLine :: String -> Maybe String
convertLine ln = listToMaybe . mrSubList $ ln =~ "^#define\\s+CV_(\\w+)\\s+.*$"
    where listToMaybe [] = Nothing
          listToMaybe [x] = Just $ "  , cv_"++x++" = CV_"++x
          listToMaybe _ = error "Unexpected match result"

main = do lines' <- mapMaybe convertLine . lines <$> readFile "cconv.txt"
          let hsc = "#{enum ColorConversion, ColorConversion" : lines' ++
                    ["  }",""]
          writeFile "cconv.hsc" $ intercalate "\n" hsc
