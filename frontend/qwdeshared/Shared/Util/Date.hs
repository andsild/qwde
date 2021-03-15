module Shared.Util.Date
  where
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import Data.Text as T
import Text.Read as TR
import Data.Maybe
import Control.Monad (mfilter)

parseStringDate :: String -> IO Time.Day
parseStringDate s = --return $ Time.fromGregorian 2021 3 3
  let arr = Prelude.map T.unpack $ T.splitOn (T.pack "-") (T.pack s)
      yyyy = TR.readMaybe (arr!!0)  :: Maybe Integer
      mm = TR.readMaybe (arr!!1) :: Maybe Int
      dd = TR.readMaybe (arr!!2) :: Maybe Int
    in
    getTimeOfDay yyyy mm dd

getTimeOfDay :: Maybe Integer -> Maybe Int -> Maybe Int -> IO Time.Day
getTimeOfDay (Just yyyy) (Just mm) (Just dd) = return $ Time.fromGregorian yyyy mm dd
getTimeOfDay yyyy mm dd = do
  putStrLn $ "error " ++ show yyyy ++ " " ++ show mm ++ " " ++ show dd
  return $ Time.fromGregorian 1970 1 1

dateToString :: Time.Day -> String
dateToString d = T.unpack $ T.replace (T.pack "-") (T.pack "") (T.pack $ Time.showGregorian d)
