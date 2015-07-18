module CalendarChart.RA where

import CalendarChart.Activities
import CalendarChart.Util
import Prelude
import Graphics.D3.Base
import Data.Either
import Data.Maybe
import Data.Date(fromString)
import Data.Traversable(traverse)
import Data.Array(mapMaybe,length,filter)
import Data.Foreign.EasyFFI

type RALine = { distance :: Number, distanceUnit :: String, activityType :: String, date :: String }

coerceDatum :: forall a. a -> D3Eff RALine
coerceDatum = unsafeForeignFunction ["x", ""]
  "{ distance: Number(x.Distance), distanceUnit: x.DistanceUnit, activityType: x.Type, date: x.Date }"

mkActRA input = do
  dist <- case input.distanceUnit of
    "Mile" -> Just $ 1609.0 * input.distance
    "Meter" -> Just $ input.distance
    _ -> Just $ 1000.0 * input.distance
  date <- fromString input.date
  return $ Activity { date: date, distance: dist, type: Run }

parseRA (Right res) = mapMaybe mkActRA <$> parseRA' res
  where
    parseRA' :: forall a. Array a -> D3Eff (Array RALine)
    parseRA' res = traverse coerceDatum res

getRAfromText :: String -> D3Eff (Array Activity)
getRAfromText str = parseRA $ Right $ parseTsv str
