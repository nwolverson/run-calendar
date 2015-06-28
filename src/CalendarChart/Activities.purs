module CalendarChart.Activities where

import Data.Date
import Data.Time
import Data.Date.UTC
import Data.Enum
import Data.Map
import Data.Tuple
import Data.Maybe
import qualified Data.Array as Arr
import Data.Foldable(foldlArray)

import Data.JSON

import CalendarChart.Util

data Type = Run | Bike | Other String

instance typeEq :: Eq Type where
  (==) Run Run = true
  (==) Bike Bike = true
  (==) (Other x) (Other y) = x == y
  (==) _ _ = false
  (/=) a b = not (a == b)

type ActivityR = { date :: Date, distance :: Number, type :: Type }
data Activity = Activity ActivityR

instance showActivity :: Show Activity where
  show (Activity { date = d, distance = n, type = t }) = show d ++ ": " ++ show n

instance activityToJSON :: ToJSON Activity where
  toJSON (Activity { date = dt, distance = dist, type = t }) =
    object [ "date" .= dt, "dist" .= dist, "ty" .= t ]

instance activityFromJSON :: FromJSON Activity where
  parseJSON (JObject o) = do
    date <- o .: "date"
    dist <- o .: "dist"
    ty <- o .: "ty"
    return $ Activity { date: date, distance: dist, type: ty }

instance activityEq :: Eq Activity where
  (==) (Activity {date: d, distance: dist, type: t}) (Activity {date: d', distance: dist', type: t'}) =
    d == d' && dist == dist' && t == t'
  (/=) a b = not (a == b)

instance typeToJSON :: ToJSON Type where
  toJSON Run = JString "run"
  toJSON Bike = JString "bike"
  toJSON (Other x) = JString x

instance typeFromJSON :: FromJSON Type where
  parseJSON (JString "run") = pure Run
  parseJSON (JString "bike") = pure Bike
  parseJSON (JString x) = pure $ Other x

instance dateToJSON :: ToJSON Date where
  toJSON date = JString $ dateToISOString $ toJSDate date

instance dateFromJSON :: FromJSON Date where
  parseJSON (JString ds) =
    case fromString ds of
      Just d -> return d
      Nothing -> fail "Not date"
  parseJSON _ = fail "Not string"

dayOf d = date (year d) (month d) (dayOfMonth d)
actToTuple (Activity { date: d, distance: n }) = Tuple d n
sameDay { date: d1 } { date: d2 } = dayOf d1 == dayOf d2
dayAct :: ActivityR -> Maybe ActivityR
dayAct a = a { date = _ } <$> dayOf (a.date)

combine1 :: Maybe Activity -> Activity -> Maybe Activity
combine1 a1 (Activity { date: d2, distance: n2, type: t }) =
  do
    Activity a <- a1
    d <- dayOf (a.date)
    return $ Activity $ a { distance = (a.distance) + n2 }

combineA :: [Activity] -> Maybe Activity
combineA ((Activity h):t) = foldlArray combine1 (Activity <$> dayAct h) t
combineA [] = Nothing

buildMap :: [ Activity ] -> Map Date Number
buildMap acts =
  let daily = Arr.groupBy (\(Activity a) (Activity b) -> sameDay a b) acts
      g = Arr.mapMaybe combineA daily
  in fromList $ Arr.map actToTuple $ g


dayNumber :: DayOfWeek -> Number
dayNumber d = (fromEnum d + 6) % 7

dayval :: Date -> Map Date Number -> Number
dayval k input =
  let res = fromMaybe 0 $ lookup k input
  in res / 1000

addDays d n =
  let milsPerDay = 1000 * 60 * 60 * 24
      addDay (Milliseconds ms) = Milliseconds (ms + (milsPerDay * n))
  in fromEpochMilliseconds <$> addDay $ toEpochMilliseconds d

weekFormat = formatDate "%W"
fullDateFormat = formatDate "%Y-%m-%d"
isoDateFormat = formatDate "%Y-%m-%dT%H:%M:%SZ"
week d = parseInt $ weekFormat $ toJSDate d
