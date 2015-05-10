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

dayOf d = date (year d) (month d) (dayOfMonth d)
actToTuple (Activity { date: d, distance: n }) = Tuple d n
sameDay { date: d1 } { date: d2 } = dayOf d1 == dayOf d2
dayAct :: ActivityR -> Maybe ActivityR
dayAct a = a { date = _ } <$> dayOf (a.date)

combine :: Maybe Activity -> Activity -> Maybe Activity
combine a1 (Activity { date: d2, distance: n2, type: t }) =
  do
    Activity a <- a1
    d <- dayOf (a.date)
    return $ Activity $ a { distance = (a.distance) + n2 }

combineA :: [Activity] -> Maybe Activity
combineA ((Activity h):t) = foldlArray combine (Activity <$> dayAct h) t
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
week d = parseInt $ weekFormat $ toJSDate d
