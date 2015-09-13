module CalendarChart.Activities where

import Prelude

import Data.Date
import Data.Time
import Data.Date.UTC
import Data.Enum
import Data.Map
import Data.Tuple hiding (lookup)
import Data.Maybe
import qualified Data.Array as Arr
import Data.Foldable(foldl)
import Math
import qualified Data.Int as I
import Data.JSON
import qualified Data.List as L

import CalendarChart.Util

data Type = Run | Bike | Other String

instance typeEq :: Eq Type where
  eq Run Run = true
  eq Bike Bike = true
  eq (Other x) (Other y) = x == y
  eq _ _ = false


type ActivityR = { date :: Date, distance :: Number, type :: Type }
data Activity = Activity ActivityR

instance showActivity :: Show Activity where
  show (Activity { date = d, distance = n, type = t }) = show d ++ ": " ++ show n

instance activityToJSON :: ToJSON Activity where
  toJSON (Activity { date = dt, distance = dist, type = t }) =
    object [ "date" .= toJSONdate dt, "dist" .= dist, "ty" .= t ]

instance activityFromJSON :: FromJSON Activity where
  parseJSON (JObject o) = do
    dateS <- o .: "date"
    date <- parseJSONdate dateS
    dist <- o .: "dist"
    ty <- o .: "ty"
    return $ Activity { date: date, distance: dist, type: ty }

instance activityEq :: Eq Activity where
  eq (Activity {date: d, distance: dist, type: t}) (Activity {date: d', distance: dist', type: t'}) =
    d == d' && dist == dist' && t == t'

instance typeToJSON :: ToJSON Type where
  toJSON Run = JString "run"
  toJSON Bike = JString "bike"
  toJSON (Other x) = JString x

instance typeFromJSON :: FromJSON Type where
  parseJSON (JString "run") = pure Run
  parseJSON (JString "bike") = pure Bike
  parseJSON (JString x) = pure $ Other x

-- instance dateToJSON :: ToJSON Date where
toJSONdate :: Date -> JValue
toJSONdate date = JString $ dateToISOString $ toJSDate date

-- instance dateFromJSON :: FromJSON Date where
parseJSONdate :: String -> JParser Date
parseJSONdate ds  =
  case fromString ds of
    Just d -> return d
    Nothing -> fail "Not date"

--   parseJSON (JString ds) =
--     case fromString ds of
--       Just d -> return d
--       Nothing -> fail "Not date"
--   parseJSON _ = fail "Not string"

data ActivityDetail = RunningAhead | StravaFile | StravaLink
data Activities = Activities ActivityDetail (Array Activity)


instance activityDetailEq :: Eq ActivityDetail where
  eq RunningAhead RunningAhead = true
  eq StravaFile StravaFile = true
  eq StravaLink StravaLink = true
  eq _ _ = false

instance activitiesEq :: Eq Activities where
  eq (Activities d acts) (Activities d' acts') = d == d' && acts == acts'

instance activityDetailToJSON :: ToJSON ActivityDetail where
  toJSON RunningAhead = JString "ra"
  toJSON StravaFile = JString "stravafile"
  toJSON StravaLink = JString "strava"

instance activityDetailFromJSON :: FromJSON ActivityDetail where
  parseJSON (JString "ra") = pure RunningAhead
  parseJSON (JString "stravafile") = pure StravaFile
  parseJSON (JString "strava") = pure StravaLink
  parseJSON (JString _) = fail "Unknown detail name"
  parseJSON _ = fail "Not string"

instance activitiesToJSON :: ToJSON Activities where
  toJSON (Activities detail acts) = object [ "detail" .= detail, "acts" .= acts ]

instance activitiesFromJSON :: FromJSON Activities where
  parseJSON (JObject o) = do
    detail <- o .: "detail"
    acts <- o .: "acts"
    return $ Activities detail acts
  parseJSON _ = fail "Not object"

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

combineA :: Array Activity -> Maybe Activity
combineA arr = c $ Arr.uncons arr
  where
    c (Just { head: (Activity h), tail: t}) = foldl combine1 (Activity <$> dayAct h) t
    c _ = Nothing

buildMap :: Array Activity -> Map Date Number
buildMap acts =
  let daily = Arr.groupBy (\(Activity a) (Activity b) -> sameDay a b) acts
      g = Arr.mapMaybe combineA daily
  in fromList $ L.toList $ map actToTuple $ g


dayNumber :: DayOfWeek -> Number
dayNumber d = (I.toNumber $ fromEnum d + 6) % 7.0

dayval :: Date -> Map Date Number -> Number
dayval k input =
  let res = fromMaybe 0.0 $ lookup k input
  in res / 1000.0

addDays d n =
  let milsPerDay = 1000.0 * 60.0 * 60.0 * 24.0
      addDay (Milliseconds ms) = Milliseconds (ms + (milsPerDay * I.toNumber n))
  in fromEpochMilliseconds <$> addDay $ toEpochMilliseconds d

weekFormat = formatDate "%W"
fullDateFormat = formatDate "%Y-%m-%d"
isoDateFormat = formatDate "%Y-%m-%dT%H:%M:%SZ"
week d = parseInt $ weekFormat $ toJSDate d

mergeActs :: Activities -> Array Activities -> Array Activities
mergeActs (newActs@(Activities d acts)) aa =
  case Arr.findIndex (\(Activities d' _) -> d == d') aa of
    Just i -> fromMaybe [] (arr i)
    Nothing -> Arr.cons newActs aa
  where
    arr :: Int -> Maybe (Array Activities)
    arr i = ((Arr.modifyAt i update aa) :: Maybe (Array Activities))
    update (Activities _ acts') = Activities d (acts++acts')

allActivities :: Array Activities -> Array Activity
allActivities acts =
  Arr.concat $ toActivities <$> acts
  where
    toActivities (Activities _ acts) = acts
