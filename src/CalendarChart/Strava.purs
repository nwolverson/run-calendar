module CalendarChart.Strava where

import Prelude

import CalendarChart.Activities

import Data.Date(Date(),fromString)
import Data.Maybe
import Data.JSON

data StravaActivity = StravaActivity Activity

instance activityFromJSON :: FromJSON StravaActivity where
  parseJSON (JObject o) = do
    startDate <- o .: "start_date"
    dist <- o .: "distance"
    ty <- o .: "type"
    let cType = case ty of
          "Run" -> Run
          "Ride" -> Bike
          x -> Other x
    return $ StravaActivity $ Activity { date: startDate, distance: dist, type: cType }
  parseJSON _ = fail "activity parse failed"

getStravaFromText :: String -> Array Activity
getStravaFromText t =
  case decode t of
    Just stravaActs -> getAct <$> stravaActs
    Nothing -> []
  where
    getAct (StravaActivity a) = a
