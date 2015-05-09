module CalendarChart.Strava where

import CalendarChart.Activities

import Data.Date(Date(),fromString)
import Data.Maybe
import Data.JSON

instance dateFromJSON :: FromJSON Date where
  parseJSON (JString ds) =
    case fromString ds of
      Just d -> return d
      Nothing -> fail "Not date"
  parseJSON _ = fail "Not string"

instance activityFromJSON :: FromJSON Activity where
  parseJSON (JObject o) = do
    startDate <- o .: "start_date"
    dist <- o .: "distance"
    ty <- o .: "type"
    let cType = case ty of
          "Run" -> Run
          "Ride" -> Bike
          x -> Other x
    return $ Activity { date: startDate, distance: dist, type: cType }
  parseJSON _ = fail "activity parse failed"

getStravaFromText :: String -> [Activity]
getStravaFromText = fromMaybe [] <<< decode
