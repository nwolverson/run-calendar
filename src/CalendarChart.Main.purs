module CalendarChart.Main where

import Data.Maybe

import Data.JSON
import Network.XHR
import Control.Monad.Cont.Trans
import Control.Monad.Trans

import Graphics.D3.Base
import Graphics.D3.Util
import Graphics.D3.Selection

import CalendarChart.Chart
import CalendarChart.Activities
import CalendarChart.Strava
import CalendarChart.RA
import CalendarChart.Util

import Data.Array(map,mapMaybe,length,filter)
import Debug.Trace
import Data.Date

import Control.Monad.Eff

chartMonths :: [ Activity ] -> D3Eff (Unit)
chartMonths x = void $ monthCharts (buildMap x) 2014 2

chartWeek :: Date -> [ Activity ] -> D3Eff (Unit)
chartWeek date x = void $ chartDays (buildMap x) date 7

getAjax fileName = ContT $ \cb ->
  void $ get defaultAjaxOptions
  { onReadyStateChange = onSuccess $ \response ->
    getResponseText response >>= cb } fileName {}

fetchCont chartf = do
  strava <- getAjax "data/activities.json"
  let vals = fromMaybe [] $ decode strava
  ra <- getAjax "data/log.txt"
  pp <- lift $ getRAfromText ra
  let acts = filter (\(Activity a) -> a.type == Run) $ vals ++ pp
  lift $ chartf $ acts
  return $ callPhantom false

mainWeek1 jsd = do
  let dt = Data.Maybe.Unsafe.fromJust $ fromJSDate jsd
  fetchCont $ chartWeek dt
  --lift $ void $ trace "hello"

mainWeek jsd = runContT (mainWeek1 jsd) void
--mainWeekW jsd = mainWeek jsd (trace "goodbye")

mainMonths = runContT (fetchCont chartMonths) void

chart = chartMonths <<< filter (\(Activity a) -> a.type == Run)

changeCallback :: Selection _ -> (String -> Eff _ _) -> _ -> Eff (trace :: Trace | _) Unit
changeCallback rs cb i = do
  let fr = fileReader unit
  case getFile rs of
    Nothing -> cb ""
    Just f -> readAsText fr (fileAsBlob f) cb

mainInteractive = do
  rs <- rootSelect "input#upload_ra"
  ss <- rootSelect "input#upload_strava"

  let cb = changeCallback ss (\str ->
    changeCallback rs (\str2 ->
      do
        ra <- getRAfromText str2
        let sa = getStravaFromText str
        chart $ ra++sa) unit)

  rs ... onChange cb
  ss ... onChange cb

  return rs

main = mainMonths