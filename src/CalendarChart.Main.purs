module CalendarChart.Main where

import Data.Maybe

import Data.JSON
--import Network.XHR
--import Control.Monad.Cont.Trans
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
import qualified Browser.WebStorage as WS

import Network.HTTP.Affjax
import Control.Monad.Aff(launchAff)

chartMonths :: [ Activity ] -> D3Eff (Unit)
chartMonths x = void $ monthCharts (buildMap x) 2014 2

chartWeek :: Date -> [ Activity ] -> D3Eff (Unit)
chartWeek date x = void $ chartDays (buildMap x) date 7

getAjax fileName = ContT $ \cb ->
  void $ get defaultAjaxOptions
  { onReadyStateChange = onSuccess $ \response ->
    getResponseText response >>= cb } fileName {}

fetchCont chartf = do
  strava <- get "data/activities.json"
  let vals = fromMaybe [] $ decode strava
  ra <- get "data/log.txt"
  pp <- liftEff $ getRAfromText ra
  let acts = filter (\(Activity a) -> a.type == Run) $ vals ++ pp
  lift $ chartf $ acts
  return $ callPhantom false

mainWeek1 jsd = do
  let dt = Data.Maybe.Unsafe.fromJust $ fromJSDate jsd
  fetchCont $ chartWeek dt
  --lift $ void $ trace "hello"

mainWeek jsd = launchAff (mainWeek1 jsd)
--mainWeekW jsd = mainWeek jsd (trace "goodbye")

mainMonths = launchAff (fetchCont chartMonths)

chart = chartMonths <<< filter (\(Activity a) -> a.type == Run)

changeCallback :: Selection _ -> (String -> Eff _ _) -> _ -> Eff (trace :: Trace | _) Unit
changeCallback rs cb i = do
  let fr = fileReader unit
  case getFile rs of
    Nothing -> cb ""
    Just f -> readAsText fr (fileAsBlob f) cb


stravaTokenKey = "stravaToken"

downloadStrava :: _ -> Eff _ Unit
downloadStrava _ = do
  let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
  cachedToken <- WS.getItem WS.localStorage stravaTokenKey
  case cachedToken of
    Nothing -> do
      openWindow stravaUrl "login" "height=600,width=800"
      trace "Downloading Strava"
    Just token -> do
      trace "Got cached token"
      downloadedStrava token
      trace "xxx"

-- called externally
downloadedStrava :: String -> Eff _ Unit
downloadedStrava token = do
  trace $ "Strava callback complete: " ++ token
  WS.setItem WS.localStorage stravaTokenKey token
  fetchStrava 1 token
  trace "42"

fetchStrava :: Number -> String -> Eff _ Unit
fetchStrava page token = do
  trace "About to fetch strava data"
  let url = "https://www.strava.com/api/v3/athlete/activities?per_page=200" ++ "&page=" ++ (show page) ++ "&access_token=" ++ token ++ "&callback={callback}"

  jsonp url (\result -> do
    trace "fetched strava data"
    let stravaData = getStravaFromText result
    cb <- combinedCallback stravaData
    cb unit
  )

combinedCallback extra = do
  rs <- rootSelect "input#upload_ra"
  ss <- rootSelect "input#upload_strava"
  return $ changeCallback ss (\str ->
    changeCallback rs (\str2 ->
      do
        ra <- getRAfromText str2
        let sa = getStravaFromText str
        chart $ ra++sa++extra) unit)

mainInteractive = do
  rs <- rootSelect "input#upload_ra"
  ss <- rootSelect "input#upload_strava"
  scs <- rootSelect "input#connect_strava"

  cb <- combinedCallback []

  rs ... onChange cb
  ss ... onChange cb
  scs ... onClick' downloadStrava

  return rs

main = mainMonths
