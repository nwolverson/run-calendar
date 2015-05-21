module CalendarChart.Main where

import Data.Maybe

import Data.JSON

import Graphics.D3.Base
import Graphics.D3.Util
import Graphics.D3.Selection

import CalendarChart.Chart
import CalendarChart.Activities
import CalendarChart.Strava
import CalendarChart.RA
import CalendarChart.Util

import Data.Array(map,mapMaybe,length,filter,concat)
import Debug.Trace
import Data.Date
import Data.Tuple

import Control.Monad.Eff

import Control.Monad.Eff.Class
import qualified Browser.WebStorage as WS

import Network.HTTP.Affjax
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(launchAff,Aff())



-- import Data.Void
import Data.Either
import Control.Bind
import DOM
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Halogen
import Halogen.Signal
import Halogen.Component
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import Control.Functor (($>))
import Control.Plus (empty)

chartMonths :: [ Activity ] -> D3Eff (Unit)
chartMonths x = void $ monthCharts (buildMap x) 2014 2

chartWeek :: Date -> [ Activity ] -> D3Eff (Unit)
chartWeek date x = void $ chartDays (buildMap x) date 7

fetchCont :: ([Activity] -> D3Eff (Unit)) -> Aff _ Unit
fetchCont chartf = do
  strava <- get "data/activities.json"
  let vals = fromMaybe [] $ decode $ strava.response
  ra <- get "data/log.txt"
  pp <- liftEff $ getRAfromText ra.response
  let acts = filter (\(Activity a) -> a.type == Run) $ vals ++ pp
  liftEff $ do
    chartf acts
    callPhantom false

mainWeek :: JSDate -> Eff _ Unit
mainWeek jsd =
  launchAff (fetchCont $ chartWeek dt)
  where
    dt :: Date
    dt = Data.Maybe.Unsafe.fromJust $ fromJSDate jsd

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
    -- cb <- combinedCallback stravaData
    -- cb unit
    return unit
  )

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
data State = State [[Activity]]

-- | Inputs to the state machine
data Input = Data [Activity] | CurrentState [[Activity]]

ui :: Component (E.Event (HalogenEffects _)) Input Input
ui = render <$> stateful (State []) update
  where
  render :: State -> H.HTML (E.Event (HalogenEffects _) Input)
  render (State s) = H.div_
    [ H.text "Upload data (RunningAhead csv format): "
    , H.input [ A.type_ "file", A.onChange $ \e -> pure (do
        text <- E.async $ case getElementFile e.target of
          Nothing -> return ""
          Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)
        liftEff $ trace "Got RA data"
        ra <- liftEff $ getRAfromText text
        pure (Data ra) <> pure (CurrentState $ ra:s)
      ) ] []
    , H.text "Upload data (Strava saved JSON format): "
    , H.input [ A.type_ "file", A.onChange $ \e -> pure (do
        text <- E.async $ case getElementFile e.target of
          Nothing -> return ""
          Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)
        liftEff $ trace "Got Strava data"
        let sa = getStravaFromText text
        pure (Data sa) <> pure (CurrentState $ sa:s)
      ) ] []

    , H.button [ A.onClick $ \_ -> pure (do
        liftEff $ downloadStrava unit
        empty
      ) ] [ H.text "Connect to Strava"]

    , H.p_ [ H.text $ "Files uploaded: " ++ show (length s) ]

    , H.div [ A.classes [A.className "monthchart", A.className "hcl2"] ] []
    ]

  update :: State -> Input -> State
  update (State s) (Data a) = State (a:s)
  update s _ = s

mainInteractive = do
  Tuple node _ <- runUIWith ui (\req elt driver -> do
    case req of
      Data _ -> return unit
      CurrentState acts -> chart $ concat acts
    )
  appendToBody node

  return unit

main = mainInteractive
