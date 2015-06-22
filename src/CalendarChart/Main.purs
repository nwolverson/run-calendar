module CalendarChart.Main where

import Data.Maybe

import Data.JSON

import CalendarChart.Chart
import CalendarChart.Activities
import CalendarChart.Strava
import CalendarChart.RA
import CalendarChart.Util

import Data.Array(map,mapMaybe,length,filter,concat,(..))
import Debug.Trace
import Data.Date
import Data.Tuple

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(launchAff,Aff())
import Network.HTTP.Affjax

import qualified Browser.WebStorage as WS

import Data.Either
import Data.Foreign(readString)
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
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Events.Monad as E
import Control.Functor (($>))
import Control.Plus (empty)


fetchCont :: ([Activity] -> Eff _ (Unit)) -> Aff _ Unit
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

mainMonths = launchAff (fetchCont (chartMonths 1))

chart y = chartMonths y <<< filter (\(Activity a) -> a.type == Run)

stravaTokenKey = "stravaToken"

downloadStrava :: forall e. Unit -> Aff (trace :: Trace | e) [Activity]
downloadStrava _ = do
  let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey

  case cachedToken of
    Nothing -> do
      liftEff $ trace "Downloading Strava"
      let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
      liftEff $ openWindow stravaUrl "login" "height=600,width=800"
      token <- externalCallAff "downloadedStrava"
      liftEff $ trace $ "Got Strava token: " ++ token
      downloadedStrava token
    Just token -> do
      liftEff $ trace "Got cached token"
      downloadedStrava token

downloadedStrava :: forall e. String -> Aff (trace :: Trace | e) [Activity]
downloadedStrava token = do
  liftEff $ trace $ "Strava callback complete: " ++ token
  liftEff $ WS.setItem WS.localStorage stravaTokenKey token
  fetchStrava 1 token

fetchStrava :: forall e. Number -> String -> Aff (trace :: Trace | e) [Activity]
fetchStrava page token = do
  liftEff $ trace "About to fetch strava data"
  let url = "https://www.strava.com/api/v3/athlete/activities?per_page=200" ++ "&page=" ++ (show page) ++ "&access_token=" ++ token ++ "&callback={callback}"
  text <- jsonpAff url
  return $ getStravaFromText text

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
data State = State { data:: [[Activity]], years:: Number }

-- | Inputs to the state machine
data StateInput = CurrentState State | Input Input
data Input = Data [Activity] | Years Number

ui :: Component (E.Event (HalogenEffects _)) StateInput StateInput
ui = render <$> stateful (State { data: [], years: 1 }) update
  where
  render :: State -> H.HTML (E.Event (HalogenEffects _) StateInput)
  render s@(State { data = activities, years = years}) = H.div_
    [ H.text "Upload data (RunningAhead csv format): "
    , H.input [ A.type_ "file", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ trace "Got RA data"
        ra <- liftEff $ getRAfromText text
        stateInput s (Data ra)
      ) ] []

    , H.text "Upload data (Strava saved JSON format): "
    , H.input [ A.type_ "file", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ trace "Got Strava data"
        let sa = getStravaFromText text
        stateInput s (Data sa)
      ) ] []

    , H.button [ A.onClick $ \_ -> pure (do
        sa <- E.async $ downloadStrava unit
        stateInput s (Data sa)
      ) ] [ H.text "Connect to Strava"]


    , H.text "Show years: "
    , H.select [ A.onValueChanged $ \v -> pure $ do
          let n = parseInt $ either (\_ -> "1") id $ readString v
          stateInput s (Years n)
        ]
      ( (\y -> H.option_ [H.text $ show y]) <$> 1..5 )

    , H.p_ [ H.text $ "Files uploaded: " ++ show (length activities) ]

    , H.p_ [ H.text $ "Years to show: " ++ show (years)]

    , H.div [ A.classes [A.className "monthchart", A.className "hcl2"] ] []
    ]

  getFile e =
    case getElementFile e.target of
      Nothing -> return ""
      Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)


  stateInput :: State -> Input -> _
  stateInput s i = pure (Input i) <> pure (CurrentState $ updateState s i)

  updateState :: State -> Input -> State
  updateState (State rec @ { data = s }) (Data a) = State rec { data = (a : s) }
  updateState (State s) (Years n) = State $ s { years = n }


  update :: State -> StateInput -> State
  update s (Input i) = updateState s i
  update s (CurrentState _) = s

mainInteractive = do
  Tuple node _ <- runUIWith ui (\req elt driver -> do
    trace "runUi callback"
    case req of
      CurrentState (State { data = acts, years = y }) -> chart y $ concat acts
      _ -> return unit
    )
  appendToBody node

  return unit

main = mainInteractive
