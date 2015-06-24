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
import Data.Time
import Data.Tuple
import Data.Foldable(for_)

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
import qualified Halogen.HTML.Events.Types as ET
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
savedStateKey = "savedState"

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

dataActivities :: forall i. String -> A.Attr i
dataActivities = A.attr $ A.attributeName "data-activities"

data State = State { data:: [[Activity]], years:: Number, lastPull:: Maybe Date }

defaultState :: State
defaultState = State { data: [], years: 1, lastPull: Nothing }

instance stateToJSON :: ToJSON State where
  toJSON (State { data = d, years = y, lastPull = lastPull }) =
    object [ "data" .= d, "years" .= y, "lastPull" .= lastPull ]

instance stateFromJSON :: FromJSON State where
  parseJSON (JObject o) = do
    d <- o .: "data"
    y <- o .: "years"
    lp <- o .: "lastPull"
    return $ State { data: d, years: y, lastPull : lp }

data Input = Data [Activity] | Years Number | DownloadData [Activity] Date

ui :: State -> Component (E.Event (HalogenEffects _)) Input Input
ui initialState = render <$> stateful initialState updateState
  where
  render :: State -> H.HTML (E.Event (HalogenEffects _) Input)
  render s@(State { data = activities, years = years}) = H.div_
    [ H.input [ A.type_ "file", A.id_ "rafileinput", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ trace "Got RA data"
        ra <- liftEff $ getRAfromText text
        return $ Data ra
      ) ] []

    , H.input [ A.type_ "file", A.id_ "stravafileinput", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ trace "Got Strava data"
        let sa = getStravaFromText text
        return $ Data sa
      ) ] []

    , H.div [ A.classes [ A.className "header" ] ] [
          H.span [ A.classes [A.className "title"] ] [ H.text "Run Calendar" ]
        , H.span [ A.onClick $ clickFileInput "#rafileinput" ] [ H.text "RA" ]
        , H.span [ A.onClick $ clickFileInput "#stravafileinput", A.classes [A.className "strava"] ] [ H.img [ A.src "strava.svg"] [],  H.text "Strava/local" ]
        , H.span [ A.classes [A.className "strava"], A.onClick $ \_ -> pure (do
              sa <- E.async $ downloadStrava unit
              d <- liftEff $ now
              return $ DownloadData sa d) ]
          [
            H.img [ A.src "strava.svg" ] []
          , H.text "Connect to Strava"
          ]
        , H.text "Years:"
        , H.span_ (yearLink years <$> 1..5 )
        , H.text $ "Data Sources: " ++  show (length activities)
      ]

    , H.div [ dataActivities $ encode s, A.classes [A.className "monthchart", A.className "hcl2"] ] []
    ]

  yearLink years y =
    if y == years then
      H.span [ A.classes [ A.className "year", A.className "selected" ] ] [ H.text $ show y ]
    else
      H.span [ A.onClick $ A.input_ $ Years y, A.classes [ A.className "year" ] ] [ H.text $ show y ]

  clickFileInput :: String -> ET.Event ET.MouseEvent -> E.EventHandler _
  clickFileInput selector _ = pure $ do
      (liftEff $ do
          doc <- document globalWindow
          fileInput <- querySelector selector doc
          case fileInput of
            Just i -> click i
            Nothing -> return unit)
      empty

  getFile e =
    case getElementFile e.target of
      Nothing -> return ""
      Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)

  updateState :: State -> Input -> State
  updateState (State rec @ { data = s }) (Data a) = State rec { data = (a : s) }
  updateState (State rec @ { data = s }) (DownloadData a d) = State rec { data = (a : s), lastPull = Just d }
  updateState (State s) (Years n) = State $ s { years = n }
  updateState (ss) _ = ss

updateChart :: HTMLElement -> Eff (HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect)) Unit
updateChart elt = do
  divElts <- querySelector ".monthchart" elt
  for_ divElts \divElt -> do
    dat <- getAttribute "data-activities" $ divElt
    liftEff $ WS.setItem WS.localStorage savedStateKey dat
    case decode dat of
      Just (State { data = acts, years = y}) -> do
        chart y $ concat acts
      Nothing -> trace "No data?!"

isOld :: Maybe Date -> Date -> Boolean
isOld Nothing _ = true
isOld (Just old) current =
  diff > Hours 1
  where
    t1 = toEpochMilliseconds old
    t2 = toEpochMilliseconds current
    diff = toHours $ t2 - t1

  --old < current

mainInteractive :: Aff (HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect)) Unit
mainInteractive =  do
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey
  savedState <- liftEff $ WS.getItem WS.localStorage savedStateKey

  let initialState = fromMaybe defaultState $ savedState >>= decode
  current <- liftEff now
  state <- case initialState of
    State (s@{ lastPull: lp }) | isOld lp current -> do
      recentActs <- maybe (pure []) downloadedStrava cachedToken
      return $ State $ s { lastPull = Just current, data = recentActs : s.data }
    _ -> return initialState


  Tuple node _ <- liftEff $ runUIWith (ui state) \req elt driver -> updateChart elt
  liftEff $ appendToBody node
  liftEff $ updateChart node

  return unit

main = launchAff $ mainInteractive
