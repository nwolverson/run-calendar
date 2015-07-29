module CalendarChart.Main where

import Prelude

import Data.Maybe
import Data.JSON

import CalendarChart.Chart
import CalendarChart.Activities
import CalendarChart.Strava
import CalendarChart.RA
import CalendarChart.Util

import Data.Array(mapMaybe,length,filter,concat,(..),findIndex,modifyAt,(:))
import Data.Date
import Data.Time
import Data.Tuple
import Data.Foldable(for_)

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(launchAff,Aff())
import Network.HTTP.Affjax
import Control.Monad.Eff.Console

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
import Data.Functor (($>))
import Control.Plus (empty)
import qualified Data.Int as I

import Network.RemoteCallback

fetchCont :: (Array Activity -> Eff _ (Unit)) -> Aff _ Unit
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

downloadStrava :: forall e. Unit -> Aff (console :: CONSOLE, dom :: DOM | e) (Array Activity)
downloadStrava _ = do
  let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey

  case cachedToken of
    Nothing -> do
      liftEff $ log "Downloading Strava"
      let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
      liftEff $ openWindow stravaUrl "login" "height=600,width=800"
      token <- externalCall "downloadedStrava"
      liftEff $ log $ "Got Strava token: " ++ token
      downloadedStrava token
    Just token -> do
      liftEff $ log "Got cached token"
      downloadedStrava token

downloadedStrava :: forall e. String -> Aff (console :: CONSOLE, dom :: DOM | e) (Array Activity)
downloadedStrava token = do
  liftEff $ log $ "Strava callback complete: " ++ token
  liftEff $ WS.setItem WS.localStorage stravaTokenKey token
  fetchStrava 1 token

fetchStrava :: forall e. Int -> String -> Aff (console :: CONSOLE, dom :: DOM | e) (Array Activity)
fetchStrava page token = do
  liftEff $ log "About to fetch strava data"
  let callback = "PS_FetchStrava_Callback"
  let url = "https://www.strava.com/api/v3/athlete/activities?per_page=200" ++ "&page=" ++ (show page) ++ "&access_token=" ++ token ++ "&callback=" ++ callback
  text <- jsonp callback url
  return $ getStravaFromText text

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

dataActivities :: forall i. String -> A.Attr i
dataActivities = A.attr $ A.attributeName "data-activities"

data ActivityDetail = RunningAhead | StravaFile | StravaLink
data Activities = Activities ActivityDetail (Array Activity)

data State = State { data:: Array Activities, years:: Int, lastPull:: Maybe Date }

defaultState :: State
defaultState = State { data: [], years: 1, lastPull: Nothing }

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


instance activitiesToJSON :: ToJSON Activities where
  toJSON (Activities detail acts) = object [ "detail" .= detail, "acts" .= acts ]

instance activitiesFromJSON :: FromJSON Activities where
  parseJSON (JObject o) = do
    detail <- o .: "detail"
    acts <- o .: "acts"
    return $ Activities detail acts

instance stateToJSON :: ToJSON State where
  toJSON (State { data = d, years = y, lastPull = lastPull }) =
    object [ "data" .= d, "years" .= y, "lastPull" .= lastPull ]

instance stateFromJSON :: FromJSON State where
  parseJSON (JObject o) = do
    d <- o .: "data"
    y <- o .: "years"
    lp <- o .: "lastPull"
    return $ State { data: d, years: y, lastPull : lp }

data Input = RaData (Array Activity) | StravaFileData (Array Activity) | Years Int | StravaDownloadData (Array Activity) Date | SavedState State

ui :: State -> Component (E.Event (HalogenEffects _)) Input Input
ui initialState = render <$> stateful initialState updateState
  where
  render :: State -> H.HTML (E.Event (HalogenEffects _) Input)
  render s@(State { data = activities, years = years}) = H.div_
    [ H.input [ A.type_ "file", A.id_ "rafileinput", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ log "Got RA data"
        ra <- liftEff $ getRAfromText text
        return $ RaData ra
      ) ] []

    , H.input [ A.type_ "file", A.id_ "stravafileinput", A.onChange $ \e -> pure (do
        text <- E.async $ getFile e
        liftEff $ log "Got Strava data"
        let sa = getStravaFromText text
        return $ StravaFileData sa
      ) ] []

    , H.div [ A.classes [ A.className "header" ] ] [
          H.span [ A.classes [A.className "title"] ] [ H.text "Run Calendar" ]
        , H.span [ A.onClick $ clickFileInput "#rafileinput" ] [ H.text "RA" ]
        , H.span [ A.onClick $ clickFileInput "#stravafileinput", A.classes [A.className "strava"] ] [ H.img [ A.src "strava.svg"] [],  H.text "Strava/local" ]
        , H.span [ A.classes [A.className "strava"], A.onClick $ \_ -> pure (do
              sa <- E.async $ downloadStrava unit
              d <- liftEff $ now
              return $ StravaDownloadData sa d) ]
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
  updateState (State rec @ { data = s }) (RaData a) =
    State rec { data = mergeActs (Activities RunningAhead a) s }
  updateState (State rec @ { data = s }) (StravaFileData a) =
    State rec { data = mergeActs (Activities StravaFile a) s }
  updateState (State rec @ { data = s }) (StravaDownloadData a d) =
    State rec { data = mergeActs (Activities StravaLink a) s, lastPull = Just d }
  updateState (State s) (Years n) = State $ s { years = n }
  updateState (State s) (SavedState ss) = ss
  updateState (ss) _ = ss

mergeActs :: Activities -> Array Activities -> Array Activities
mergeActs (newActs@(Activities d acts)) aa =
  case findIndex (\(Activities d' _) -> d == d') aa of
    Just i -> fromMaybe [] (arr i)
    Nothing -> newActs : aa
  where
    arr :: Int -> Maybe (Array Activities)
    arr i = ((modifyAt i update aa) :: Maybe (Array Activities))
    update (Activities _ acts') = Activities d (acts++acts')

-- mergeActs :: Activities -> Array Activities -> Array Activities
-- mergeActs act [] = [act]
-- mergeActs (Activities d acts) (Activities d' acts' : rest) | d == d' = Activities d (acts++acts') : rest
-- mergeActs x (a : rest) = a : mergeActs x rest

allActivities :: Array Activities -> Array Activity
allActivities acts =
  concat $ toActivities <$> acts
  where
    toActivities (Activities _ acts) = acts

updateChart :: HTMLElement -> Eff (HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect)) Unit
updateChart elt = do
  divElts <- querySelector ".monthchart" elt
  for_ divElts \divElt -> do
    dat <- getAttribute "data-activities" $ divElt
    liftEff $ WS.setItem WS.localStorage savedStateKey dat
    case decode dat of
      Just (State { data = acts, years = y}) -> do
        chart y $ allActivities acts
      Nothing -> log "No data?!"

isOld :: Maybe Date -> Date -> Boolean
isOld Nothing _ = true
isOld (Just old) current =
  diff > Hours 1.0
  where
    t1 = toEpochMilliseconds old
    t2 = toEpochMilliseconds current
    diff = toHours $ t2 - t1


mainInteractive :: Aff (HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect)) Unit
mainInteractive =  do
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey
  savedState <- liftEff $ WS.getItem WS.localStorage savedStateKey

  Tuple node driver <- liftEff $ runUIWith (ui defaultState) \req elt driver -> updateChart elt
  liftEff $ appendToBody node

  --liftEff $ updateChart node

  let initialState = fromMaybe defaultState $ savedState >>= decode
  liftEff $ driver $ SavedState initialState
  current <- liftEff now
  case {s: initialState, t: cachedToken} of
    { s: State { lastPull: lp }, t: Just token } | isOld lp current -> do
      recentActs <- downloadedStrava token
      liftEff $ driver $ StravaDownloadData recentActs current
    _ -> return unit

  return unit

main = launchAff $ mainInteractive
