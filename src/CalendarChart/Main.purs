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
import DOM.File.Types
import DOM.Node.Types
import DOM.HTML.Types
import DOM.Node.Document
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)

import Halogen
import Halogen.Component
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Query.StateF as S
import Data.Functor (($>))
import Control.Plus (empty)
import qualified Data.Int as I

import Control.Monad.Free
import Data.Coyoneda

import Control.Monad.Rec.Class (MonadRec)

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

dataActivities :: forall i. String -> H.Prop i
dataActivities s = H.prop (H.propName "alt") (Just $ H.attrName "alt") s

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

instance stateToJSON :: ToJSON State where
  toJSON (State { data = d, years = y, lastPull = lastPull }) =
    object [ "data" .= d, "years" .= y, "lastPull" .= lastPull ]

instance stateFromJSON :: FromJSON State where
  parseJSON (JObject o) = do
    d <- o .: "data"
    y <- o .: "years"
    lp <- o .: "lastPull"
    return $ State { data: d, years: y, lastPull : lp }
  parseJSON _ = fail "Not object"

-- data Input a = RaData (Array Activity) a
--   | StravaFileData (Array Activity) a
--   | Years Int a
--   | StravaDownloadData (Array Activity) Date a
--   | SavedState State a

data Input a =
  DownloadStrava a
  | RaFileInput a
  | StravaFileInput a
  | StravaFileInputControl HTMLElement a
  | RaFileInputControl HTMLElement a

  | Years Int a
  -- Driver input:
  | StravaDownloadData (Array Activity) Date a
  | SavedState State a

type AppEffects = HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect, console :: CONSOLE)

ui :: forall p. Component State Input (Aff AppEffects) p
-- ui :: State -> Component (E.Event (HalogenEffects _)) Input Input
-- ui initialState = render <$> stateful initialState updateState
ui = component render eval
  where
  render :: Render State _ p
  render (s@(State { data = activities, years = years})) = H.div_ [
     H.div [ A.classes [ H.className "header" ] ] [
          H.input [ A.type_ "file", A.id_ "rafileinput"
            , A.onChange $ A.input (\e -> RaFileInputControl e.target) ]

          , H.input [ A.type_ "file", A.id_ "stravafileinput"
            , A.onChange $ A.input (\e -> StravaFileInputControl e.target) ]

        , H.span [ A.classes [H.className "title"] ] [ H.text "Run Calendar" ]
        , H.span [
            A.onClick (A.input_ RaFileInput)
          ] [ H.text "RA" ]
        , H.span [
            A.onClick (A.input_ StravaFileInput)
          , A.classes [H.className "strava"] ] [ H.img [ A.src "strava.svg"],  H.text "Strava/local" ]
        , H.span [
            A.classes [ H.className "strava"]
          , A.onClick (A.input_ DownloadStrava)
          ] [
            H.img [ A.src "strava.svg" ]
          , H.text "Connect to Strava"
          ]
        , H.text "Years:"
        , H.span_ (yearLink years <$> 1..5 )
        , H.text $ "Data Sources: " ++  show (length activities)
      ]
    , H.div [ dataActivities (encode s), H.prop (H.propName "title") (Just $ H.attrName "title") (encode s),  A.classes [H.className "monthchart", H.className "hcl2"] ] []
    ]

  yearLink :: Int -> Int -> H.HTML p (Input Unit)
  yearLink years y =
    if y == years then
      H.span [ A.classes [ H.className "year", H.className "selected" ] ] [ H.text $ show y ]
    else
      H.span [
        A.onClick (A.input_ (Years y))
      , A.classes [ H.className "year" ] ] [ H.text $ show y ]

  eval :: Eval Input State Input (Aff AppEffects)
  eval (DownloadStrava next) = S.modify (\s -> (s :: State)) $> next

  eval (StravaFileInput next) = liftFI $ clickFileInput "#stravafileinput" $> next
  eval (RaFileInput next) = liftFI $ clickFileInput "#rafileinput" $> next

  eval (RaFileInputControl e next) = do
    text <- liftFI $ getFile'' e
    liftFI (logA "Got RA data")
    acts <- liftFI $ ((liftEff $ getRAfromText text) :: Aff AppEffects (Array Activity))
    S.modify (\(State x@{data=s}) ->
      State $ x { data = mergeActs (Activities RunningAhead acts) s }
    )
    pure next

  eval (StravaFileInputControl e next) = do
    text <- liftFI $ getFile'' e
    liftFI (logA "Got Strava data")
    let acts = getStravaFromText text
    S.modify (\(State x@{data=s}) ->
      State $ x { data = mergeActs (Activities StravaFile acts) s }
    )
    pure next

  eval (Years n next) = S.modify (\(State x) -> State $ x { years = n}) $> next
  eval (StravaDownloadData acts dt next) =  S.modify (\(State x@{data=s}) ->
    State $ x { data = mergeActs (Activities StravaLink acts) s, lastPull = Just dt }
    ) $> next
  eval (SavedState ss next) = S.modify (\_ -> ss) $> next

--   updateState :: State -> Input -> State
--   updateState (State rec @ { data = s }) (RaData a) =
--     State rec { data = mergeActs (Activities RunningAhead a) s }
--   updateState (State rec @ { data = s }) (StravaFileData a) =
--     State rec { data = mergeActs (Activities StravaFile a) s }
--   updateState (State rec @ { data = s }) (StravaDownloadData a d) =
--     State rec { data = mergeActs (Activities StravaLink a) s, lastPull = Just d }
--   updateState (State s) (Years n) = State $ s { years = n }
--   updateState (State s) (SavedState ss) = ss
--   updateState (ss) _ = ss

clickFileInput :: String -> Aff AppEffects Unit
clickFileInput selector =
  liftEff $ do
      w <- DOM.HTML.window
      doc <- DOM.HTML.Window.document w
      fileInput <- DOM.Node.ParentNode.querySelector selector $ documentToParentNode $ htmlDocumentToDocument doc
      case Data.Nullable.toMaybe fileInput of
        Just i -> return unit -- TODO -- click i
        Nothing -> return unit

getFile'' :: HTMLElement -> Aff AppEffects String
getFile'' e =
  case getElementFile e of
    Nothing -> return ""
    Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)

logA :: String -> Aff AppEffects Unit
logA s = liftEff $ log s

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

updateChart :: HTMLElement -> Eff (HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect, console :: CONSOLE)) Unit
updateChart elt = do
  divElts <- querySelector ".monthchart" $ elementToParentNode $ htmlElementToElement $ elt
  return unit
  -- for_ divElts \divElt -> do
  --   dat <- getAttribute "data-activities" $ divElt
  --   liftEff $ WS.setItem WS.localStorage savedStateKey dat
  --   case decode dat of
  --     Just (State { data = acts, years = y}) -> do
  --       chart y $ allActivities acts
  --     Nothing -> log "No data?!"

isOld :: Maybe Date -> Date -> Boolean
isOld Nothing _ = true
isOld (Just old) current =
  diff > Hours 1.0
  where
    t1 = toEpochMilliseconds old
    t2 = toEpochMilliseconds current
    diff = toHours $ t2 - t1

mainInteractive :: Aff AppEffects Unit
mainInteractive =  do
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey
  savedState <- liftEff $ WS.getItem WS.localStorage savedStateKey
  --
  { node: node, driver: driver } <- runUI ui defaultState -- \req elt driver -> updateChart elt
  liftEff $ (Halogen.Util.appendToBody node :: Eff AppEffects Unit)

  let initialState = fromMaybe defaultState $ savedState >>= decode
  driver (action $ SavedState initialState)
  liftEff $ updateChart node
  current <- liftEff now
  case {s: initialState, t: cachedToken} of
    { s: State { lastPull: lp }, t: Just token } | isOld lp current -> do
      recentActs <- downloadedStrava token
      driver (action $ StravaDownloadData recentActs current)
    _ -> return unit

  return unit

main :: Eff AppEffects Unit
main = launchAff $ mainInteractive
