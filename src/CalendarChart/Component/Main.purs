module CalendarChart.Component.Main where

import CalendarChart.Component.Types

import Prelude

import Data.Maybe
import Data.JSON
import Data.Const
import Data.Void

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


container :: forall cp co. ParentComponent State ChartState Input ChartInput (Aff AppEffects) co ChartPlaceholder cp
container = component render eval
  where
  render :: Render State Input ChartPlaceholder
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
    , H.div_ [ H.Placeholder ChartPlaceholder ]
    ]

  yearLink :: Int -> Int -> H.HTML ChartPlaceholder (Input Unit)
  yearLink years y =
    if y == years then
      H.span [ A.classes [ H.className "year", H.className "selected" ] ] [ H.text $ show y ]
    else
      H.span [
        A.onClick (A.input_ (Years y))
      , A.classes [ H.className "year" ] ] [ H.text $ show y ]

  eval :: Eval Input State Input (QueryFP State ChartState ChartInput (Aff AppEffects) co ChartPlaceholder cp)
  eval (DownloadStrava next) = S.modify (\s -> (s :: State)) $> next
  eval (StravaFileInput next) = liftQuery $ liftFI $ clickFileInput "#stravafileinput" $> next
  eval (RaFileInput next) = liftQuery $ liftFI $ clickFileInput "#rafileinput" $> next
  eval (RaFileInputControl e next) = do
    text <- liftQuery $ liftFI $ getFile'' e
    liftQuery $ liftEff' $ log "Got RA data"
    acts <- liftQuery $ liftEff' $ getRAfromText text
    S.modify (\(State x@{data=s}) ->
      State $ x { data = mergeActs (Activities RunningAhead acts) s }
    )
    ns <- S.get
    liftQuery $ query ChartPlaceholder (action (ChartInput ns))
    pure next
  eval (StravaFileInputControl e next) = do
    text <- liftQuery $ liftFI $ getFile'' e
    liftQuery $ liftEff' $ log "Got Strava data"
    let acts = getStravaFromText text
    S.modify (\(State x@{data=s}) ->
      State $ x { data = mergeActs (Activities StravaFile acts) s }
    )
    ns <- S.get
    liftQuery $ query ChartPlaceholder (action (ChartInput ns))
    pure next
  eval (StravaDownloadData acts date next) = do
    S.modify (\(State x@{data=s}) ->
      State $ x { data = mergeActs (Activities StravaLink acts) s, lastPull = Just date }
    )
    ns <- S.get
    liftQuery $ query ChartPlaceholder (action (ChartInput ns))
    pure next
  eval (Years n next) = do
    S.modify (\(State x) -> State $ x { years = n})
    state <- S.get
    liftQuery $ query ChartPlaceholder (action (ChartInput state))
    pure next
  eval (SavedState ss next) = do
    S.modify (\_ -> ss)
    liftQuery $ query ChartPlaceholder (action (ChartInput ss))
    pure next


clickFileInput :: String -> Aff AppEffects Unit
clickFileInput selector =
  liftEff $ do
      w <- DOM.HTML.window
      doc <- DOM.HTML.Window.document w
      fileInput <- DOM.Node.ParentNode.querySelector selector $ documentToParentNode $ htmlDocumentToDocument doc
      case Data.Nullable.toMaybe fileInput of
        Just i ->
          case elementToHtmlElement i of
            Just htmlElt -> click htmlElt
            _ -> pure unit
        Nothing -> return unit

getFile'' :: HTMLElement -> Aff AppEffects String
getFile'' e =
  case getElementFile e of
    Nothing -> return ""
    Just f -> readAsTextAff (fileReader unit) (fileAsBlob f)


logA :: forall eff. String -> Aff (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect, console :: CONSOLE, avar :: Control.Monad.Aff.AVar.AVAR, err :: Control.Monad.Eff.Exception.EXCEPTION, dom :: DOM.DOM | eff) Unit
logA s = liftEff $ log s
