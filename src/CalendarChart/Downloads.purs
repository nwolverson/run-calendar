module CalendarChart.Downloads where


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
import Network.HTTP.Affjax hiding (get)
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

stravaTokenKey = "stravaToken"

downloadStrava :: forall e. Unit -> Aff (console :: CONSOLE, dom :: DOM | e) (Array Activity)
downloadStrava _ = do
  let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey

  case cachedToken of
    Nothing -> do
      liftEff $ log "Downloading Strava"
      let stravaUrl = "https://www.strava.com/oauth/authorize?client_id=2746&response_type=code&redirect_uri=http://localhost:8123/token_exchange&scope=public&state=mystate&approval_prompt=force"
      liftEff $ openWindow stravaUrl "login" "height=600,width=800"
      ftoken <- externalCall "downloadedStrava"
      case readString ftoken of
        Right token -> do
          liftEff $ log $ "Got Strava token: " ++ token
          downloadedStrava token
        _ -> return []
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
  result <- jsonp callback url
  let text = foreignStringify result
  return $ getStravaFromText text
