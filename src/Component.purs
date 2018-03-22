module Component where

import Prelude
import Types

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Array (head)
import Data.Array as A
import Data.Maybe (Maybe(..), maybe)
import Global.Unsafe (unsafeStringify)
import Halogen (liftAff, liftEff)
import Halogen as H
import Halogen.HTML (p)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import YouTube as YT

data Query a
  = HandleInput String a
  | PerformSearch a
  | PlayPauseButton a
  | PrevButton a
  | NextButton a
  | IncomingSockMsg Message a

type FrontendState =
  { searchLoading :: Boolean
  , searchInput :: String
  , searchResults :: Maybe String
  , app :: AppState
  }

component :: H.Component HH.HTML Query Unit Message (Aff _)
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: FrontendState
  initialState =
    { searchLoading: false
    , searchInput: ""
    , searchResults: Nothing
    , app: initState
    }

  render :: FrontendState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.iframe
        [ HP.src $ "https://www.youtube.com/embed?enablejsapi=1" ]
      , HH.div_
        [ HH.button
          [ HE.onClick (HE.input_ PrevButton) ]
          [ HH.text "Prev" ]
        , HH.button
          [ HE.onClick (HE.input_ PlayPauseButton) ]
          [ HH.text "Play/Pause" ]
        , HH.button
          [ HE.onClick (HE.input_ NextButton) ]
          [ HH.text "Next" ]
        ]
      , HH.input
          [ HP.value state.searchInput
          , HE.onValueInput (HE.input HandleInput)
          ]
      , HH.button
          [ HE.onClick (HE.input_ PerformSearch) ]
          [ HH.text "Search" ]
      , HH.div_
         case state.searchResults of
           Nothing -> []
           Just res ->
             [ HH.h2_
                 [ HH.text "Response:" ]
             , HH.pre_
                 [ HH.code_ [ HH.text res ] ]
             ]
      ]

  eval :: Query ~> H.ComponentDSL FrontendState Query Message (Aff _)
  -- Handle incroming messages on websocket
  eval (IncomingSockMsg (State stateMsg) next) = do
    prevState <- H.gets _.app
    let nextState = stateMsg.state
    H.modify \st -> st { app = nextState }

    if nextState.play
      then liftEff $ YT.callPlayer "playVideo" []
      else liftEff $ YT.callPlayer "pauseVideo" []

    let prevVideo = head prevState.queue
    let nextVideo = head nextState.queue

    let prevVideoId = maybe (VideoId "") _.id prevVideo
    let nextVideoId = maybe (VideoId "") _.id nextVideo

    if prevVideoId /= nextVideoId
      then liftEff $ YT.callPlayer "loadVideoById" [show nextVideoId]
      else pure unit

    liftAff $ log $ "new state: " <> unsafeStringify nextState

    pure next
  -- Ignore any other messages as they are client -> server
  eval (IncomingSockMsg m next) = do
    liftAff $ log $ "ignoring message: " <> unsafeStringify m
    pure next

  -- Handles play/pause button press
  eval (PlayPauseButton next) = do
    play <- H.gets _.app.play
    H.raise $ PlayPause { play: not play }
    pure next
  -- Handles prev button press
  eval (PrevButton next) = do
    H.raise $ Skip { skip: -1 }
    pure next
  -- Handles next button press
  eval (NextButton next) = do
    H.raise $ Skip { skip: 1 }
    pure next
  -- Handles search button press
  eval (PerformSearch next) = do
    query <- H.gets _.searchInput
    H.modify (_ { searchLoading = true })
    response <- H.liftAff $ AX.get ("https://www.googleapis.com/youtube/v3/search?q=" <> query <> "&part=snippet&key=AIzaSyBi8SM9GfJyr_xOY38ec2EJ4Y6w6-xVjdo")
    H.modify (_ { searchLoading = false, searchResults = Just response.response })
    pure next
  -- Handles search input field changes
  eval (HandleInput searchInput next) = do
    H.modify (\state -> state { searchInput = searchInput })
    pure next
