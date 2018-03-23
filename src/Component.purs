module Component where

import Prelude
import Types

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import DOM.Event.KeyboardEvent (code)
import DOM.HTML.HTMLTrackElement (label)
import DOM.Node.Document (doctype)
import Data.Array (head)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Global.Unsafe (unsafeStringify)
import Halogen (liftAff, liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Simple.JSON (readJSON)
import Unsafe.Coerce (unsafeCoerce)
import YouTube as YT

data Query a
  = HandleInput String a
  | PerformSearch a
  | PlayPauseButton a
  | PrevButton a
  | NextButton a
  | IncomingSockMsg Message a
  | EnqueueSearchResult Video a
  | Init a

type FrontendState =
  { searchLoading :: Boolean
  , searchInput :: String
  , searchResults :: Maybe SearchResults
  , loadedVideoId :: Maybe VideoId
  , player :: Maybe YT.Player
  , app :: AppState
  }

type YouTubeItem =
  { id :: { videoId :: String }
    , snippet ::
      { channelTitle :: String
      , description :: String
      , thumbnails :: { high :: { url :: String } }
      , title :: String
      }
  }

type YouTubeResponse =
  { items :: Array YouTubeItem }

type SearchResults = Array Video

component :: H.Component HH.HTML Query Unit Message (Aff _)
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: FrontendState
  initialState =
    { searchLoading: false
    , searchInput: ""
    , searchResults: Nothing
    , loadedVideoId: Nothing
    , player: Nothing
    , app: initState
    }

  render :: FrontendState -> H.ComponentHTML Query
  render state =
    HH.div_
      -- [ HH.iframe [ HP.src "https://www.youtube.com/embed?enablejsapi=1&controls=0&showinfo=0" ]
      -- [ HH.div [ HP.id_ "player" ] []
      [ HH.iframe [ HP.id_ "player", HP.src "https://www.youtube.com/embed?enablejsapi=1&controls=0&showinfo=0" ]
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
      , HH.div_ [ HH.text "Queue:" ]
      , HH.ol_ $ map (\video -> HH.li_ [ HH.text video.title ]) state.app.queue
      , HH.input
        [ HP.value state.searchInput
        , HE.onValueInput (HE.input HandleInput)
        , HE.onKeyDown \e -> case code e of
          "Enter" -> Just (H.action PerformSearch)
          _       -> Nothing
        --, HP (HE.input_ PerformSearch)
        ]
      , HH.button
        [ HE.onClick (HE.input_ PerformSearch) ]
        [ HH.text "Search" ]
      , HH.div_
        [ HH.text "Search results:" ]
      , HH.div_ case state.searchResults of
        Nothing -> []
        Just results ->
          [ HH.h2_ [ HH.text "Response:" ]
          , HH.div_ $ map renderResult results
          ]
      ]

  renderResult :: Video -> _ -- halp
  renderResult result =
    HH.a
      [ HE.onClick $ HE.input_ $ EnqueueSearchResult result ]
      [ HH.img
        [ HP.src result.thumbnail ]
      , HH.div_
        [ HH.b_ [ HH.text result.title ] ]
      , HH.div_
        [ HH.text result.description ]
      ]

  updatePlayerState :: FrontendState -> FrontendState -> YT.Player -> Aff _ Unit
  updatePlayerState old new player = do
    let (loadedVideo :: Maybe VideoId) = old.loadedVideoId
    let (nextVideo :: Maybe VideoId) = _.id <$> head new.app.queue

    log $ unsafeCoerce loadedVideo
    log $ unsafeCoerce nextVideo
    log $ unsafeCoerce new.app.play

    liftEff $
      case new.app.play /\ nextVideo of
        true /\ id | id == loadedVideo
          -> YT.callPlayer player "playVideo" []
        true /\ Just nextVideo'
          -- TODO: update loadedVideo
          -> YT.callPlayer player "loadVideoById" [unwrap nextVideo']
        _ -> YT.callPlayer player "pauseVideo" []

    -- if new.app.play
    --   then do
    --     if loadedVideoId == new.app.
    --     liftEff $ YT.callPlayer player "playVideo" []
    --   else
    --     liftEff $ YT.callPlayer player "pauseVideo" []

  eval :: Query ~> H.ComponentDSL FrontendState Query Message (Aff _)
  -- Handle initialization
  eval (Init next) = do
    player <- liftAff $ YT.initPlayer "player"
      (\foo -> pure unit)

    --traverse_ launchAff_ $ updatePlayerState initialState initialState <$> player
    H.modify \st -> st { player = Just player }
      --(\ready -> log $ unsafeStringify ready)
      --(\player -> H.modify (\st -> st { player = Just player }))
      --(\ready -> log $ unsafeStringify ready)
      --(\ready -> log $ unsafeStringify ready)
    pure next

    where asd a = log $ unsafeStringify a

  -- Handle incroming messages on websocket
  eval (IncomingSockMsg (State {state}) next) = do
    prevState <- H.get
    let nextState = prevState { app = state }

    case nextState.player of
      Just player -> liftAff $ updatePlayerState prevState nextState player
      _           -> pure unit

    H.modify \st -> nextState


    -- let nextVideo = head nextState.queue

    -- prevVideoId <- H.gets _.loadedVideoId
    -- let nextVideoId = _.id <$> nextVideo

    -- if nextState.play
    --   then do
    --     if prevVideoId /= nextVideoId
    --       then do
    --         liftEff $ YT.callPlayer "loadVideoById" [maybe "" (\id -> unwrap id) nextVideoId, "0", "large"]
    --         H.modify \st -> st { loadedVideoId = nextVideoId }
    --       else pure unit
    --
    --     --player <- H.gets _.player
    --     --YT.callPlayer <$> player <*> Just "playVideo" <*> Just []
    --   --else liftEff $ YT.callPlayer "pauseVideo" []
    --   else pure unit
    --
    --liftAff $ log $ "new state: " <> unsafeStringify nextState

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
    res <- H.liftAff $ AX.get ("https://www.googleapis.com/youtube/v3/search?q=" <> query <> "&type=video&part=snippet&key=AIzaSyBi8SM9GfJyr_xOY38ec2EJ4Y6w6-xVjdo")

    let searchResults = parseSearchResults res.response
    H.modify (_ { searchLoading = false, searchResults = searchResults })
    pure next
  -- Handles search input field changes
  eval (HandleInput searchInput next) = do
    H.modify (\state -> state { searchInput = searchInput })
    pure next

  eval (EnqueueSearchResult result next) = do
    H.raise $ Enqueue { enqueue: result }
    H.modify (\state -> state { searchResults = Nothing })
    pure next

parseSearchResults
  :: String
  -> Maybe SearchResults
parseSearchResults res = do
  let (decoded :: Maybe YouTubeResponse) = hush $ readJSON res

  map ytToSearchResult <$> _.items <$> decoded

  where
    ytToSearchResult :: YouTubeItem -> Video
    ytToSearchResult = \i ->
      { id: VideoId i.id.videoId
      , title: i.snippet.title
      , channel: i.snippet.channelTitle
      , thumbnail: i.snippet.thumbnails.high.url
      , description: i.snippet.description
      }
