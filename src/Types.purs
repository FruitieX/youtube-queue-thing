module Types where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Variant (Variant)
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignVariant, read', write, writeImpl, writeVariantImpl)
import Type.Row (class RowToList, RLProxy(..))

--type VideoId = String
newtype VideoId = VideoId String
derive newtype instance rfVideoId :: ReadForeign VideoId
derive newtype instance wfVideoId :: WriteForeign VideoId
derive instance genericVideoId :: Generic VideoId _
derive instance newtypeVideoId :: Newtype VideoId _
derive instance eqVideoId :: Eq VideoId
instance showVideoId :: Show VideoId where
  show = genericShow

-- TODO: Number or newtype?
--type Duration = Number
-- newtype Duration = Duration Number
-- derive newtype instance rfDuration :: ReadForeign Duration
-- derive newtype instance wfDuration :: WriteForeign Duration
-- derive instance genericDuration :: Generic Duration _
-- instance showDuration :: Show Duration where
--   show = genericShow

type Video =
  { id :: VideoId
  , title :: String
  , description :: String
  , channel :: String
  , thumbnail :: String
  --, duration :: Number
  }

type Queue = Array Video

type AppState =
  { queue :: Queue
  , history :: Queue
  , play :: Boolean
  , seek :: Number
  }

initState
  :: AppState
initState =
  { queue: []
  , history: []
  , play: false
  , seek: 0.0
  }

type PlayPauseMessage =
  { play :: Boolean }

type SkipMessage =
  { skip :: Int }

type SeekMessage =
  { seek :: Number }

type EnqueueMessage =
  { enqueue :: Video }

type DequeueMessage =
  { dequeue :: Int }

type StateMessage =
  { state :: AppState }

data Message
  = PlayPause    PlayPauseMessage
  | Skip         SkipMessage
  | Seek         SeekMessage
  | Enqueue      EnqueueMessage
  | Dequeue      DequeueMessage
  | State        StateMessage

instance readForeignMessage :: ReadForeign Message where
  readImpl f = PlayPause <$> read' f
    <|> Skip <$> read' f
    <|> Seek <$> read' f
    <|> Enqueue <$> read' f
    <|> Dequeue <$> read' f
    <|> State <$> read' f

instance writeForeignMessage :: WriteForeign Message where
  writeImpl (PlayPause f) = writeImpl f
  writeImpl (Skip f) = writeImpl f
  writeImpl (Seek f) = writeImpl f
  writeImpl (Enqueue f) = writeImpl f
  writeImpl (Dequeue f) = writeImpl f
  writeImpl (State f) = writeImpl f

-- derive instance genericMessage :: Generic Message _
-- instance showMessage :: Show Message where
--   show = genericShow
