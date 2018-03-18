module Types where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign, read')

--type VideoId = String
newtype VideoId = VideoId String
derive newtype instance rfVideoId :: ReadForeign VideoId
derive newtype instance wfVideoId :: WriteForeign VideoId
derive instance genericVideoId :: Generic VideoId _
derive instance eqVideoId :: Eq VideoId
instance showVideoId :: Show VideoId where
  show = genericShow

--type Duration = Number
newtype Duration = Duration Number
derive newtype instance rfDuration :: ReadForeign Duration
derive newtype instance wfDuration :: WriteForeign Duration
derive instance genericDuration :: Generic Duration _
instance showDuration :: Show Duration where
  show = genericShow

type Video =
  { id :: VideoId
  , duration :: Duration
  }

type Queue = Array Video

type AppState =
  { queue :: Queue
  , current :: Number
  , isPlaying :: Boolean
  , shouldPlay :: Boolean
  }

type PlayPauseMessage =
  { play :: Boolean }

type SkipMessage =
  { skip :: Number }

type SeekMessage =
  { seek :: Number }

type EnqueueMessage =
  { enqueue :: Video }

type DequeueMessage =
  { dequeue :: VideoId }

data Message
  = PlayPause    PlayPauseMessage
  | Skip         SkipMessage
  | Seek         SeekMessage
  | Enqueue      EnqueueMessage
  | Dequeue      DequeueMessage
instance readForeignMessage :: ReadForeign Message where
  readImpl f = PlayPause <$> read' f
    <|> Skip <$> read' f
    <|> Seek <$> read' f
    <|> Enqueue <$> read' f
    <|> Dequeue <$> read' f

-- derive instance genericMessage :: Generic Message _
-- instance showMessage :: Show Message where
--   show = genericShow
