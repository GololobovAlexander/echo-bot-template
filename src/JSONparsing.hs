{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSONparsing where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson ( (.:), (.:?), FromJSON(parseJSON), Value(Object), ToJSON, KeyValue ((.=)), toEncoding, pairs)
import GHC.Generics ( Generic )
import Data.Text (Text)

data TelegramResponse = TelegramResponse { 
    telegramResponseOk     :: Bool, 
    telegramResponseResult :: [Result]
} deriving (Show, Generic)

instance FromJSON TelegramResponse where
  parseJSON (Object v) = TelegramResponse <$> v .: "ok" <*> v .: "result"
  parseJSON _ = mzero

data Result = Result { 
    update_id :: Integer,
    message   :: Maybe Message,
    callback_query :: Maybe CallbackQuery
} deriving (Show, Generic)

instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .: "update_id" <*> v .:? "message" <*> v .:? "callback_query"
  parseJSON _ = mzero

data Message = Message { 
    -- | 	Unique message identifier inside this chat
    message_id    :: Integer,
    -- |  Sender of the message
    from          :: From,
    -- | Sender of the message, sent on behalf of a chat.
    chat          :: Chat,
    -- | Date the message was sent in Unix time
    date          :: Integer,
    -- | Text of the message
    incoming_text :: Maybe Text,
    -- | Sticker information
    sticker       :: Maybe Sticker
} deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object v) = Message <$> v .: "message_id" <*> v .: "from" <*> v .: "chat" <*> v .: "date" <*> v .:? "text" <*> v.:? "sticker"
  parseJSON _ = mzero

data From = From {
  from_id         :: Int,
  is_bot          :: Bool,
  from_first_name :: Maybe Text,
  from_username   :: Maybe Text,
  language_code   :: Maybe Text
} deriving (Show, Generic)

instance FromJSON From where
  parseJSON (Object v) = From <$> v .: "id" <*> v .: "is_bot" <*> v .:? "first_name" <*> v .:? "username" <*> v .:? "language_code"
  parseJSON _ = mzero

data Chat = Chat {
  cid :: Maybe Integer,
  first_name :: Maybe Text,
  username   :: Maybe Text,
  chat_type  :: Maybe Text
} deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON (Object v) = Chat <$> v .:? "id" <*> v .:? "first_name" <*> v .:? "username" <*> v .:? "chat_type"
  parseJSON _ = mzero

data Button = Button {
  text :: Text,
  callback_data :: Integer
  } deriving (Show, Generic)

instance ToJSON Button where
  toEncoding (Button txt callback) =
    pairs ("text" .= txt <> "callback_data" .= callback)

newtype Inline = Inline {
  inline_keyboard :: [[Button]]
} deriving (Show, Generic)

instance ToJSON Inline where 

data CallbackQuery = CallbackQuery {
  -- | Unique identifier for this query
  query_id          :: String,
  -- | Sender
  query_from        :: From,
  -- | Optional. Message with the callback button that originated the query
  query_message     :: Maybe Message,
  -- | Optional. Identifier of the message sent via the bot in inline mode, that originated the query.
  inline_message_id :: Maybe Text,
  -- | Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent.
  chat_instance     :: Maybe Text,
  -- | Optional. Data associated with the callback button.
  query_data        :: Maybe Text,

  game_short_name   :: Maybe Text
} deriving (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON (Object v) = CallbackQuery <$> v .: "id" <*> v .: "from"           <*> v .:? "message" <*> v .:? "inline_message_id" 
                                                     <*> v .:? "chat_instance" <*> v .:? "data"    <*> v .:? "game_short_name"
  parseJSON _ = mzero 

data Sticker = Sticker {
  -- | Identifier for this file, which can be used to download or reuse the file
  file_id :: Text,
  -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  file_unique_id :: Text,
  -- | 	Sticker width
  width :: Integer,
  -- | Sticker height
  height :: Integer,
  is_animated :: Bool,
  is_video :: Bool,
  emoji :: Text,
  set_name :: Text,
  file_size :: Integer
} deriving (Show, Generic)

instance FromJSON Sticker where
  parseJSON (Object v) = Sticker <$> v .: "file_id" <*> v .: "file_unique_id" <*> v .: "width"    <*> v .: "height" 
                                                    <*> v .: "is_animated"    <*> v .: "is_video" <*> v .: "emoji" 
                                                    <*> v .: "set_name"       <*> v .: "file_size"
  parseJSON _ = mzero