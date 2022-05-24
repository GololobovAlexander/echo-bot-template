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
    message   :: Maybe Message
} deriving (Show, Generic)

instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .: "update_id" <*> v .:? "message"
  parseJSON _ = mzero

data Message = Message { 
    message_id :: Integer,
    from       :: From,
    chat       :: Chat,
    date       :: Integer,
    incoming_text       :: Maybe Text
} deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object v) = Message <$> v .: "message_id" <*> v .: "from" <*> v 
                                       .: "chat" <*> v .: "date" <*> v .:? "text"
  parseJSON _ = mzero

data From = From {
  from_id         :: Integer,
  is_bot          :: Bool,
  from_first_name :: Maybe Text,
  from_username   :: Maybe Text,
  language_code   :: Maybe Text
} deriving (Show, Generic)

instance FromJSON From where
  parseJSON (Object v) = From <$> v .: "id" <*> v .: "is_bot" <*> v .:? "first_name" <*> v 
                                                  .:? "username" <*> v .:? "language_code"
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
  callback_data :: Text
  } deriving (Show, Generic)

instance ToJSON Button where
  toEncoding (Button txt callback) =
    pairs ("text" .= txt <> "callback_data" .= callback)

newtype Inline = Inline {
  inline_keyboard :: [[Button]]
} deriving (Show, Generic)

instance ToJSON Inline where 

data CallbackQuery = CallbackQuery {
  query_id          :: Text,
  query_from        :: From,
  query_message     :: Maybe Message,
  inline_message_id :: Maybe Text,
  chat_instance     :: Maybe Text,
  query_data        :: Maybe Text,
  game_short_name   :: Maybe Text
}

instance FromJSON CallbackQuery where
  parseJSON (Object v) = CallbackQuery <$> v .: "id" <*> v .: "from" <*> v .:? "message" <*>
                                           v .:? "inline_message_id" <*> v .:? "chat_instance" <*>
                                           v .:? "data" <*> v .:? "game_short_name"
  parseJSON _ = mzero 