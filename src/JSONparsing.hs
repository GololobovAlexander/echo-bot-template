{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JSONparsing where

import Control.Monad (MonadPlus (mzero))
import Control.Monad.IO.Class ()
import Data.Aeson ( (.:), (.:?), FromJSON(parseJSON), Value(Object) )
import GHC.Generics ( Generic )

data TelegramResponse = TelegramResponse { 
    telegramResponseOk     :: Bool, 
    telegramResponseResult :: [Result]
} deriving (Show, Generic)

instance FromJSON TelegramResponse where
  parseJSON (Object v) = TelegramResponse <$> v .: "ok" <*> v .: "result"
  parseJSON _ = mzero

data Result = Result { 
    update_id :: Integer,
    message   :: Message
} deriving (Show, Generic)

instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .: "update_id" <*> v .: "message"
  parseJSON _ = mzero

data Message = Message { 
    message_id :: Integer,
    from       :: From,
    chat       :: Chat,
    date       :: Integer,
    text       :: Maybe String
} deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object v) = Message <$> v .: "message_id" <*> v .: "from" <*> v 
                                       .: "chat" <*> v .: "date" <*> v .:? "text"
  parseJSON _ = mzero

data From = From {
  from_id         :: Integer,
  is_bot          :: Bool,
  from_first_name :: String,
  from_username   :: String,
  language_code   :: Maybe String
} deriving (Show, Generic)

instance FromJSON From where
  parseJSON (Object v) = From <$> v .: "id" <*> v .: "is_bot" <*> v .: "first_name" <*> v 
                                                  .: "username" <*> v .:? "language_code"
  parseJSON _ = mzero

data Chat = Chat {
  id :: Maybe Integer,
  first_name :: String,
  username   :: String,
  chat_type  :: Maybe String
} deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON (Object v) = Chat <$> v .:? "id" <*> v .: "first_name" <*> v .: "username" <*> v .:? "chat_type"
  parseJSON _ = mzero