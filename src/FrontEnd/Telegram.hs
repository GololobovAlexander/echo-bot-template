{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram where

import Data.Aeson (encode)
import Network.HTTP.Req
import Token (token)
import JSONparsing
import qualified EchoBot
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Control.Monad.IO.Class ( MonadIO(liftIO) )


newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

data State = State
  { state :: EchoBot.State,
    user  :: Integer}

run :: FrontEnd.Telegram.Handle -> IO ()
run h = runReq defaultHttpConfig $ do
  let handle = hBotHandle h 
  response <- req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "getUpdates")
                NoReqBody
                jsonResponse
                mempty
  let responseB = responseBody response :: TelegramResponse
  let txt = fromJust . incoming_text . fromJust . message . last . telegramResponseResult $ responseB 
  let ch = fromJust . cid . chat . fromJust . message . last . telegramResponseResult $ responseB
  let msg = EchoBot.hMessageFromText handle $ T.pack txt
  responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
  _ <- sendResponses responses ch
  liftIO $ print responseB

key1 :: Button
key1 = Button {text = "1", callback_data = "1"}

key2 :: Button
key2 = Button {text = "2", callback_data = "2"}

key3 :: Button
key3 = Button {text = "3", callback_data = "3"}

key4 :: Button
key4 = Button {text = "4", callback_data = "4"}

key5 :: Button
key5 = Button {text = "5", callback_data = "5"}


keyboard :: Inline
keyboard = Inline {inline_keyboard = [[key1, key2, key3, key4, key5]]}

keyboardEncoded :: String
keyboardEncoded = read . show $ encode keyboard

sendMessage :: (MonadHttp m, Show a) => a -> String -> m IgnoreResponse
sendMessage ch txt = req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "sendMessage")
                NoReqBody
                ignoreResponse
                (mconcat $ fmap (uncurry (=:)) [("chat_id", show ch), ("text", txt)])

sendKeyboard :: (MonadHttp m, Show a) => a -> String -> m IgnoreResponse
sendKeyboard ch txt = req 
              GET
              (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "sendMessage")
              NoReqBody
              ignoreResponse
              (mconcat $ fmap (uncurry (=:)) [("chat_id", show ch), ("text", txt), ("reply_markup", keyboardEncoded)]) -- !!

sendResponses :: (MonadHttp m, Show t) => [EchoBot.Response T.Text] -> t -> m IgnoreResponse
sendResponses [] ch = sendMessage ch "Please enter command or send a message"
sendResponses (EchoBot.MessageResponse x : xs) ch = sendMessage ch (T.unpack x) >> sendResponses xs ch
sendResponses ((EchoBot.MenuResponse _ _) : _) ch = sendKeyboard ch "Please choose the repetition count"
