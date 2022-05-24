{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrontEnd.Telegram where
import Control.Concurrent
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
  {
    user  :: Integer,
    last_id :: Integer,
    chat_id :: Integer} deriving (Show)

makeTempState :: State
makeTempState = State {
  user = 0,
  last_id = 0,
  chat_id = 0
}

makeRequest :: Req (JsonResponse TelegramResponse)
makeRequest = req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "getUpdates")
                NoReqBody
                jsonResponse
                mempty

newState :: State
newState = makeTempState

modState :: Integer -> State -> IO ()
modState n state = print $ state {last_id = n}

run :: FrontEnd.Telegram.Handle -> IO ()
run h = sup h 0

getMessage :: TelegramResponse -> Maybe Message
getMessage = message . last . telegramResponseResult

extractMessage :: Maybe Message -> Message
extractMessage (Just x) = x
extractMessage Nothing = error "Not a message?"

getText :: Message -> Maybe T.Text
getText = incoming_text

extractText :: Maybe T.Text -> T.Text
extractText (Just x) = x
extractText Nothing = error "Not a text"

sup :: Handle -> Integer -> IO a
sup h lastid = runReq defaultHttpConfig $ do
  let handle = hBotHandle h
  response <- makeRequest
  let responseB = responseBody response :: TelegramResponse
  let responseId = update_id . last . telegramResponseResult $ responseB
  if responseId == lastid
  then liftIO $ sup h lastid
  else do
    let txt = extractText . getText . extractMessage . getMessage $ responseB
    let ch = T.pack . show $ fromJust . cid . chat . extractMessage . getMessage $ responseB
    let msg = EchoBot.hMessageFromText handle txt
    responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
    _ <- sendResponses responses ch
    liftIO $ threadDelay 100000
    liftIO $ sup h responseId

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

keyboardEncoded :: T.Text
keyboardEncoded = read . show $ encode keyboard

sendMessage :: MonadHttp m => T.Text -> T.Text -> m IgnoreResponse
sendMessage ch txt = req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "sendMessage")
                NoReqBody
                ignoreResponse
                (mconcat $ fmap (uncurry (=:)) [("chat_id", ch), ("text", txt)])

sendKeyboard :: MonadHttp m => T.Text -> T.Text -> m IgnoreResponse
sendKeyboard ch txt = req
              GET
              (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "sendMessage")
              NoReqBody
              ignoreResponse
              (mconcat $ fmap (uncurry (=:)) [("chat_id", ch), ("text", txt), ("reply_markup", keyboardEncoded)])

sendResponses :: MonadHttp m => [EchoBot.Response T.Text] -> T.Text -> m IgnoreResponse
sendResponses [] ch = sendMessage ch "Please enter command or send a message"
sendResponses (EchoBot.MessageResponse x : xs) ch = sendMessage ch x >> sendResponses xs ch
sendResponses ((EchoBot.MenuResponse title _) : _) ch = sendKeyboard ch title 