{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module FrontEnd.Telegram where
import Control.Concurrent
import Data.Aeson (encode)
import Network.HTTP.Req
import Token (token)
import JSONparsing
import qualified EchoBot
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Control.Monad.IO.Class ( MonadIO(liftIO) )


newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }


type ChatId = T.Text

-- | Should be "sendMessage" or "sendSticker" for now
type Method = T.Text

-- | Should be "sticker" or "text" for now
type QueryParameter = T.Text

data State = State
  {
    user  :: Integer,
    chat_id :: Integer} deriving (Show)

makeTempState :: State
makeTempState = State {
  user = 0,
  chat_id = 0
}

makeRequest :: Req (JsonResponse TelegramResponse)
makeRequest = req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "getUpdates")
                NoReqBody
                jsonResponse
                mempty

getMessage :: TelegramResponse -> Maybe Message
getMessage = message . last . telegramResponseResult

getQuery :: TelegramResponse -> Maybe CallbackQuery
getQuery = callback_query . last . telegramResponseResult

getQueryChatId :: TelegramResponse -> T.Text
getQueryChatId = T.pack . show . fromJust . cid . chat . fromJust . query_message . fromJust . callback_query . last . telegramResponseResult

getQueryNewCount :: TelegramResponse -> T.Text
getQueryNewCount = fromJust . query_data . fromJust . callback_query . last . telegramResponseResult

getMessageChatId :: TelegramResponse -> T.Text
getMessageChatId = T.pack . show . fromJust . cid . chat . fromJust . getMessage

getMessageText :: TelegramResponse -> T.Text
getMessageText = fromJust . incoming_text . fromJust . getMessage

getMessageSticker :: TelegramResponse -> T.Text
getMessageSticker = file_id . fromJust . sticker . fromJust . getMessage

getResponses :: TelegramResponse -> EchoBot.Handle IO T.Text -> Req IgnoreResponse
getResponses responseB handle
    | isJust (getQuery responseB) = handleCallbackQuery responseB handle "text"
    | isJust (getMessage responseB) = case () of
       () |isJust (incoming_text . fromJust . getMessage $ responseB) -> handleIncomingText responseB handle "text"
          |isJust (sticker . fromJust . getMessage $ responseB) -> handleIncomingSticker responseB handle "sticker"
          |otherwise -> error "Unknown message type"
    | otherwise = error "Unknown reponse"

getResponseCount :: EchoBot.Response p -> p
getResponseCount (EchoBot.MessageResponse x) = x
getResponseCount (EchoBot.MenuResponse _ _) = error "i'm not gonna do this i swear"

handleCallbackQuery :: TelegramResponse -> EchoBot.Handle IO T.Text -> QueryParameter -> Req IgnoreResponse
handleCallbackQuery responseB handle _ = do
        let nc = getQueryNewCount responseB
        let ch = getQueryChatId responseB
        let msg = EchoBot.hMessageFromText handle nc
        responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
        menuResponse <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent (EchoBot.hMessageFromText handle "/repeat"))
        let [EchoBot.MenuResponse _ menu] = menuResponse
        let gotCount = getResponseCount $ head responses
        let count = read . T.unpack $ gotCount
        let event = fromJust $ lookup count menu
        _ <- liftIO $ EchoBot.respond handle event
        _ <- sendMessage ch (T.replace "{count}" nc "Repetition count is set to {count}") "sendMessage" "text" 
        sendResponses [] ch "sendMessage" "text"

handleIncomingText :: TelegramResponse -> EchoBot.Handle IO T.Text -> QueryParameter -> Req IgnoreResponse
handleIncomingText responseB handle param = do
        let chatId = getMessageChatId responseB
        let txt = getMessageText responseB
        let msg = EchoBot.hMessageFromText handle txt
        responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
        --liftIO $ print responses
        sendResponses responses chatId "sendMessage" param

handleIncomingSticker :: TelegramResponse -> EchoBot.Handle IO T.Text -> QueryParameter -> Req IgnoreResponse
handleIncomingSticker responseB handle param = do
        let ch = getMessageChatId responseB
        let getSticker = getMessageSticker responseB
        let msg = EchoBot.hMessageFromText handle getSticker
        responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
        sendResponses responses ch "sendSticker" param

run :: FrontEnd.Telegram.Handle -> IO ()
run h = sup h 0

sup :: Handle -> Integer -> IO a
sup h lastid = runReq defaultHttpConfig $ do
  let handle = hBotHandle h
  response <- makeRequest
  let responseB = responseBody response :: TelegramResponse
  let responseId = update_id . last . telegramResponseResult $ responseB
  if responseId == lastid
  then liftIO $ sup h lastid
  else do
    _ <- getResponses responseB handle
    liftIO $ threadDelay 100000
    liftIO $ sup h responseId

key1 :: Button
key1 = Button {text = "1", callback_data = 1}

key2 :: Button
key2 = Button {text = "2", callback_data = 2}

key3 :: Button
key3 = Button {text = "3", callback_data = 3}

key4 :: Button
key4 = Button {text = "4", callback_data = 4}

key5 :: Button
key5 = Button {text = "5", callback_data = 5}

keyboard :: Inline
keyboard = Inline {inline_keyboard = [[key1, key2, key3, key4, key5]]}

keyboardEncoded :: T.Text
keyboardEncoded = read . show $ encode keyboard

sendMessage :: ChatId -> T.Text -> Method -> QueryParameter -> Req IgnoreResponse
sendMessage chatId txt method param = req
                POST
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: method)
                NoReqBody
                ignoreResponse
                (mconcat $ fmap (uncurry (=:)) [("chat_id", chatId), (param, txt)])

sendKeyboard :: ChatId -> T.Text -> Method -> QueryParameter -> Req IgnoreResponse
sendKeyboard chatId txt method _ = req
                POST
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: method)
                NoReqBody
                ignoreResponse
                (mconcat $ fmap (uncurry (=:)) [("chat_id", chatId), ("text", txt), ("reply_markup", keyboardEncoded)])

sendResponses :: [EchoBot.Response T.Text] -> ChatId -> Method -> QueryParameter -> Req IgnoreResponse
sendResponses [] chatId _ _ = sendMessage chatId "Please enter command or send a message" "sendMessage" "text"
sendResponses (EchoBot.MessageResponse message : otherMessages) chatId method param = sendMessage chatId message method param >>
                                                                                      sendResponses otherMessages chatId method param
sendResponses ((EchoBot.MenuResponse title _) : _) chatId method param = sendKeyboard chatId title method param