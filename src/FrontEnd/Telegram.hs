{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module FrontEnd.Telegram where
import Data.Aeson (encode)
import Network.HTTP.Req
import Token (token)
import JSONparsing
import qualified EchoBot
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.Text as T
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.Map as Map

newtype Handle = Handle {
  hBotHandle :: EchoBot.Handle IO T.Text
}

newtype State = State {
  hGetState :: EchoBot.State
} 

type ChatId = T.Text

-- | Should be "sendMessage" or "sendSticker" for now
type Method = T.Text

-- | Should be "sticker" or "text" for now
type QueryParameter = T.Text

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

getResponses :: TelegramResponse -> Handle -> Req IgnoreResponse
getResponses responseB handle
--     | isJust (getQuery responseB) = handleCallbackQuery responseB handle "text"
    | isJust (getMessage responseB) = case () of
       () |isJust (incoming_text . fromJust . getMessage $ responseB) -> handleIncomingText responseB handle "text"
          |isJust (sticker . fromJust . getMessage $ responseB) -> handleIncomingSticker responseB handle "sticker"
          |otherwise -> error "Unknown message type"
    | otherwise = error "Unknown reponse"

-- handleCallbackQuery :: TelegramResponse -> Handle -> QueryParameter -> Req IgnoreResponse
-- handleCallbackQuery responseB h _ = do
--         let newCount = getQueryNewCount responseB
--         let chatId = getQueryChatId responseB
--         let handle = hBotHandle h
--         menuResponse <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent (EchoBot.hMessageFromText handle "/repeat"))
--         let [EchoBot.MenuResponse _ menu] = menuResponse
--         let count = read . T.unpack $ newCount
--         let event = fromJust $ lookup count menu

--         _ <- liftIO $ EchoBot.respond handle event
--         _ <- sendMessage chatId (T.replace "{count}" newCount "Repetition count is set to {count}") "sendMessage" "text"
--         sendResponses [] chatId "sendMessage" "text"

handleIncomingText :: TelegramResponse -> Handle -> QueryParameter -> Req IgnoreResponse
handleIncomingText responseB h param = do
        let chatId = getMessageChatId responseB
        let txt = getMessageText responseB
        let handle = hBotHandle h
        let msg = EchoBot.hMessageFromText handle txt
        responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
        sendResponses responses chatId "sendMessage" param

handleIncomingSticker :: TelegramResponse -> Handle -> QueryParameter -> Req IgnoreResponse
handleIncomingSticker responseB h param = do
        let chatId = getMessageChatId responseB
        let getSticker = getMessageSticker responseB
        let handle = hBotHandle h
        let msg = EchoBot.hMessageFromText handle getSticker
        responses <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent msg)
        sendResponses responses chatId "sendSticker" param

keyboard :: Inline
keyboard = Inline {inline_keyboard = [[Button {text = "1", callback_data = 1},
                                       Button {text = "2", callback_data = 2},
                                       Button {text = "3", callback_data = 3},
                                       Button {text = "4", callback_data = 4},
                                       Button {text = "5", callback_data = 5}]]}

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
sendResponses (EchoBot.MessageResponse firstMessage : otherMessages) chatId method param = sendMessage chatId firstMessage method param >>
                                                                                      sendResponses otherMessages chatId method param
sendResponses ((EchoBot.MenuResponse title _) : _) chatId method param = sendKeyboard chatId title method param

repCounts :: Map.Map Integer Int
repCounts = Map.empty

getUserId :: TelegramResponse -> Integer
getUserId responseB
        | isJust (getMessage responseB) = from_id . from . fromJust . getMessage $ responseB
        | isJust (getQuery responseB) = from_id . query_from . fromJust . getQuery $ responseB
        | otherwise = error ""

checkUserInHandles :: Integer -> Handle -> Map.Map Integer Handle -> Handle
checkUserInHandles userId handle mapHandles =
        fromMaybe handle (Map.lookup userId mapHandles)

run :: Handle -> IO ()
run h = sup h 0 repCounts

sup :: Handle -> Integer -> Map.Map Integer Int -> IO a
sup h lastid mapHandles = runReq defaultHttpConfig $ do
  let handle = hBotHandle h
  response <- makeRequest
  let responseB = responseBody response :: TelegramResponse
  let result = telegramResponseResult responseB
  let userId = getUserId responseB
  if null result
  then liftIO $ sup h lastid mapHandles
  else do
        if isJust (getQuery responseB)
        then do
                let newCount = getQueryNewCount responseB
                let count = read . T.unpack $ newCount 
                liftIO $ sup h lastid (Map.insert userId count mapHandles)
        else do
                let responseId = update_id . last $ result 
                let newCount = fromMaybe 1 (Map.lookup userId mapHandles)
                if responseId == lastid
                then do
                        liftIO $ sup h lastid (Map.insert userId newCount mapHandles)
                else do
                        menuResponse <- liftIO $ EchoBot.respond handle (EchoBot.MessageEvent (EchoBot.hMessageFromText handle "/repeat"))
                        let [EchoBot.MenuResponse _ menu] = menuResponse
                        let event = fromJust $ lookup newCount menu
                        _ <- liftIO $ EchoBot.respond handle event
                        _ <- getResponses responseB h
                        liftIO $ sup h responseId (Map.insert userId newCount mapHandles)
