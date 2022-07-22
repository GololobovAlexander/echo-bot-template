{-# LANGUAGE OverloadedStrings #-}
module FrontEnd.Telegram where
import Network.HTTP.Req
import JSONparsing
import qualified Data.Text as T
import Token ( token )
import qualified EchoBot
import Data.Maybe ( fromJust, isJust, fromMaybe )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Aeson (encode)
import qualified Data.Map as Map
import Logger ((.<))
import qualified Logger

newtype Handle = Handle {
  hBotHandle :: EchoBot.Handle IO T.Text
}

type ChatId = T.Text

keyboard :: Inline
keyboard = Inline {inline_keyboard = [[Button {text = "1", callback_data = 1},
                                       Button {text = "2", callback_data = 2},
                                       Button {text = "3", callback_data = 3},
                                       Button {text = "4", callback_data = 4},
                                       Button {text = "5", callback_data = 5}]]}

keyboardEncoded :: T.Text
keyboardEncoded = read . show $ encode keyboard

getMessage :: TelegramResponse -> Maybe Message
getMessage = message . last . telegramResponseResult

getQuery :: TelegramResponse -> Maybe CallbackQuery
getQuery = callback_query . last . telegramResponseResult

getQueryChatId :: TelegramResponse -> T.Text
getQueryChatId = T.pack . show . fromJust . cid . chat . fromJust . query_message . fromJust . getQuery


getQueryNewCount :: TelegramResponse -> Maybe T.Text
getQueryNewCount = query_data . fromJust . getQuery

getMessageChatId :: TelegramResponse -> T.Text
getMessageChatId = T.pack . show . fromJust . cid . chat . fromJust . getMessage


getMessageText :: TelegramResponse -> Maybe T.Text
getMessageText = incoming_text . fromJust . getMessage

getMessageSticker :: TelegramResponse -> Maybe Sticker
getMessageSticker = sticker . fromJust . getMessage

getUpdateId :: [Result] -> Integer
getUpdateId = update_id . last

makeRequest :: [(T.Text, T.Text)] -> IO (JsonResponse TelegramResponse)
makeRequest params = runReq defaultHttpConfig $ req
                GET
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: "getUpdates")
                NoReqBody
                jsonResponse
                (mconcat $ fmap (uncurry (=:)) params)

sendRequest :: T.Text -> [(T.Text, T.Text)] -> IO IgnoreResponse
sendRequest method params = runReq defaultHttpConfig $ req
                POST
                (https "api.telegram.org" /: T.pack ("bot" ++ token) /: method)
                NoReqBody
                ignoreResponse
                (mconcat $ fmap (uncurry (=:)) params)

getUpdate :: Integer -> IO (JsonResponse TelegramResponse)
getUpdate (-1) = getUpdate 0
getUpdate n = makeRequest [("offset", T.pack $ show n), ("timeout", T.pack $ show (100 :: Integer))]

sendMessage :: ChatId -> T.Text -> IO IgnoreResponse
sendMessage chatId txt = sendRequest "sendMessage" [("chat_id", chatId), ("text", txt)]

sendKeyboard :: ChatId -> T.Text -> IO IgnoreResponse
sendKeyboard chatId txt = sendRequest "sendMessage" [("chat_id", chatId), ("text", txt), ("reply_markup", keyboardEncoded)]

sendSticker :: ChatId -> T.Text -> IO IgnoreResponse
sendSticker chatId stickerId = sendRequest "sendSticker" [("chat_id", chatId), ("sticker", stickerId)]


handleResponse :: Handle -> Map.Map Int Int -> TelegramResponse -> IO ()
handleResponse handle repetitionMap responseB
  | isJust (getQuery responseB) = handleQuery handle repetitionMap responseB
  | isJust (getMessageSticker responseB) = handleSticker handle repetitionMap responseB
  | isJust (getMessageText responseB) = handleText handle repetitionMap responseB
  | otherwise = handleError handle repetitionMap responseB

handleQuery :: Handle -> Map.Map Int Int -> TelegramResponse -> IO ()
handleQuery handle repetitionMap responseB = do
  let repCount = read . T.unpack . fromJust . getQueryNewCount $ responseB :: Int
  let userId = from_id . query_from . fromJust . getQuery $ responseB
  let update = update_id . last . telegramResponseResult $ responseB
  Logger.logInfo (EchoBot.hLogHandle $ hBotHandle handle) $ "The user has set the repetition count to " .< repCount
  --let chatId = fromJust . chat_instance . fromJust . getQuery $ responseB
  runHelper handle (Map.insert userId repCount repetitionMap) (update + 1)

handleSticker :: Handle -> Map.Map Int Int -> TelegramResponse -> IO ()
handleSticker handle repetitionMap responseB = do
  let stickerId = file_id . fromJust . getMessageSticker $ responseB
  let chatId = T.pack . show . fromJust . cid . chat . fromJust . message . last . telegramResponseResult $ responseB
  let msg = EchoBot.hMessageFromText (hBotHandle handle) stickerId
  let userId = from_id . from . fromJust . message . last . telegramResponseResult $ responseB
  let repCount = fromMaybe 1 $ Map.lookup userId repetitionMap
  let update = update_id . last . telegramResponseResult $ responseB
  _ <- liftIO $ EchoBot.respond (hBotHandle handle) (EchoBot.SetRepetitionCountEvent repCount)
  responses <- liftIO $ EchoBot.respond (hBotHandle handle) (EchoBot.MessageEvent msg)
  _ <- handleBotResponse sendSticker responses chatId
  runHelper handle repetitionMap (update + 1)

handleText :: Handle -> Map.Map Int Int -> TelegramResponse -> IO ()
handleText handle repetitionMap responseB = do
  let chatId = T.pack . show . fromJust . cid . chat . fromJust . message . last . telegramResponseResult $ responseB
  let userId = from_id . from . fromJust . message . last . telegramResponseResult $ responseB
  let repCount = fromMaybe 1 $ Map.lookup userId repetitionMap
  let txt = fromJust $ getMessageText responseB
  let msg = EchoBot.hMessageFromText (hBotHandle handle) txt
  let update = update_id . last . telegramResponseResult $ responseB
  _ <- liftIO $ EchoBot.respond (hBotHandle handle) (EchoBot.SetRepetitionCountEvent repCount)
  responses <- liftIO $ EchoBot.respond (hBotHandle handle) (EchoBot.MessageEvent msg)
  _ <- handleBotResponse sendMessage responses chatId
  runHelper handle repetitionMap (update + 1)

handleError :: Handle -> Map.Map Int Int -> TelegramResponse -> IO ()
handleError handle repetitionMap responseB = do
  Logger.logInfo (EchoBot.hLogHandle $ hBotHandle handle) "Unknown type of media was sent"
  let chatId = T.pack . show . fromJust . cid . chat . fromJust . message . last . telegramResponseResult $ responseB
  let update = update_id . last . telegramResponseResult $ responseB
  _ <- sendMessage chatId "This type of media is not supported"
  runHelper handle repetitionMap (update + 1)

handleBotResponse :: (ChatId -> T.Text -> IO IgnoreResponse) -> [EchoBot.Response T.Text] -> ChatId -> IO IgnoreResponse
handleBotResponse _ [] chatId  = sendMessage chatId "Please enter command or send a message"
handleBotResponse method (EchoBot.MessageResponse firstMessage : otherMessages) chatId = method chatId firstMessage >> handleBotResponse method otherMessages chatId
handleBotResponse _ ((EchoBot.MenuResponse title _) : _) chatId = sendKeyboard chatId title

run :: Handle -> IO ()
run handle = do
  runHelper handle Map.empty (-1)

runHelper :: Handle -> Map.Map Int Int -> Integer -> IO ()
runHelper handle mapHandles n = runReq defaultHttpConfig $ do
  responses <- liftIO $ getUpdate n
  let responseB = responseBody responses :: TelegramResponse
  if null $ telegramResponseResult responseB
  then do liftIO $ runHelper handle mapHandles n
  else do
    liftIO $ handleResponse handle mapHandles responseB