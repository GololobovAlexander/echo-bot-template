{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot
import Data.Maybe (fromMaybe)

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run h = do
  TIO.putStrLn "Please input message, /help or /repeat command"
  -- 1. Read a line from the console.
  line <- getLine
  -- 2. Send it to the bot, get its response and output it.
  let handle = hBotHandle h
  let message = EchoBot.hMessageFromText handle (T.pack line)
  responses <- EchoBot.respond handle (EchoBot.MessageEvent message)
  let getR [] = putStrLn ""
      getR ((EchoBot.MessageResponse a) : xs) = TIO.putStrLn a >> getR xs
      getR ((EchoBot.MenuResponse _ xs) : _) = do
        putStrLn "Input the number of repeatitions"
        newCount <- getLine
        let nc = read newCount
        let newEvent = fromMaybe (EchoBot.MessageEvent "") $ lookup nc xs
        _ <- EchoBot.respond handle newEvent
        putStrLn ""
  getR responses
  --let texts = map (getR handle) responses
  --printList texts
  -- 3. Go to 1.
  run h
  -- error "Not implemented"
