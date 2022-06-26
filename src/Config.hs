-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import qualified Data.Text as T
import qualified Logger
import qualified System.IO   

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = return $ EchoBot.Config
  { EchoBot.confHelpReply = T.pack "If you want to change the repetition count of echoed messages send /repeat. Otherwise just enter your message",
    EchoBot.confRepeatReply = T.pack "Current repetition count is {count}",
    EchoBot.confRepetitionCount = 1
  }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = return $ Logger.Impl.Config
  { Logger.Impl.confFileHandle = System.IO.stdout,
    Logger.Impl.confMinLevel = Logger.Warning
  }

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.TelegramFrontEnd -- or "return ConfigurationTypes.ConsoleFrontEnd" for console
