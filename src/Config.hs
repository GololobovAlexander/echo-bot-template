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
getBotConfig = return $ EchoBot.Config -- this need to be in another file for easier control over parameters
  { EchoBot.confHelpReply = T.pack "that's helpful",
    EchoBot.confRepeatReply = T.pack "Current repetition count is {count}",
    EchoBot.confRepetitionCount = 1
  }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = return $ Logger.Impl.Config
  { Logger.Impl.confFileHandle = System.IO.stdout,
    Logger.Impl.confMinLevel = Logger.Warning
  }

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.ConsoleFrontEnd -- need to make in choose between telegram and console
