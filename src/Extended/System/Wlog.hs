-- | Extended @log-warper@ functionality to work with importify tool.

module Extended.System.Wlog
       ( initImportifyLogger
       , printInfo
       , printWarning
       ) where

import           Universum

import           Lens.Micro.Mtl (zoom, (?=))
import           System.Wlog    (LoggerConfig, LoggerNameBox, Severity (Info, Warning),
                                 consoleOutB, lcTree, logInfo, logWarning, ltSeverity,
                                 setupLogging, usingLoggerName, zoomLogger)

importifyLoggerConfig :: LoggerConfig
importifyLoggerConfig = executingState consoleOutB $ zoom lcTree $ do
    ltSeverity ?= Warning
    zoomLogger "importify" $
        ltSeverity ?= Info

-- | Initializes importify logger.
initImportifyLogger :: IO ()
initImportifyLogger = setupLogging importifyLoggerConfig

withImportify :: LoggerNameBox m a -> m a
withImportify = usingLoggerName "importify"

-- | Prints 'Info' message.
printInfo :: Text -> IO ()
printInfo = withImportify  . logInfo

-- | Prints 'Warning' message.
printWarning :: Text -> IO ()
printWarning = withImportify . logWarning
