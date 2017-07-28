-- | Extended @log-warper@ functionality to work with importify tool.

module Extended.System.Wlog
       ( initImportifyLogger
       , printDebug
       , printInfo
       , printNotice
       , printWarning
       ) where

import           Universum

import           Lens.Micro.Mtl (zoom, (?=))
import           System.Wlog    (LoggerConfig, LoggerNameBox, Severity (Warning),
                                 consoleOutB, lcTree, logDebug, logInfo, logNotice,
                                 logWarning, ltSeverity, setupLogging, usingLoggerName,
                                 zoomLogger)

importifyLoggerConfig :: Severity -> LoggerConfig
importifyLoggerConfig importifySeverity =
    executingState consoleOutB $ zoom lcTree $ do
        ltSeverity ?= Warning
        zoomLogger "importify" $
            ltSeverity ?= importifySeverity

-- | Initializes importify logger.
initImportifyLogger :: Severity -> IO ()
initImportifyLogger = setupLogging . importifyLoggerConfig

withImportify :: LoggerNameBox m a -> m a
withImportify = usingLoggerName "importify"

-- | Prints 'System.Wlog.Debug' message.
printDebug :: Text -> IO ()
printDebug = withImportify  . logDebug

-- | Prints 'Info' message.
printInfo :: Text -> IO ()
printInfo = withImportify  . logInfo

-- | Prints 'Notice' message.
printNotice :: Text -> IO ()
printNotice = withImportify  . logNotice

-- | Prints 'Warning' message.
printWarning :: Text -> IO ()
printWarning = withImportify . logWarning
