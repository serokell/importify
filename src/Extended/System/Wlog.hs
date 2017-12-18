-- | Extended @log-warper@ functionality to work with importify tool.

module Extended.System.Wlog
       ( initImportifyLogger
       , printDebug
       , printError
       , printInfo
       , printNotice
       , printWarning
       ) where

import Universum

import Lens.Micro.Platform (zoom, (.=), (?=))
import System.Wlog (LoggerConfig, LoggerNameBox, Severities, lcShowTime, lcTree, logDebug, logError,
                    logInfo, logNotice, logWarning, ltSeverity, productionB, setupLogging,
                    usingLoggerName, warningPlus, zoomLogger)

importifyLoggerConfig :: Severities -> LoggerConfig
importifyLoggerConfig importifySeverities = executingState productionB $ do
    lcShowTime .= Any False
    zoom lcTree $ do
        ltSeverity ?= warningPlus
        zoomLogger "importify" $
            ltSeverity ?= importifySeverities

-- | Initializes importify logger.
initImportifyLogger :: MonadIO m => Severities -> m ()
initImportifyLogger = setupLogging Nothing . importifyLoggerConfig

withImportify :: LoggerNameBox m a -> m a
withImportify = usingLoggerName "importify"

-- | Prints 'System.Wlog.Debug' message.
printDebug :: MonadIO m => Text -> m ()
printDebug = liftIO . withImportify  . logDebug

-- | Prints 'Info' message.
printInfo :: MonadIO m => Text -> m ()
printInfo = liftIO . withImportify  . logInfo

-- | Prints 'Notice' message.
printNotice :: MonadIO m => Text -> m ()
printNotice = liftIO . withImportify  . logNotice

-- | Prints 'Warning' message.
printWarning :: MonadIO m => Text -> m ()
printWarning = liftIO . withImportify . logWarning

-- | Prints 'Error' message.
printError :: MonadIO m => Text -> m ()
printError = liftIO . withImportify . logError
