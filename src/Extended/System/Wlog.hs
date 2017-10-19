-- | Extended @log-warper@ functionality to work with importify tool.

module Extended.System.Wlog
       ( initImportifyLogger
       , printDebug
       , printError
       , printInfo
       , printNotice
       , printWarning
       ) where

import           Universum

import           Lens.Micro.Platform (zoom, (?=))
import           System.Wlog         (LoggerConfig, LoggerNameBox, Severity (..),
                                      consoleOutB, lcTermSeverity, lcTree, logDebug,
                                      logError, logInfo, logNotice, logWarning,
                                      ltSeverity, setupLogging, usingLoggerName,
                                      zoomLogger)

importifyLoggerConfig :: Severity -> LoggerConfig
importifyLoggerConfig importifySeverity = executingState consoleOutB $ do
    lcTermSeverity ?= importifySeverity
    zoom lcTree $ do
        ltSeverity ?= Warning
        zoomLogger "importify" $
            ltSeverity ?= importifySeverity

-- | Initializes importify logger.
initImportifyLogger :: MonadIO m => Severity -> m ()
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
