{-# LANGUAGE OverloadedStrings #-}
module Control.Logging (
  module Control.Logging
  , FL.LogType(..)
  , FL.LogStr
  , FL.ToLogStr(..)
  , FL.BufSize
  , FL.defaultBufSize
) where

import           Control.Monad
import           Data.Monoid                hiding ((<>))
import           Data.Semigroup

import           Control.Monad.Reader       (ReaderT, ask)
import qualified System.Log.FastLogger      as FL
import qualified System.Log.FastLogger.Date as FL
import           UnliftIO

data LogLevel = Debug | Info | Warning | Error | Fatal deriving (Eq, Ord, Enum, Bounded, Show)

instance FL.ToLogStr LogLevel where
  toLogStr = \case
    Debug   -> "[DEBUG] "
    Info    -> "[INFO]  "
    Warning -> "[WARN]  "
    Error   -> "[ERROR] "
    Fatal   -> "[FATAL] "

class MonadLog m where
  logM :: LogLevel -> FL.LogStr -> m ()

debugM, infoM, warnM, errorM, fatalM :: MonadLog m => FL.LogStr -> m ()
debugM = logM Debug
infoM = logM Info
warnM = logM Warning
errorM = logM Error
fatalM = logM Fatal

defaultLogM :: MonadIO m => m LoggingEnv -> LogLevel -> FL.LogStr -> m ()
defaultLogM getEnv lvl msg = do
  env <- getEnv
  when (lvl >= leMinimumLogLevel env) $ liftIO $
    leLogger env (\time -> FL.toLogStr time <> FL.toLogStr lvl <> msg)

instance (MonadIO m) => MonadLog (ReaderT LoggingEnv m) where
  logM = defaultLogM ask

defaultTimeCache :: IO (IO FL.FormattedTime)
defaultTimeCache = FL.newTimeCache FL.simpleTimeFormat'

data LoggingEnv = LoggingEnv
  { leLogger          :: !FL.TimedFastLogger
  , leMinimumLogLevel :: !LogLevel
  }

withLoggingEnv :: (MonadUnliftIO m) => FL.LogType -> m FL.FormattedTime -> LogLevel -> (LoggingEnv -> m a) -> m a
withLoggingEnv lty curTime minlvl act = withRunInIO $ \runInIO ->
  FL.withTimedFastLogger (runInIO curTime) lty $ \logger -> runInIO $ act LoggingEnv {leLogger = logger, leMinimumLogLevel = minlvl}
