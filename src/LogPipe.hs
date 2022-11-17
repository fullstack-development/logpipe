module LogPipe
    ( module LogPipe
    , Class.MonadLog (..)
    , Class.MonadLogContext (..)
    , Common.LogContext (..)
    , Common.LogMessage (..)
    , Common.LogLevel (..)
    , Common.meta
    , Common.meta'
    , Common.lookupMeta
    , Common.metaKey
    , Common.metaEntry
    , Common.lookupMetaEntry
    , Common.synchronize
    , Writers.Console.attachConsoleWriter
    )
where

import FSD.Prelude
import qualified LogPipe.Class as Class
import qualified LogPipe.Common as Common
import qualified LogPipe.Writers.Console as Writers.Console

at :: Text -> Common.LogContext
at dp = Common.LogContext dp mempty

with :: (Class.MonadLogContext m) => Common.LogContext -> m a -> m a
with context act =
    Class.localLogContext (<> context) act

debugWith :: (Class.MonadLog m) => Common.LogContext -> Text -> m ()
debugWith context text =
    Class.sendLogMessage $
        Common.LogMessage (Common.meta Common.LLDebug <> context) text

infoWith :: (Class.MonadLog m) => Common.LogContext -> Text -> m ()
infoWith context text =
    Class.sendLogMessage $
        Common.LogMessage (Common.meta Common.LLInfo <> context) text

warnWith :: (Class.MonadLog m) => Common.LogContext -> Text -> m ()
warnWith context text =
    Class.sendLogMessage $
        Common.LogMessage (Common.meta Common.LLWarn <> context) text

errorWith :: (Class.MonadLog m) => Common.LogContext -> Text -> m ()
errorWith context text =
    Class.sendLogMessage $
        Common.LogMessage (Common.meta Common.LLError <> context) text

criticalWith :: (Class.MonadLog m) => Common.LogContext -> Text -> m ()
criticalWith context text =
    Class.sendLogMessage $
        Common.LogMessage (Common.meta Common.LLCritical <> context) text

debug :: (Class.MonadLog m) => Text -> m ()
debug text =
    Class.sendLogMessage $ Common.LogMessage (Common.meta Common.LLDebug) text

info :: (Class.MonadLog m) => Text -> m ()
info text =
    Class.sendLogMessage $ Common.LogMessage (Common.meta Common.LLInfo) text

warn :: (Class.MonadLog m) => Text -> m ()
warn text =
    Class.sendLogMessage $ Common.LogMessage (Common.meta Common.LLWarn) text

error :: (Class.MonadLog m) => Text -> m ()
error text =
    Class.sendLogMessage $ Common.LogMessage (Common.meta Common.LLError) text

critical :: (Class.MonadLog m) => Text -> m ()
critical text =
    Class.sendLogMessage $ Common.LogMessage (Common.meta Common.LLCritical) text
