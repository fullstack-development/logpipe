module LogPipe.Common
    ( LogMessage (..)
    , LogContext (..)
    , LogLevel (..)
    , closeLogSystem
    , WriterHandle
    , attachWriter
    , detachWriter
    , sendLogMessage
    , metaEntry
    , lookupMeta
    )
where

import FSD.Prelude
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson.TH
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.IO.Unsafe
import qualified Type.Reflection as Type

data LogMessage = LogMessage
    { logMessageContext :: LogContext
    , logMessageText :: Text
    }
    deriving (Show, Eq, Ord, Generic)

data LogContext = LogContext
    { logContextDomainPrefix :: Text
    , logContextMetadata :: Map Text Aeson.Value
    }
    deriving (Show, Eq, Ord, Generic)

instance IsString LogContext where
    fromString str = LogContext (fromString str) mempty

instance Semigroup LogContext where
    LogContext dpa ma <> LogContext dpb mb =
        LogContext (dpa <> dpb) (ma <> mb)

instance Monoid LogContext where
    mempty = LogContext "" mempty

data LogLevel
    = LLDebug
    | LLInfo
    | LLWarn
    | LLError
    | LLCritical
    deriving (Show, Eq, Ord, Generic)

Aeson.TH.deriveJSON
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = lowerInit . List.drop 2
        }
    ''LogLevel

data ChannelSignal
    = ChannelSignalLog LogMessage
    | ChannelSignalTerminate
    deriving (Show, Eq, Ord, Generic)

logMessageTChan :: TChan ChannelSignal
logMessageTChan = System.IO.Unsafe.unsafePerformIO newBroadcastTChanIO
{-# NOINLINE logMessageTChan #-}

pendingWriterCountTVar :: TVar Int
pendingWriterCountTVar = System.IO.Unsafe.unsafePerformIO $ newTVarIO 0
{-# NOINLINE pendingWriterCountTVar #-}

closeLogSystem :: IO ()
closeLogSystem = do
    atomically $ writeTChan logMessageTChan ChannelSignalTerminate
    atomically $ do
        count <- readTVar pendingWriterCountTVar
        unless (count == 0) retry

data WriterStatus
    = WSWorking
    | WSDetaching (TVar Bool)

newtype WriterHandle = WriterHandle (TVar WriterStatus)

attachWriter ::
    (forall t. (b -> IO t) -> IO t) ->
    (b -> LogMessage -> IO ()) ->
    IO WriterHandle
attachWriter wrapper lineHandler = do
    ourTChan <- atomically $ dupTChan logMessageTChan
    detachTVar <- newTVarIO WSWorking
    void $ uninterruptibleMask_ $ do
        atomically $ modifyTVar' pendingWriterCountTVar (+ 1)
        forkIOWithUnmask $ \restore -> do
            finally
                (restore $ wrapper $ listenLoop detachTVar ourTChan)
                (atomically $ modifyTVar' pendingWriterCountTVar (subtract 1))
    pure $ WriterHandle detachTVar
  where
    listenLoop detachTVar ourTChan resource =
        loop
      where
        loop = do
            join $ atomically $ do
                detachState <- readTVar detachTVar
                case detachState of
                    WSWorking -> do
                        msg <- readTChan ourTChan
                        case msg of
                            ChannelSignalLog logMessage ->
                                pure (lineHandler resource logMessage >> loop)
                            ChannelSignalTerminate ->
                                pure (pure ())
                    WSDetaching signalTVar -> do
                        writeTVar signalTVar True
                        pure (pure ())

detachWriter ::
    WriterHandle ->
    IO ()
detachWriter (WriterHandle detachTVar) = loop
  where
    loop = do
        join $ atomically $ do
            detachState <- readTVar detachTVar
            case detachState of
                WSWorking -> do
                    signalTVar <- newTVar False
                    writeTVar detachTVar (WSDetaching signalTVar)
                    pure loop
                WSDetaching signalTVar -> do
                    signalFlag <- readTVar signalTVar
                    unless signalFlag retry
                    pure (pure ())

sendLogMessage :: LogMessage -> IO ()
sendLogMessage logMessage = do
    atomically $ writeTChan logMessageTChan $ ChannelSignalLog logMessage

metaEntry ::
    (Type.Typeable t, Aeson.ToJSON t) =>
    t ->
    Map Text Aeson.Value
metaEntry object = Map.singleton metaKey metaValue
  where
    metaKey = Text.pack $ lowerInit $ show $ Type.typeOf object
    metaValue = Aeson.toJSON object

lookupMeta ::
    forall t.
    (Type.Typeable t, Aeson.FromJSON t) =>
    Map Text Aeson.Value ->
    Maybe t
lookupMeta m = do
    value <- Map.lookup metaKey m
    case Aeson.fromJSON value of
        Aeson.Success x -> Just x
        Aeson.Error _ -> Nothing
  where
    metaKey = Text.pack $ lowerInit $ show $ Type.typeRep @t
