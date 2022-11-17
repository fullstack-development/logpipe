{-# LANGUAGE AllowAmbiguousTypes #-}

module LogPipe.Common
    ( meta
    , meta'
    , lookupMeta
    , metaKey
    , metaEntry
    , lookupMetaEntry
    , LogMessage (..)
    , LogContext (..)
    , LogLevel (..)
    , WriterHandle
    , attachWriter
    , detachWriter
    , sendLogMessage
    , synchronize
    )
where

import FSD.Prelude
import Control.Concurrent
import Control.Concurrent.STM
import Data.Word
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson.TH
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified System.IO.Unsafe
import qualified Text.Read
import qualified Type.Reflection as Type





meta ::
    forall t. (Type.Typeable t, ToJSON t) => t -> LogContext
meta object =
    meta' (metaKey @t) object

meta' ::
    (ToJSON t) => Text -> t -> LogContext
meta' key object =
    LogContext "" (metaEntry key object)

lookupMeta ::
    forall t. (Type.Typeable t, FromJSON t) => LogContext -> Maybe t
lookupMeta (LogContext _ m) =
    lookupMetaEntry (metaKey @t) m



metaKey :: forall t. (Type.Typeable t) => Text
metaKey = Text.pack $ lowerInit $ show $ Type.typeRep @t

metaEntry ::
    (Aeson.ToJSON t) =>
    Text ->
    t ->
    Map Text Aeson.Value
metaEntry key object = Map.singleton key metaValue
  where
    metaValue = Aeson.toJSON object

lookupMetaEntry ::
    (Aeson.FromJSON t) =>
    Text ->
    Map Text Aeson.Value ->
    Maybe t
lookupMetaEntry key m = do
    value <- Map.lookup key m
    case Aeson.fromJSON value of
        Aeson.Success x -> Just x
        Aeson.Error _ -> Nothing



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



data ChannelInput
    = ChannelInputLog Word64 LogMessage (TMVar ChannelInput)
    | ChannelInputSynchronize (TVar Bool) (TVar Bool) (TMVar ChannelInput)
    | ChannelInputTerminate (TVar Bool)

newtype WriterHandle = WriterHandle (TVar (Maybe (TMVar ChannelInput)))

globalWriterSet :: TVar (Seq WriterHandle)
globalWriterSet = System.IO.Unsafe.unsafePerformIO $ newTVarIO Seq.Empty
{-# NOINLINE globalWriterSet #-}

attachWriter ::
    (forall t. (b -> IO t) -> IO t) ->
    (b -> Word64 -> LogMessage -> IO ()) ->
    IO WriterHandle
attachWriter wrapper lineHandler = do
    initialInputTMVar <- newEmptyTMVarIO
    writerQueueTailTVar <- newTVarIO $ Just initialInputTMVar
    queueHeadTVar <- newTVarIO $ Just initialInputTMVar
    let handle = WriterHandle writerQueueTailTVar
    void $ uninterruptibleMask_ $ do
        atomically $ modifyTVar globalWriterSet (handle Seq.:<|)
        forkIOWithUnmask $ \restore -> do
            finally
                (restore $ wrapper (listenLoop queueHeadTVar . Just))
                (do
                    atomically $ writeTVar writerQueueTailTVar Nothing
                    listenLoop queueHeadTVar Nothing
                )
    pure handle
  where
    listenLoop queueHeadTVar mbResource =
        goListen
      where
        goListen = do
            join $ atomically $ do
                mbInputTMVar <- readTVar queueHeadTVar
                case mbInputTMVar of
                    Just inputTMVar -> do
                        input <- readTMVar inputTMVar
                        case input of
                            ChannelInputLog tid logMessage next -> do
                                writeTVar queueHeadTVar $ Just next
                                pure $ do
                                    forM_ mbResource $ \resource ->
                                        lineHandler resource tid logMessage
                                    goListen
                            ChannelInputSynchronize doneTVar waitTVar next -> do
                                writeTVar queueHeadTVar $ Just next
                                writeTVar doneTVar True
                                pure $ do
                                    waitForBoolTVar waitTVar
                                    goListen
                            ChannelInputTerminate doneTVar -> do
                                writeTVar queueHeadTVar Nothing
                                writeTVar doneTVar True
                                pure $ do
                                    goListen
                    Nothing -> do
                        pure $ do
                            pure ()

detachWriter ::
    WriterHandle ->
    IO ()
detachWriter (WriterHandle writerQueueTailTVar) = do
    join $ atomically $ do
        mbQueueTail <- readTVar writerQueueTailTVar
        case mbQueueTail of
            Nothing -> pure $ pure ()
            Just queueTail -> do
                doneTVar <- newTVar False
                putTMVar queueTail $
                    ChannelInputTerminate doneTVar
                writeTVar writerQueueTailTVar Nothing
                pure $ do
                    waitForBoolTVar doneTVar

sendLogMessage ::
    LogMessage ->
    IO ()
sendLogMessage logMessage = do
    threadPtr <- myThreadId
    let threadId =
            case
                List.stripPrefix "ThreadId " (show threadPtr)
                    >>= Text.Read.readMaybe
              of
                Nothing -> 0
                Just n -> n
    writers <- atomically $ readTVar globalWriterSet
    forM_ writers $ \(WriterHandle writerQueueTailTVar) -> do
        atomically $ do
            mbQueueTail <- readTVar writerQueueTailTVar
            case mbQueueTail of
                Nothing -> pure ()
                Just queueTail -> do
                    next <- newEmptyTMVar
                    putTMVar queueTail $
                        ChannelInputLog threadId logMessage next
                    writeTVar writerQueueTailTVar $ Just next

synchronize ::
    IO ()
synchronize = do
    doneTVar <- newTVarIO False
    finally
        (join $ atomically $ do
            writers <- readTVar globalWriterSet
            syncTVarSeq <-
                forM writers $ \(WriterHandle writerQueueTailTVar) -> do
                    mbQueueTail <- readTVar writerQueueTailTVar
                    case mbQueueTail of
                        Nothing -> pure Nothing
                        Just queueTail -> do
                            next <- newEmptyTMVar
                            syncTVar <- newTVar False
                            putTMVar queueTail $
                                ChannelInputSynchronize syncTVar doneTVar next
                            writeTVar writerQueueTailTVar $ Just next
                            pure $ Just syncTVar
            pure $ do
                forM_ syncTVarSeq $ \mbSyncTVar -> do
                    forM_ mbSyncTVar $ \syncTVar -> do
                        waitForBoolTVar syncTVar
        )
        (uninterruptibleMask_ $ do
            atomically $ writeTVar doneTVar True
        )

waitForBoolTVar :: TVar Bool -> IO ()
waitForBoolTVar btvar = do
    atomically $ do
        b <- readTVar btvar
        unless b retry
