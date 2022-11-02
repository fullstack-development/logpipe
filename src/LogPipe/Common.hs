module LogPipe.Common
    ( metaEntry
    , lookupMeta
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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson.TH
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified System.IO.Unsafe
import qualified Type.Reflection as Type



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
    = ChannelInputLog LogMessage (TMVar ChannelInput)
    | ChannelInputSynchronize (TVar Bool) (TVar Bool) (TMVar ChannelInput)
    | ChannelInputTerminate (TVar Bool)

newtype WriterHandle = WriterHandle (TVar (Maybe (TMVar ChannelInput)))

globalWriterSet :: TVar (Seq WriterHandle)
globalWriterSet = System.IO.Unsafe.unsafePerformIO $ newTVarIO Seq.Empty
{-# NOINLINE globalWriterSet #-}

attachWriter ::
    (forall t. (b -> IO t) -> IO t) ->
    (b -> LogMessage -> IO ()) ->
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
                            ChannelInputLog logMessage next -> do
                                writeTVar queueHeadTVar $ Just next
                                pure $ do
                                    forM_ mbResource $ \resource ->
                                        lineHandler resource logMessage
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
    writers <- atomically $ readTVar globalWriterSet
    forM_ writers $ \(WriterHandle writerQueueTailTVar) -> do
        atomically $ do
            mbQueueTail <- readTVar writerQueueTailTVar
            case mbQueueTail of
                Nothing -> pure ()
                Just queueTail -> do
                    next <- newEmptyTMVar
                    putTMVar queueTail $
                        ChannelInputLog logMessage next
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
