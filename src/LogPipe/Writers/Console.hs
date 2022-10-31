module LogPipe.Writers.Console
    ( attachConsoleWriter
    )
where

import FSD.Prelude
import Control.Concurrent.STM
import LogPipe.Common
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Error as Text.Encoding.Error
import qualified Data.Time as Time
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified System.Console.ANSI as ANSI
import qualified System.IO.Unsafe

currentConsoleWriterTVar :: TVar (Maybe WriterHandle)
currentConsoleWriterTVar = System.IO.Unsafe.unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE currentConsoleWriterTVar #-}

attachConsoleWriter ::
    IO WriterHandle
attachConsoleWriter = do
    mask_ $ do
        mbPrevWriter <- atomically $ readTVar currentConsoleWriterTVar
        case mbPrevWriter of
            Nothing -> pure ()
            Just prevWriter -> detachWriter prevWriter
        newWriter <-
            attachWriter
                ($ ())
                consoleWriter
        atomically $ writeTVar currentConsoleWriterTVar (Just newWriter)
        pure newWriter

consoleWriter ::
    () ->
    LogMessage ->
    IO ()
consoleWriter _ msg = do
    now <- Time.getCurrentTime
    let nowString =
            Time.formatTime Time.defaultTimeLocale "%F %T%6Q" now
    let message =
            Text.pack (ANSI.setSGRCode introSGR) <>
            Text.pack nowString <> introTag <> domain <> "\n" <>
            renderMeta (Map.delete "logLevel" meta) <>
            trimRight (prefixEachLineWith "    " (logMessageText msg)) <>
            Text.pack (ANSI.setSGRCode [ANSI.Reset])
    Text.IO.putStrLn message
  where
    domain = logContextDomainPrefix context
    meta = logContextMetadata context
    context = logMessageContext msg
    introSGR =
        case lookupMeta meta of
            Nothing ->
                [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White
                ]
            Just LLDebug ->
                [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue
                ]
            Just LLInfo ->
                [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White
                ]
            Just LLWarn ->
                [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta
                ]
            Just LLError ->
                [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red
                ]
            Just LLCritical ->
                [ ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Red
                , ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black
                ]
    introTag =
        case lookupMeta meta of
            Nothing         -> "         "
            Just LLDebug    -> " [DEBUG] "
            Just LLInfo     -> " [INFO]  "
            Just LLWarn     -> " [WARN]  "
            Just LLError    -> " [ERROR] "
            Just LLCritical -> " [CRIT]  "

renderMeta ::
    Map Text Aeson.Value ->
    Text
renderMeta m
    | Map.null m = ""
    | otherwise =
        prefixEachLineWith "      | " $
        Text.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode $
        Yaml.Pretty.encodePretty yamlConfig m
  where
    yamlConfig =
        Yaml.Pretty.setConfCompare compare $
        Yaml.Pretty.setConfDropNull True $
        Yaml.Pretty.defConfig

prefixEachLineWith :: Text -> Text -> Text
prefixEachLineWith prefix =
    Text.unlines . List.map (\l -> prefix <> trimRight l) . Text.lines

trimRight :: Text -> Text
trimRight = Text.dropWhileEnd Char.isSpace
