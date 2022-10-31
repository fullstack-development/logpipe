module LogPipe.Writers.File
    ( OpenMode (..)
    , Formatter (..)
    , attachFileWriter
    , plainText
    )
where

import FSD.Prelude
import LogPipe.Common
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Yaml.Pretty as Yaml.Pretty
import qualified System.IO

data OpenMode
    = OMAppend
    | OMRewrite ByteString ByteString

data Formatter = Formatter
    { formatterOpenMode :: OpenMode
    , formatterRenderMessage :: LogMessage -> IO ByteString
    }

attachFileWriter ::
    Formatter ->
    FilePath ->
    IO WriterHandle
attachFileWriter fmt path =
    attachWriter
        (fileWrapper (formatterOpenMode fmt) path)
        (fileWriter (formatterRenderMessage fmt))

fileWrapper ::
    OpenMode ->
    FilePath ->
    (System.IO.Handle -> IO t) ->
    IO t
fileWrapper OMAppend path inner = do
    System.IO.withFile path System.IO.AppendMode $ \handle -> do
        inner handle
fileWrapper (OMRewrite intro outro) path inner = do
    System.IO.withFile path System.IO.WriteMode $ \handle -> do
        BS.hPutStr handle intro
        r <- inner handle
        BS.hPutStr handle outro
        pure r

fileWriter ::
    (LogMessage -> IO ByteString) ->
    System.IO.Handle ->
    LogMessage ->
    IO ()
fileWriter renderMessage handle msg = do
    bs <- renderMessage msg
    BS.hPutStr handle bs

plainText :: OpenMode -> Formatter
plainText openMode = Formatter
    { formatterOpenMode = openMode
    , formatterRenderMessage = \msg -> do
        let msgText = logMessageText msg
        let context = logMessageContext msg
        let domain = logContextDomainPrefix context
        let meta = logContextMetadata context
        let introTag =
                case lookupMeta meta of
                    Nothing         -> "         "
                    Just LLDebug    -> " [DEBUG] "
                    Just LLInfo     -> " [INFO]  "
                    Just LLWarn     -> " [WARN]  "
                    Just LLError    -> " [ERROR] "
                    Just LLCritical -> " [CRIT]  "
        now <- Time.getCurrentTime
        let nowString =
                Time.formatTime Time.defaultTimeLocale "%F %T%6Q" now
        pure $
            toUtf8 (Text.pack nowString) <> introTag <>
            toUtf8 domain <> "\n" <>
            renderMeta (Map.delete "logLevel" meta) <>
            trimRight (prefixEachLineWith "    " (toUtf8 msgText))
    }
  where
    toUtf8 = Text.Encoding.encodeUtf8

renderMeta ::
    Map Text Aeson.Value ->
    ByteString
renderMeta m
    | Map.null m = ""
    | otherwise =
        prefixEachLineWith "      | " $
        Yaml.Pretty.encodePretty yamlConfig m
  where
    yamlConfig =
        Yaml.Pretty.setConfCompare compare $
        Yaml.Pretty.setConfDropNull True $
        Yaml.Pretty.defConfig

prefixEachLineWith :: ByteString -> ByteString -> ByteString
prefixEachLineWith pf =
    BS.concat .
    List.map (\l -> pf <> trimRight l <> "\n") .
    BS.split 10 .
    BS.dropWhileEnd (== 10)

trimRight :: ByteString -> ByteString
trimRight = BS.dropWhileEnd (\b -> b <= 32)
