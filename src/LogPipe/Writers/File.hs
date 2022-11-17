module LogPipe.Writers.File
    ( OpenMode (..)
    , Formatter (..)
    , attachFileWriter
    , plainText
    )
where

import FSD.Prelude
import Data.Word
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
import qualified Text.Printf

data OpenMode
    = OMAppend
    | OMRewrite ByteString ByteString

data Formatter = Formatter
    { formatterOpenMode :: OpenMode
    , formatterRenderMessage :: Word64 -> LogMessage -> IO ByteString
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
    (Word64 -> LogMessage -> IO ByteString) ->
    System.IO.Handle ->
    Word64 ->
    LogMessage ->
    IO ()
fileWriter renderMessage handle tid msg = do
    bs <- renderMessage tid msg
    BS.hPutStr handle bs

plainText :: OpenMode -> Formatter
plainText openMode = Formatter
    { formatterOpenMode = openMode
    , formatterRenderMessage = \tid msg -> do
        let msgText = logMessageText msg
        let context = logMessageContext msg
        let domain = logContextDomainPrefix context
        let metadata = logContextMetadata context
        let threadTag = packText $ Text.Printf.printf "(%16x)" tid
        let introTag =
                case lookupMetaEntry "logLevel" metadata of
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
            toUtf8 (Text.pack nowString) <> toUtf8 threadTag <> introTag <>
            toUtf8 domain <> "\n" <>
            renderMeta (Map.delete "logLevel" metadata) <>
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
