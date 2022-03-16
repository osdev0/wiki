{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Stork where
-- intentionally nongeneric, reuse with care

import           Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import qualified Hakyll as H

data File = File
    { fileTitle    :: Text
    , fileUrl      :: Text
    , filePath     :: Text
    , fileFileType :: Text
    }

instance ToJSON File where
    toJSON (File title url path ftype) =
        object [ "title"    .= title
               , "url"      .= url
               , "path"     .= path
               , "filetype" .= ftype
               ]

data Output = Output
    { outputSaveNearestId :: Bool
    }

instance ToJSON Output where
    toJSON (Output saveNearestId) =
        object [ "save_nearest_html_id" .= saveNearestId ]

data Input = Input
    { inputFiles         :: [File]
    , inputUrlPrefix     :: Text
    , inputBaseDirectory :: Text
    , inputHtmlSelector  :: Text
    }

instance ToJSON Input where
    toJSON (Input files prefix basedir selector) =
        object [ "files"          .= files
               , "url_prefix"     .= prefix
               , "base_directory" .= basedir
               , "html_selector"  .= selector
               ]

data IndexerConfig = IndexerConfig
    { indexerInput  :: Input
    , indexerOutput :: Output
    }

instance ToJSON IndexerConfig where
    toJSON (IndexerConfig input output) =
        object [ "input"   .= input
               , "output"  .= output
               ]

itemToFile :: H.Item String -> H.Compiler File
itemToFile i = do
    let identifier = H.itemIdentifier i
    title <- H.getMetadataField' identifier "title"
    routeMaybe <- H.getRoute identifier
    route <- case routeMaybe of
        Nothing -> fail $ "No route for " ++ (show identifier)
        Just x  -> return x

    return $ File
        { fileTitle = T.pack title
        , fileUrl = T.pack route
        , filePath = T.pack route
        , fileFileType = "HTML"
        }

encodeToLazyUtf8 :: Text -> LBS.ByteString
encodeToLazyUtf8 = LBS.fromChunks . return . E.encodeUtf8

render :: String -> [H.Item String] -> H.Compiler (H.Item LBS.ByteString)
render outPath is = do
    files <- sequenceA $ map itemToFile is
    let input = Input
            { inputFiles = files
            , inputUrlPrefix = "/"
            , inputBaseDirectory = T.pack $ outPath ++ "/"
            , inputHtmlSelector = "#content > article"
            }
    let output = Output
            { outputSaveNearestId = False
            }
    let json = A.encode $ IndexerConfig input output

    storkDb <- H.unixFilterLBS storkCmd' storkArgs json
    H.makeItem storkDb
        where storkCmd = ["stork", "build", "--input", "-", "--output", "/dev/stdout"]
              (storkCmd':storkArgs) = storkCmd
