{-# LANGUAGE DeriveGeneric , FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies #-}

module Papierlos.Common.Types where

import Data.Aeson
import GHC.Generics
import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock 
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B
import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Backend.Internal
import Servant.Server (Handler)

type PapierM = ReaderT Config (SeldaT SQLite Handler)

runPapierM :: Config -> PapierM a -> Handler a 
runPapierM cf ac = withSQLite (database . general $ cf) $ runReaderT ac cf 

instance MonadSelda PapierM where
  type Backend PapierM = SQLite
  withConnection act = ReaderT (\_ -> S get) >>= act

data Config = Config {
  programs :: ProgramsConfig ,
  general :: GeneralConfig
} deriving Show

data ProgramsConfig = ProgramsConfig { 
  tesseract :: String ,
  pdfimages :: String ,
  unpaper :: String ,
  magick :: String ,
  pdftoppm :: String
} deriving Show

data GeneralConfig = GeneralConfig {
  language :: String ,
  psm :: Int ,
  tesseractEngine :: Int ,
  consumptionDir :: String ,
  tmpDir :: String , 
  storageDir :: String ,
  database :: String ,
  timeout :: Int ,
  prefix :: String ,
  dpi :: Int
} deriving Show

data Document = Document { 
  document_id :: ID Document ,
  document_name :: T.Text ,
  document_content :: T.Text ,
  document_pdf :: ID DocumentFile ,
  document_thumbnail :: ID ThumbnailFile ,
  document_date :: UTCTime 
} deriving (Generic, Show) 

data ThumbnailFile = ThumbnailFile {
  thumbnailFile_id :: ID ThumbnailFile ,
  thumbnailFile_path :: T.Text
} deriving (Generic, Show)

data DocumentFile = DocumentFile {
  documentFile_id :: ID DocumentFile ,
  documentFile_path :: T.Text
} deriving (Generic, Show)

instance ToJSON (ID Document) where
  toJSON = toJSON . fromId

instance ToJSON (ID ThumbnailFile) where
  toJSON = toJSON . fromId

instance ToJSON (ID DocumentFile) where
  toJSON = toJSON . fromId
  
instance SqlRow ThumbnailFile
instance SqlRow DocumentFile

instance ToJSON Document
instance SqlRow Document

getTesseract = asks $ tesseract . programs :: PapierM String
getPdfimages = asks $ pdfimages . programs :: PapierM String
getUnpaper = asks $ unpaper . programs :: PapierM String
getMagick = asks $ magick . programs :: PapierM String
getPdftoppm = asks $ pdftoppm . programs :: PapierM String

getLanguage = asks $ language . general :: PapierM String
getPsm = asks $ psm . general :: PapierM Int
getTesseractEngine = asks $ tesseractEngine . general :: PapierM Int
getConsumptionDir = asks $ consumptionDir . general :: PapierM String
getTmpDir = asks $ tmpDir . general :: PapierM String
getStorageDir = asks $ storageDir . general :: PapierM String
getDatabase = asks $ database . general :: PapierM String
getTimeout = asks $ timeout . general :: PapierM Int
getPrefix = asks $ prefix . general :: PapierM String
getDpi = asks $ dpi . general :: PapierM Int

data SearchAlgorithm = ContainsText
