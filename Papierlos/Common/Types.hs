{-# LANGUAGE DuplicateRecordFields #-}

module Papierlos.Common.Types where

import Control.Monad.Reader
import Data.Time.Clock 
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

type PapierM = ReaderT Config IO

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
  docId :: Maybe Int ,
  fileName :: String ,
  content :: T.Text ,
  filePath :: String ,
  thumbnail :: String ,
  date :: UTCTime 
} deriving Show

getTesseract = asks $ tesseract . programs :: PapierM String
getPdfimages = asks $ pdfimages . programs :: PapierM String
getUnpaper = asks $ unpaper . programs :: PapierM String
getMagick = asks $ magick . programs :: PapierM String
getPdftoppm = asks $ pdftoppm . programs :: PapierM String

getLanguage = asks $ language . general :: PapierM String
getPsm = asks $ psm . general :: PapierM Int
getTesseractEngine = asks $ tesseractEngine . general  :: PapierM Int
getConsumptionDir = asks $ consumptionDir . general :: PapierM String
getTmpDir = asks $ tmpDir . general :: PapierM String
getStorageDir = asks $ storageDir . general :: PapierM String
getDatabase = asks $ database . general :: PapierM String
getTimeout = asks $ timeout . general :: PapierM Int
getPrefix = asks $ prefix . general :: PapierM String
getDpi = asks $ dpi . general :: PapierM Int
