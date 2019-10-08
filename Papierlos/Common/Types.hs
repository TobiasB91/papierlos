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
  database :: String
} deriving Show


data Document = Document { 
  docId :: Maybe Int ,
  fileName :: String ,
  content :: T.Text ,
  filePath :: String ,
  thumbnail :: String ,
  date :: UTCTime 
} deriving Show

instance FromRow Document where
  fromRow = Document
    <$> field 
    <*> field
    <*> field 
    <*> field 
    <*> field
    <*> field

instance ToRow Document where
  toRow (Document a b c d e f) = toRow (a, b, c, d, e, f)
