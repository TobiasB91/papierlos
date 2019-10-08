{-# LANGUAGE OverloadedStrings #-}

module Papierlos.Config.Parser where

import Data.String 
import Data.Ini.Config
import Data.Text (Text)
import Control.Applicative

import Papierlos.Common.Types 

parseConfig :: Text -> Either String Config
parseConfig = flip parseIniFile configP

configP :: IniParser Config 
configP = do
  programsCf <- section "Programs" $ 
    ProgramsConfig 
    <$> (fieldOf "tesseract" filepath <|> pure "tesseract")
    <*> (fieldOf "pdfimages" filepath <|> pure "pdfimages")
    <*> (fieldOf "unpaper" filepath <|> pure "unpaper")
    <*> (fieldOf "magick" filepath <|> pure "magick")
    <*> (fieldOf "pdftoppm" filepath <|> pure "pdftoppm")
  generalCf <- section "General" $  
    GeneralConfig
    <$> (fieldOf "language" string <|> pure "eng")
    <*> (fieldOf "psm" number <|> pure 3)
    <*> (fieldOf "engine" number <|> pure 3)
    <*> fieldOf "consumptionDir" filepath
    <*> (fieldOf "tmpDir" filepath <|> pure "/var/tmp")
    <*> (fieldOf "storageDir" filepath <|> pure "/media")
    <*> (fieldOf "database" filepath <|> pure "/home/tobr/media/papierlos.db")
  pure $ Config programsCf generalCf

-- TODO
filepath :: (IsString a) => Text -> Either String a
filepath = string
