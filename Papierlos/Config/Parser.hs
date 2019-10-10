{-# LANGUAGE OverloadedStrings #-}

module Papierlos.Config.Parser where

import Data.String 
import Data.Ini.Config
import Data.Text (Text)
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath

import Papierlos.Common.Types 

-- TODO: do this the right way
{-# NOINLINE home #-}
home :: FilePath
home = unsafePerformIO getHomeDirectory 

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
    <*> (fieldOf "storageDir" filepath <|> pure (home </> ".papierlos/files"))
    <*> (fieldOf "database" filepath <|> pure  (home </> ".papierlos/papierlos.db"))
    <*> (fieldOf "consumptionTimeout" number <|> pure 20000000)
    <*> (fieldOf "filePrefix" string <|> pure "papierlos")
    <*> (fieldOf "dpi" number <|> pure 300)
  pure $ Config programsCf generalCf

-- TODO
filepath :: (IsString a) => Text -> Either String a
filepath = string
