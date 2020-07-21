{-# LANGUAGE OverloadedStrings , LambdaCase #-}
module Papierlos.OCR.Consume where

import Papierlos.Common.Types
import Papierlos.Common.Database

import Control.Monad.Reader
import Data.Char (isSpace)
import System.Command
import System.Directory
import System.Exit
import System.FilePath
import Data.List (isSuffixOf, inits) 
import Data.Time.Clock
import Safe (headMay)
import Control.Concurrent
import Database.Selda (def)
import Servant.Server (Handler)

import qualified Data.Text as T
import qualified Data.Text.IO as T

startConsume :: Config -> Handler ()
startConsume = flip runPapierM consume

consume :: PapierM () 
consume =
  getPdfPath >>= \case
    Nothing  -> pure ()
    Just pdf -> do
      content   <- convertToPGM >=> runTesseract $ pdf
      date      <- liftIO getCurrentTime

      pk <- insertDocument $ 
        Document def def content def def date 
      
      thumbnail <- createThumbnail pk pdf
      path      <- movePdf pk pdf

      updateDocumentName pk $ T.pack $ takeFileName path
      updateDocumentPath pk $ T.pack path
      updateDocumentThumbnail pk $ T.pack thumbnail

movePdf :: Int -> FilePath -> PapierM FilePath 
movePdf pk pdf = do
  storageDir <- getStorageDir
  let
    newName = takeBaseName pdf ++ show pk ++ takeExtension pdf
    newPath = storageDir </> newName 
  liftIO $ do
    Exit c <- command [] "mv" [pdf, newPath]
    pure newPath
    
createThumbnail :: Int -> FilePath -> PapierM FilePath
createThumbnail pk pdfPath = do
  storageDir <- getStorageDir
  pdfimages  <- getPdfimages
  liftIO $ do 
    let outName = "thumbnail" ++ show pk
    Exit c <- command [ Cwd storageDir ] pdfimages 
      ["-f", "1", "-l", "1", "-png", pdfPath, outName ]
    pure $ storageDir </> (outName ++ "-000.png")    

getPdfPath :: PapierM (Maybe FilePath)
getPdfPath = do 
  dir <- getConsumptionDir 
  liftIO $ headMay . map (dir </>) . filter (isSuffixOf ".pdf") <$> listDirectory dir

convertToPGM :: FilePath -> PapierM [FilePath]
convertToPGM pdfPath = do
  pdftoppm <- getPdftoppm 
  magick   <- getMagick 
  tmpDir   <- getTmpDir 
  imgName  <- getPrefix
  dpi      <- show <$> getDpi
  liftIO $ do 
    Exit c <- command [Cwd tmpDir] pdftoppm 
      ["-gray", "-forcenum" , "-r", dpi, pdfPath, imgName ] 
    (Exit c', Stdout files) <- command [Cwd tmpDir, Shell] "ls" [ imgName ++ "-*" ]
    let out = words files
    cs <- forM out $ \rawPgm -> do
      Exit c'' <- command [Cwd tmpDir] "unpaper" 
        [ "-v", "--overwrite", "-t", "pgm", "--dpi", dpi, rawPgm , rawPgm ]
      pure c'' 
    if all (==ExitSuccess) (c:c':cs) then 
      pure $ map (tmpDir </>) out 
    else pure []

runTesseract :: [FilePath] -> PapierM T.Text
runTesseract pgmFiles = do
  tesseract <- getTesseract 
  language  <- getLanguage
  tmpDir    <- getTmpDir 
  psm       <- show <$> getPsm 
  engine    <- show <$> getTesseractEngine 
  dpi       <- show <$> getDpi
  liftIO $ do
    res <- forM pgmFiles $ \file -> do 
      Exit c <- command [ Cwd tmpDir ] tesseract 
        [ "-l", language, "--oem", engine, "--psm", psm, "--dpi", dpi, file, file ]
      text <- T.readFile $ file ++ ".txt"
      Exit c' <- command [Cwd tmpDir , Shell] "rm" [file ++ "*"] 
      pure (c, text)
    pure $ T.concat $ map snd res
