{-# LANGUAGE OverloadedStrings , LambdaCase #-}
module Papierlos.OCR.Consume where

import Papierlos.Common.Types

import Control.Monad.Reader
import System.Command
import System.Directory
import System.Exit
import System.FilePath
import Data.List (isSuffixOf, inits) 
import Data.Time.Clock
import Safe (headMay)
import Database.SQLite.Simple
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Text.IO as T

startConsume :: Config -> IO ()
startConsume = runReaderT consume

consume :: PapierM () 
consume =
  getPdfName >>= \case
    Nothing -> liftIO (threadDelay 10000000) >> consume
    Just pdf -> do
      content <- convertToPGM >=> runTesseract $ pdf
      path <- movePdf pdf
      db <- asks $ database . general
      liftIO $ withConnection db $ \conn -> do
        date <- getCurrentTime
        let 
          name = takeFileName path
          doc = Document Nothing name content path "thumbnail" date 
        execute conn "INSERT INTO documents VALUES (?,?,?,?,?,?)" doc
      pure ()

movePdf :: FilePath -> PapierM FilePath 
movePdf pdf = undefined

createThumbnail :: FilePath -> PapierM FilePath
createThumbnail pdf = undefined

getPdfName :: PapierM (Maybe FilePath)
getPdfName = do 
  dir <- asks $ consumptionDir . general
  liftIO $ headMay . map (dir </>) . filter (isSuffixOf ".pdf") <$> listDirectory dir

convertToPGM :: FilePath -> PapierM [FilePath]
convertToPGM pdfPath = do
  pdftoppm <- asks $ pdftoppm . programs
  magick <- asks $ magick . programs
  tmpDir <- asks $ tmpDir . general
  let imgName = "papierlos"
  liftIO $ do 
    Exit c <- command [Cwd tmpDir] pdftoppm ["-gray", "-forcenum" , pdfPath, imgName ] 
    (Exit c', Stdout files) <- command [Cwd tmpDir, Shell] "ls" [ imgName ++ "-*" ]
    let out = words files
    cs <- forM out $ \rawPgm -> do
      Exit c'' <- command [Cwd tmpDir] "unpaper" [ "-v", "-overwrite", rawPgm , rawPgm ]
      pure c'' 
    if all (==ExitSuccess) (c:c':cs) then 
      pure $ map (tmpDir </>) out 
    else pure []

runTesseract :: [FilePath] -> PapierM T.Text
runTesseract pgmFiles = do
  tesseract <- asks $ tesseract . programs
  language <- asks $ language . general
  psm <- asks $ show . psm . general
  engine <- asks $ show . tesseractEngine . general 
  tmpDir <- asks $ tmpDir . general
  liftIO $ do
    res <- forM pgmFiles $ \file -> do 
      Exit c <- command [ Cwd tmpDir ] tesseract [ "-l", language, "--oem", engine, "--psm", psm, file, file ]
      text <- T.readFile $ file ++ ".txt"
      Exit c' <- command [Cwd tmpDir , Shell] "rm" [file ++ "*"] 
      pure (c, text)
    pure $ T.concat $ map snd res
