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
import Data.Time.Format.ISO8601
import Safe (headMay)
import Control.Concurrent
import Database.Selda (def, ID)
import Servant.Server (Handler)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.IO as T

startConsume :: Config -> Handler ()
startConsume = flip runPapierM consume

serveFile :: T.Text -> PapierM T.Text
serveFile path = liftIO $ T.decodeUtf8 . B64.encode <$> B.readFile (T.unpack path)

consume :: PapierM () 
consume =
  getPdfPath >>= \case
    Nothing  -> pure ()
    Just pdf -> do
      content   <- convertToPGM >=> runTesseract $ pdf
      date      <- liftIO getCurrentTime
      
      thumbnailFileId <- createThumbnail date pdf
      documentFileId  <- movePdf date pdf

      void $ insertDocument $ 
        Document def (T.pack $ "document-" ++ iso8601Show date) 
          content documentFileId thumbnailFileId date 

movePdf :: ISO8601 t => t -> FilePath -> PapierM (ID DocumentFile) 
movePdf date pdf = do
  storageDir <- getStorageDir
  let
    newName = takeBaseName pdf ++ iso8601Show date ++ takeExtension pdf
    newPath = storageDir </> newName 
  liftIO $ do
    Exit c <- command [] "mv" [pdf, newPath]
    pure c -- TODO catch or do something in case of failure 
  insertDocumentFile $ DocumentFile def $ T.pack newPath
    
createThumbnail :: ISO8601 t => t -> FilePath -> PapierM (ID ThumbnailFile)
createThumbnail date pdfPath = do
  storageDir <- getStorageDir
  pdfimages  <- getPdfimages
  path <- liftIO $ do 
    let outName = "thumbnail" ++ iso8601Show date
    Exit c <- command [ Cwd storageDir ] pdfimages 
      ["-f", "1", "-l", "1", "-png", pdfPath, outName ]
    pure $ storageDir </> (outName ++ "-000.png")    
  insertThumbnailFile $ ThumbnailFile def $ T.pack path

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
