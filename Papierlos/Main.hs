{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO 
import qualified Data.Text.IO as T

import Papierlos.Config.Parser
import Papierlos.OCR.Consume

import Control.Monad.Reader

main :: IO ()
main = do 
  putStrLn "Hello, Haskell!"
  parseConfig <$> T.readFile "/etc/papierlos.conf" >>= \case 
    Left err     -> hPutStrLn stderr err 
    Right config -> do 
      runReaderT (do 
        consume
        undefined
        files <- convertToPGM "/home/tobr/projectTest/Scan.pdf"
        out <- runTesseract files
        liftIO $ print out) config
      print config --startConsume config 
