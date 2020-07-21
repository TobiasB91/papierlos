{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO 
import qualified Data.Text.IO as T

import Papierlos.Config.Parser
import Papierlos.OCR.Consume
import Papierlos.API.Server

import Control.Monad.Reader
import Control.Concurrent

main :: IO ()
main = 
  parseConfig <$> T.readFile "/etc/papierlos.conf" >>= \case 
    Left err     -> hPutStrLn stderr err 
    Right config -> do 
      runServer config
      undefined
