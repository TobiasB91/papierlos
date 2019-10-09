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
      startConsume config
      undefined
