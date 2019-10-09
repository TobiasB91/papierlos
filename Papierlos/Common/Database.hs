{-# LANGUAGE OverloadedStrings #-}

module Papierlos.Common.Database where

import Papierlos.Common.Types
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Control.Monad.Reader

instance FromRow Document where
  fromRow = Document <$> field 
    <*> field <*> field 
    <*> field <*> field
    <*> field

instance ToRow Document where
  toRow (Document a b c d e f) = toRow (a, b, c, d, e, f)

insertDocument :: Document -> PapierM ()
insertDocument doc = do 
  db <- getDatabase 
  liftIO $ withConnection db $ \conn -> 
    execute conn "INSERT INTO documents VALUES (?,?,?,?,?,?)" doc

countDocuments :: PapierM Int
countDocuments = do
  db <- getDatabase 
  liftIO $ withConnection db $ \conn -> do
    [Only count] <- query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
    pure count 
