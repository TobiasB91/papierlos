{-# LANGUAGE OverloadedStrings #-}

module Papierlos.Common.Database where

import Papierlos.Common.Types
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Command

import Control.Monad.Reader

instance FromRow Document where
  fromRow = Document 
    <$> field <*> field  
    <*> field <*> field
    <*> field <*> (read <$> field)

instance ToRow Document where
  toRow (Document a b c d e f) = toRow (a, b, c, d, e, f)

queryDatabase :: (Connection -> IO a) -> PapierM a
queryDatabase cb = getDatabase >>= \db -> liftIO $ withConnection db cb

insertDocument :: Document -> PapierM ()
insertDocument doc = queryDatabase $ \conn -> 
    execute conn "INSERT INTO documents VALUES (?,?,?,?,?,?)" doc

countDocuments :: PapierM Int
countDocuments = queryDatabase $ \conn -> do
    [Only count] <- query_ conn "SELECT COUNT(*) FROM documents" :: IO [Only Int]
    pure count 

maxDocumentsId :: PapierM Int
maxDocumentsId = queryDatabase $ \conn -> do
    [Only maxId] <- query_ conn "SELECT MAX(id) FROM documents" :: IO [Only Int]
    pure maxId 

deleteDocumentById :: Int -> PapierM ()
deleteDocumentById i = queryDatabase $ \conn -> do
    xs <- query conn "SELECT thumbnail , path FROM documents WHERE id = ?" 
      (Only i) :: IO [(String, String)]
    execute conn "DELETE FROM documents WHERE id = ?" (Only i)
    unless (null xs) $ do
      let [(thumbnail, path)] = xs
      Exit c <- command [] "rm" [ thumbnail , path ] 
      pure ()

getDocuments :: Maybe (Int, Int) -> PapierM [Document]
getDocuments limit = queryDatabase $ \conn ->
    case limit of 
      Nothing    -> query_ conn "SELECT * FROM documents"
      Just (s,o) -> query conn "SELECT * FROM documents LIMIT ?,?" (s,o)

getDocumentById :: Int -> PapierM (Maybe Document)
getDocumentById i = queryDatabase $ \conn -> do
  xs <- query conn "SELECT * FROM documents WHERE id = ?" (Only i)
  if null xs then pure Nothing
  else pure . pure . head $ xs
