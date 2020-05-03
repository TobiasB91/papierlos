{-# LANGUAGE OverloadedStrings, OverloadedLabels, LambdaCase #-}
module Papierlos.Common.Database where

import Database.Selda
import Papierlos.Common.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Maybe (listToMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import Control.Monad.Reader

documents :: Table Document
documents = table "documents" [ #document_id :- autoPrimary ]

createDocumentsTable :: PapierM ()
createDocumentsTable = tryCreateTable documents

insertDocument :: Document -> PapierM Int 
insertDocument = fmap fromId . insertWithPK documents . pure 

countDocuments :: PapierM Int
countDocuments = head <$> query countDocs where
  countDocs = aggregate $
    select documents >>= \docs -> pure $ count (docs ! #document_id)

updateDocumentName :: Int -> T.Text -> PapierM ()
updateDocumentName = updateDocumentById #document_name 

updateDocumentPath:: Int -> T.Text -> PapierM ()
updateDocumentPath = updateDocumentById #document_pdf

updateDocumentThumbnail :: Int -> T.Text -> PapierM ()
updateDocumentThumbnail = updateDocumentById #document_thumbnail

updateDocumentById :: (MonadSelda m, SqlType a) => Selector Document a -> Int -> a -> m ()
updateDocumentById sel pk val = update_ documents
  (is #document_id (toId pk)) $ \doc -> doc `with` [sel := literal val] 

deleteDocumentById :: Int -> PapierM (Maybe Document)
deleteDocumentById pk = do
  doc <- getDocumentById pk
  deleteFrom_ documents $ is #document_id (toId pk) 
  pure doc

getDocuments :: Maybe (Int, Int) -> PapierM [Document]
getDocuments = mapM fillInFiles <=< \case 
  Nothing            -> query (select documents)
  Just (offset,size) -> query (limit offset size $ select documents)

getDocumentById :: Int -> PapierM (Maybe Document)
getDocumentById pk = fillIn =<< query queryDoc where
  queryDoc = do
    docs <- select documents
    restrict (docs ! #document_id .== literal (toId pk))
    pure docs 

  fillIn []    = pure Nothing 
  fillIn (d:_) = pure <$> fillInFiles d

fillInFiles :: Document -> PapierM Document
fillInFiles doc = liftIO $ do
  pdf       <- T.decodeUtf8 . B64.encode <$> B.readFile (T.unpack $ document_pdf doc)
  thumbnail <- T.decodeUtf8 . B64.encode <$> B.readFile (T.unpack $ document_thumbnail doc)
  pure $ doc { document_pdf = pdf, document_thumbnail = thumbnail } 

searchDocs :: T.Text -> SearchAlgorithm -> PapierM [Document]
searchDocs t = mapM fillInFiles <=< query . \case 
  ContainsText -> cnts
    where
      cnts = do
        docs <- select documents
        restrict $ 
          (docs ! #document_content) `like` text ("%" <> t <> "%") .||
          (docs ! #document_name) `like` text ("%" <> t <> "%")
        pure docs
