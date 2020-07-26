{-# LANGUAGE OverloadedStrings, OverloadedLabels, LambdaCase, FlexibleContexts #-}
module Papierlos.Common.Database 
  ( module Papierlos.Common.Database 
  , fromId 
  , toId
  ) where

import Database.Selda
import Papierlos.Common.Types
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Safe

import Control.Monad.Reader

documents :: Table Document
documents = table "documents" [ #document_id :- autoPrimary ]

thumbnailFiles :: Table ThumbnailFile
thumbnailFiles = table "thumbnailFiles" [ #thumbnailFile_id :- autoPrimary ]

documentFiles :: Table DocumentFile
documentFiles = table "documentFiles" [ #documentFile_id :- autoPrimary ]

tags :: Table Tag
tags = table "tags" [ #tag_id :- autoPrimary , #tag_name :- unique ]

docToTags :: Table DocToTag
docToTags = table "docToTags" [ (#dtt_document_id :+ #dtt_tag_id) :- primary ] 

initializeDB :: PapierM ()
initializeDB = sequence_ 
  [ createDocumentsTable
  , createDocumentFilesTable
  , createThumbnailFilesTable
  , createTagsTable
  , createDocToTagsTable
  ]

createDocumentsTable :: PapierM ()
createDocumentsTable = tryCreateTable documents

createThumbnailFilesTable :: PapierM ()
createThumbnailFilesTable = tryCreateTable thumbnailFiles

createDocumentFilesTable :: PapierM ()
createDocumentFilesTable = tryCreateTable documentFiles

createTagsTable :: PapierM ()
createTagsTable = tryCreateTable tags

createDocToTagsTable :: PapierM ()
createDocToTagsTable = tryCreateTable docToTags

allDocuments :: Query s (Row s Document)
allDocuments = select documents

insertDocument :: Document -> PapierM (ID Document) 
insertDocument = insertWithPK documents . pure 

insertThumbnailFile :: ThumbnailFile -> PapierM (ID ThumbnailFile)
insertThumbnailFile = insertWithPK thumbnailFiles . pure

insertDocumentFile :: DocumentFile -> PapierM (ID DocumentFile)
insertDocumentFile = insertWithPK documentFiles . pure

insertTag :: Tag -> PapierM (ID Tag)
insertTag t@(Tag tId tName tColor) = do
  tryInsert tags [t]
  res <- query $ do
    t' <- select tags
    restrict $ t' ! #tag_name .== literal tName
    pure t'
  pure . tag_id . head $ res 

countDocuments :: PapierM Int
countDocuments = head <$> query countDocs where
  countDocs = aggregate $
    select documents >>= \docs -> pure $ count (docs ! #document_id)

getDocuments :: Maybe (Int, Int) -> PapierM [Document]
getDocuments = query . \case 
  Nothing            -> allDocuments 
  Just (offset,size) -> limit offset size allDocuments

getTags :: PapierM [Tag]
getTags = query $ select tags

addTagToDocument :: ID Document -> ID Tag -> PapierM Int
addTagToDocument docId = insert docToTags . pure . DocToTag docId

getTagsForDocument :: Int -> PapierM [Tag]
getTagsForDocument dId = query $ do
  docs <- allDocuments
  tags <- select tags
  dtts <- select docToTags
  restrict $ 
    docs ! #document_id .== literal (toId dId) .&&
    docs ! #document_id .== dtts ! #dtt_document_id .&&
    tags ! #tag_id .== dtts ! #dtt_tag_id
  pure tags


getDocumentsByTag :: T.Text -> PapierM [Document]
getDocumentsByTag tName = query $ do
  docs <- allDocuments 
  tags <- select tags
  dtts <- select docToTags
  restrict $ 
    docs ! #document_id .== dtts ! #dtt_document_id .&&
    tags ! #tag_id .== dtts ! #dtt_tag_id .&&
    tags ! #tag_name .== literal tName
  pure docs

getDocumentById :: Int -> PapierM (Maybe Document)
getDocumentById pk = headMay <$> query queryDoc where
  queryDoc = do
    docs <- allDocuments
    restrict (docs ! #document_id .== literal (toId pk))
    pure docs 

getThumbnailById :: Int -> PapierM (Maybe T.Text)
getThumbnailById pk = headMay <$> query 
  (fileQuery pk thumbnailFiles #thumbnailFile_id #document_thumbnail #thumbnailFile_path)   

getDocumentFileById :: Int -> PapierM (Maybe T.Text)
getDocumentFileById pk = headMay <$> query 
  (fileQuery pk documentFiles #documentFile_id #document_pdf #documentFile_path)


fileQuery pk fTable pKeyF fKeyF pF = do
  fKey <- from fKeyF $ do 
    docs <- allDocuments
    restrict (docs ! #document_id .== literal (toId pk))
    pure docs
  fRows <- select fTable 
  restrict (fRows ! pKeyF .== fKey) 
  pure $ fRows ! pF


{-
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

-}
