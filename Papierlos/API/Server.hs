{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Papierlos.API.Server where

import Papierlos.Common.Types
import Papierlos.Common.Database
import Papierlos.OCR.Consume

import Control.Monad
import Control.Concurrent
import Control.Monad.Reader
import Data.Functor (void)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text.Encoding as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Safe
import Servant
import WaiAppStatic.Types
import WaiAppStatic.Storage.Filesystem

api :: Proxy API
api = Proxy

type API = GETDocuments 
  :<|> GETDocumentById
  :<|> GETThumbnailById
  :<|> GETDocumentFileById
  :<|> POSTDocumentAddTag
  :<|> GETDocumentsByTag
  :<|> GETTags
  :<|> POSTCreateTag
  :<|> Raw

type GETDocuments = "documents" :> Get '[JSON] [Document] 
type GETDocumentById = "documents" :> Capture "docId" Int :> Get '[JSON] (Maybe Document)
type GETThumbnailById = "documents" :> "thumbnail" :> Capture "docId" Int :> Get '[JSON] (Maybe T.Text)
type GETDocumentFileById = "documents" :> "file" :> Capture "docId" Int :> Get '[JSON] (Maybe T.Text)
type POSTDocumentAddTag = "documents" :> "addtag" :> Capture "docId" Int :> ReqBody '[JSON] ClientTag :> Post '[JSON] Int
type GETDocumentsByTag = "documents" :> "tags" :> Capture "tagName" T.Text :> Get '[JSON] [Document]
type GETTags = "tags" :> Get '[JSON] [Tag]
type POSTCreateTag = "tags" :> "create" :> ReqBody '[JSON] ClientTag :> Post '[JSON] Int

application :: Config -> Application 
application cf = serve api $ 
  hoistServer api (runPapierM cf . (initializeDB >>)) appApi

runServer :: Config -> IO () 
runServer cf = do
  print "starting server..."
  forkIO $ do
    runHandler $ runPapierM cf consume
    threadDelay 1000000
  run 3000 $ middleware . application $ cf 

middleware :: Application -> Application
middleware = logStdoutDev 

appApi :: ServerT API PapierM
appApi = getDocumentsApi
  :<|> getDocumentByIdApi
  :<|> getThumbnailByIdApi
  :<|> getDocumentFileByIdApi
  :<|> addTagApi
  :<|> getDocumentsByTagApi
  :<|> getTagsApi
  :<|> createTagApi
  :<|> static

getDocumentsByTagApi :: ServerT GETDocumentsByTag PapierM
getDocumentsByTagApi = getDocumentsByTag 

getDocumentByIdApi :: ServerT GETDocumentById PapierM
getDocumentByIdApi = getDocumentById

getThumbnailByIdApi :: ServerT GETThumbnailById PapierM
getThumbnailByIdApi = mapM serveFile <=< getThumbnailById 

getDocumentFileByIdApi :: ServerT GETDocumentFileById PapierM
getDocumentFileByIdApi = mapM serveFile <=< getDocumentFileById 

getDocumentsApi :: ServerT GETDocuments PapierM
getDocumentsApi = getDocuments Nothing 

getTagsApi :: ServerT GETTags PapierM
getTagsApi = getTags

createTagApi :: ServerT POSTCreateTag PapierM
createTagApi = fmap fromId . insertTag . toTag

addTagApi :: ServerT POSTDocumentAddTag PapierM
addTagApi docId = addTagToDocument (toId docId) <=< insertTag . toTag

static :: ServerT Raw PapierM
static = serveDirectoryWith $ 
  (defaultWebAppSettings "static/") 
    { ssIndices = map unsafeToPiece ["index.html", "index.htm"] }
