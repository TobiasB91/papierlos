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
  :<|> Raw

type GETDocuments = "documents" :> Get '[JSON] [Document] 
type GETDocumentById = "documents" :> Capture "userid" Int :> Get '[JSON] (Maybe Document)
type GETThumbnailById = "documents" :> "thumbnail" :> Capture "userid" Int :> Get '[JSON] (Maybe T.Text)
type GETDocumentFileById = "documents" :> "file" :> Capture "userid" Int :> Get '[JSON] (Maybe T.Text)

application :: Config -> Application 
application cf = serve api $ 
  hoistServer api (runPapierM cf . (initialize >>)) appApi where
    initialize = sequence 
      [  createDocumentsTable  
      ,  createDocumentFilesTable
      ,  createThumbnailFilesTable
      -- ,  consume 
      ]

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
  :<|> static

getDocumentByIdApi :: ServerT GETDocumentById PapierM
getDocumentByIdApi = getDocumentById

getThumbnailByIdApi :: ServerT GETThumbnailById PapierM
getThumbnailByIdApi = mapM serveFile <=< getThumbnailById 

getDocumentFileByIdApi :: ServerT GETDocumentFileById PapierM
getDocumentFileByIdApi = mapM serveFile <=< getDocumentFileById 

getDocumentsApi :: ServerT GETDocuments PapierM
getDocumentsApi = getDocuments Nothing 

static :: ServerT Raw PapierM
static = serveDirectoryWith $ 
  (defaultWebAppSettings "static/") 
    { ssIndices = map unsafeToPiece ["index.html", "index.htm"] }

{-
startServer :: Config -> IO () 
startServer cf = do
  runPapierM cf createDocumentsTable
  scotty 3000 $ do
    middleware (staticPolicy $ addBase "static")

    get "/" $ file "./static/index.html" 

    get "/documents" $ do
      docs <- liftAndCatchIO $
        runPapierM cf $ getDocuments Nothing
      json docs

    get "/documents/limit/:offset/:size" $ do
      offset <- param "offset"
      size   <- param "size"
      docs <- liftAndCatchIO $
        runPapierM cf $ getDocuments $ Just (offset,size)
      json docs

    get "/documents/count" $ do 
      c <- liftAndCatchIO  $ runPapierM cf countDocuments
      json c

    get "/documents/:id" $ do
      docId <- param "id"
      jDoc  <- liftAndCatchIO $
        runPapierM cf (getDocumentById docId) 
      json jDoc 

    get "/documents/search/:query" $ do
      txt  <- param "query" 
      docs <- liftAndCatchIO $ 
        runPapierM cf $ searchDocs txt ContainsText
      json docs

    post "/tags/create" $ 
      params >>= liftIO . print >> redirect "/" 

-}
