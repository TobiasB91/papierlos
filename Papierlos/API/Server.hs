{-# LANGUAGE OverloadedStrings #-}

module Papierlos.API.Server where

import Papierlos.Common.Types
import Papierlos.Common.Database

import Web.Scotty
import Control.Monad.Reader
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B
import Data.Aeson hiding (json)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL (fromStrict)
import qualified Data.Text.Encoding as T
import Network.Wai.Middleware.Static hiding (defaultOptions)

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
