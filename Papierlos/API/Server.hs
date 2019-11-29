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

instance ToJSON DocumentJSON where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON B.ByteString where
  toJSON = String . T.decodeUtf8 . B.encode

docToDocJson :: Document -> IO DocumentJSON
docToDocJson doc = do
  let
    id       = fromMaybe 0 $ docId doc
    content' = content doc
    name     = fileName doc
    date'    = date doc
  pdf        <- B.readFile (filePath doc)
  thumbnail  <- B.readFile (thumbnail doc)
  pure $ DocumentJSON id name content'
    pdf thumbnail date'


startServer :: Config -> IO () 
startServer cf = scotty 3000 $ do
  middleware (staticPolicy $ addBase "static")

  get "/" $ file "./static/index.html" 

  get "/documents" $ do
    docs <- liftAndCatchIO $
      runPapierM cf (getDocuments Nothing) >>=
        mapM docToDocJson
    json docs

  get "/documents/:id" $ do
    docId <- param "id"
    jDoc  <- liftAndCatchIO $
      runPapierM cf (getDocumentById docId) >>=
        docToDocJson . fromJust
    json jDoc 

  get "/documents/search/:query" undefined

  post "/tags/create" $ 
    params >>= liftIO . print >> redirect "/" 
