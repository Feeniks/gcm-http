{-# LANGUAGE OverloadedStrings #-}

module Network.GCM.HTTP(
    module Network.GCM.HTTP.Types,
    module Data.Default.Class,
    runGCM,
    createClient,
    send
)   where

import Network.GCM.HTTP.Types
import Network.GCM.HTTP.Instances

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Default.Class
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Conduit as HTTP

runGCM :: Monad m => GCM m a -> GCMClient -> m a
runGCM = runReaderT

createClient :: MonadIO m => B.ByteString -> m GCMClient
createClient key = liftIO $ liftM (GCMClient key) (HTTP.newManager HTTP.tlsManagerSettings)

send :: (MonadThrow m, MonadIO m) => Message -> GCM m Response
send msg = do
    (GCMClient key manager) <- ask
    iReq <- liftIO $ HTTP.parseUrl "https://gcm-http.googleapis.com/gcm/send"
    let req = iReq {
        HTTP.method = "POST",
        HTTP.requestHeaders = [("Content-Type", "application/json"), ("Authorization", B.toStrict $ B.concat ["key=", key])],
        HTTP.requestBody = HTTP.RequestBodyLBS $ encode msg
    }
    liftIO (HTTP.httpLbs req manager >>= asJSON)

asJSON :: MonadIO m => HTTP.Response B.ByteString -> m Response
asJSON resp = case (decode body) of
    Nothing -> error $ B.unpack body
    Just r -> return r
    where body = HTTP.responseBody resp
