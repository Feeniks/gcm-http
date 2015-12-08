{-# LANGUAGE TemplateHaskell #-}

module Network.GCM.HTTP.Types where

import Control.Lens.TH
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Network.HTTP.Conduit (Manager)

data GCMClient = GCMClient {
    _cApiKey :: B.ByteString,
    _cManager :: Manager
}

type GCM = ReaderT GCMClient

data Message = Message {
    _messageRegistrationIDs :: [String],
    _messageCollapseKey :: Maybe String,
    _messagePriority :: Maybe MessagePriority,
    _messageDelayWhileIdle :: Bool,
    _messageTTLSecs :: Int,
    _messageRestrictedPackageName :: Maybe String,
    _messageDryRun :: Bool,
    _messageData :: M.Map String String,
    _messageNotification :: Maybe Notification
} deriving Show

data MessagePriority = Normal | High deriving Show

data Notification = Notification {
    _notificationTitle :: String,
    _notificationBody :: String,
    _notificationIcon :: String
} deriving Show

data Response = Response {
    _responseMulticastID :: Int,
    _responseSuccess :: Int,
    _responseFailure :: Int,
    _responseCanonicalIDs :: Int,
    _responseResults :: Maybe [Result]
} deriving Show

data Result = Result {
    _resultMessageID :: Maybe String,
    _resultRegistrationID :: Maybe String,
    _resultError :: Maybe String
} deriving Show

makeLenses ''GCMClient
makeLenses ''Message
makeLenses ''Notification
makeLenses ''Response
makeLenses ''Result
