{-# LANGUAGE OverloadedStrings #-}

module Network.GCM.HTTP.Instances where

import Network.GCM.HTTP.Types

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import Data.Default.Class
import qualified Data.Map as M

(.=?) _ Nothing = []
(.=?) k (Just v) = [(.=) k v]

instance Default Message where
    def =
        Message {
            _messageRegistrationIDs = [],
            _messageCollapseKey = Nothing,
            _messagePriority = Nothing,
            _messageDelayWhileIdle = False,
            _messageTTLSecs = 2419200,
            _messageRestrictedPackageName = Nothing,
            _messageDryRun = False,
            _messageData = M.empty,
            _messageNotification = Nothing
        }

instance ToJSON Message where
    toJSON m =
        object $ [
            "registration_ids" .= _messageRegistrationIDs m,
            "delay_while_idle" .= _messageDelayWhileIdle m,
            "time_to_live" .= _messageTTLSecs m,
            "dry_run" .= _messageDryRun m,
            "data" .= _messageData m
        ]
        ++ ("collapse_key" .=? _messageCollapseKey m)
        ++ ("priority" .=? fmap (fmap toLower . show) (_messagePriority m))
        ++ ("restricted_package_name" .=? _messageRestrictedPackageName m)
        ++ ("notification" .=? _messageNotification m)

instance ToJSON Notification where
    toJSON n =
        object [
            "title" .= _notificationTitle n,
            "body" .= _notificationBody n,
            "icon" .= _notificationIcon n
        ]

instance FromJSON Response where
    parseJSON (Object v) = do
        mcID <- v .: "multicast_id"
        success <- v .: "success"
        failure <- v .: "failure"
        canonicalIDs <- v .: "canonical_ids"
        results <- v .:? "results"
        return $ Response mcID success failure canonicalIDs results
    parseJSON _ = mzero

instance FromJSON Result where
    parseJSON (Object v) = Result <$> v .:? "message_id" <*> v .:? "registration_id" <*> v .:? "error"
    parseJSON _ = mzero
