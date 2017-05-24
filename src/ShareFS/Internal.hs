{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ShareFS.Internal
  (
    Gateway (..)
  , initMgr
  ) where

import           Data.Aeson          (FromJSON (..), Value, withObject, (.!=),
                                      (.:), (.:?))
import           Data.Int            (Int64)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      managerConnCount, managerResponseTimeout,
                                      newManager, responseTimeoutMicro)

data Gateway = Gateway { getGWUri       :: String
                       , getGWAppKey    :: String
                       , getGWAppSecret :: String
                       , getGWTimeout   :: Int
                       , getGWConnCount :: Int
                       , getGWMgr       :: Maybe Manager
                       }

instance Show Gateway where
  show a = concat [ "uri = ", getGWUri a
                  , ", key = ", getGWAppKey a
                  , ", secret = ", getGWAppSecret a
                  ]

initMgr :: Gateway -> IO Gateway
initMgr gw = do
  mgr <- newManager defaultManagerSettings { managerConnCount = connCount
                                           , managerResponseTimeout = responseTimeoutMicro $ timeout * 1000
                                           }

  return gw { getGWMgr = Just mgr }

  where timeout = getGWTimeout gw
        connCount = getGWConnCount gw


instance FromJSON Gateway where
  parseJSON = withObject "Gateway" $ \o -> do
    getGWUri        <- o .:? "host"       .!= "https://gw.huabot.com"
    getGWAppKey     <- o .:  "key"
    getGWAppSecret  <- o .:  "secret"
    getGWTimeout    <- o .:? "timeout"    .!= 30
    getGWConnCount  <- o .:? "conn-count" .!= 10
    return Gateway{ getGWMgr = Nothing, ..}
