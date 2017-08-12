{-# LANGUAGE OverloadedStrings #-}
module ShareFS.Wreq
  (
    getOptions
  , getOptionsAndSign
  , getOptionsAndSignRaw
  , responseEither
  , responseEither'
  , responseEitherJSON
  , tryResponse
  ) where

import           Control.Exception      (try)
import           Control.Lens           ((&), (.~), (^.))
import           Data.Aeson             (FromJSON)
import qualified Data.ByteString.Char8  as B (ByteString, pack)
import qualified Data.ByteString.Lazy   as LB (ByteString)
import qualified Data.Text.Lazy         as LT (Text, pack)
import           Data.UnixTime
import           Network.HTTP.Client    (HttpException (..),
                                         HttpExceptionContent (..), Manager)
import           Network.Wreq           (Options, Response, asJSON, defaults,
                                         header, manager, responseBody)
import           ShareFS.Internal       (Gateway (..))
import           Yuntan.Utils.Signature (signParams, signRaw)


getMgr :: Maybe Manager -> Options
getMgr Nothing    = defaults
getMgr (Just mgr) = defaults & manager .~ Right mgr

getOptions :: Gateway -> Options
getOptions (Gateway { getGWAppKey = key, getGWMgr = mgr }) =
  getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
             & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]

getOptionsAndSign :: [(LT.Text, LT.Text)] -> Gateway -> IO Options
getOptionsAndSign params (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signParams (B.pack sec) (("timestamp", LT.pack t):("key", LT.pack key):params)
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]
  return opts

getOptionsAndSignRaw :: String -> B.ByteString -> Gateway -> IO Options
getOptionsAndSignRaw path dat (Gateway { getGWAppKey = key, getGWAppSecret = sec, getGWMgr = mgr }) = do
  t <- show . toEpochTime <$> getUnixTime
  let sign = signRaw (B.pack sec) [ ("key", B.pack key)
                                  , ("timestamp", B.pack t)
                                  , ("raw", dat)
                                  , ("sign_path", B.pack path)
                                  ]
      opts = getMgr mgr & header "X-REQUEST-KEY" .~ [B.pack key]
                        & header "X-REQUEST-SIGNATURE" .~ [sign]
                        & header "X-REQUEST-TIME" .~ [B.pack t]
                        & header "User-Agent" .~ ["haskell dispatch-base-0.1.0.0"]
  return opts


tryResponse :: IO (Response a) -> IO (Either String (Response a))
tryResponse req = do
  e <- try req
  case e of
    Left (HttpExceptionRequest _ content) -> do
      case content of
        (StatusCodeException code _) -> return . Left $ "StatusCodeException " ++ show code
        ResponseTimeout -> return $ Left "ResponseTimeout"
        other -> return . Left $ show other

    Left (InvalidUrlException _ _) -> do
      return $ Left "InvalidUrlException"
    Right r  -> return $ Right r

responseEither :: IO (Response a) -> IO (Either String a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

responseEither' :: IO (Response LB.ByteString) -> IO (Either String ())
responseEither' req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

responseEitherJSON :: FromJSON a => IO (Response LB.ByteString) -> IO (Either String a)
responseEitherJSON req = responseEither $ asJSON =<< req
