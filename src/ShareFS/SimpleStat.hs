{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ShareFS.SimpleStat
  (
    SimpleStat (..)
  , simpleStat
  ) where

import           Data.Aeson (FromJSON, ToJSON (..), object, parseJSON,
                             withObject, (.!=), (.:), (.:?), (.=))
import           Data.Int   (Int64)

data SimpleStat = SimpleStat { simpleType :: Char
                             , simpleName :: String
                             , simpleSize :: Int64
                             , simpleMode :: Int64
                             }
  deriving (Show)

simpleStat :: Char -> String -> SimpleStat
simpleStat simpleType simpleName = SimpleStat
  { simpleSize = 0
  , simpleMode = 0
  , ..
  }

instance FromJSON SimpleStat where
  parseJSON = withObject "SimpleStat" $ \o -> do
    simpleType <- o .:  "type"
    simpleName <- o .:  "name"
    simpleSize <- o .:? "size" .!= 0
    simpleMode <- o .:? "mode" .!= 0
    return SimpleStat {..}

instance ToJSON SimpleStat where
  toJSON SimpleStat{..} = object [ "type" .= simpleType
                                 , "name" .= simpleName
                                 , "size" .= simpleSize
                                 , "mode" .= simpleMode
                                 ]
