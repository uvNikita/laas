{-# LANGUAGE DeriveGeneric #-}

module Types (
      Request (..)
    , Response (..)
) where

import           Data.ByteString (ByteString)
import           Data.Serialize  (Serialize)
import           GHC.Generics    (Generic)

type Tar = ByteString
type PDF = ByteString

data Request = Request { mainName     :: String
                       , inputArchive :: Tar } deriving (Generic)

instance Serialize Request


data Response = Ok PDF | Error String deriving (Generic)

instance Serialize Response
