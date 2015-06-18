{-# LANGUAGE DeriveGeneric #-}

module Types (
      Request (..)
    , Response (..)
) where

import           Data.ByteString (ByteString)
import           Data.Serialize  (Serialize)
import           GHC.Generics (Generic)

data Request = Request { mainName :: String
                       , inputArchive  :: ByteString } deriving (Generic)

instance Serialize Request


data Response = Response { outputPdf :: ByteString } deriving (Generic)

instance Serialize Response
