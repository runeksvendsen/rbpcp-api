{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module RBPCP.Internal.Types
(
    module RBPCP.Internal.Types
  , module Network.Haskoin.Transaction
  , module Network.Haskoin.Crypto
  , module Network.Haskoin.Script
  , module Data.Word
  , ByteString
  , Generic
)
where

import RBPCP.Internal.Util

import Network.Haskoin.Transaction
import Network.Haskoin.Crypto hiding (PubKey)
import Network.Haskoin.Script
import Data.ByteString      (ByteString)
import Data.Word            (Word32, Word64)
import           GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Serialize as Bin


type PubKey = PubKeyC

data JsonHex a = JsonHex { fromHex :: a } deriving (Eq, Show, Generic, Bin.Serialize)
instance (Eq a, Show a, Bin.Serialize a) => ToJSON (JsonHex a) where
    toJSON = String . cs . hexEncode . fromHex
instance (Eq a, Show a, Bin.Serialize a) => FromJSON (JsonHex a) where
    parseJSON = withText "JsonHex a" $
        either (fail . (++ "Hex decode fail: ")) return .
            fmap JsonHex . hexDecode . cs

-- | Wraps any client-related datatype (eg. pubkey, signature)
data Client a = Client a deriving (Eq, Show, Generic, Bin.Serialize, ToJSON, FromJSON)

-- | Wraps any server-related datatype (eg. pubkey, signature)
data Server a = Server a deriving (Eq, Show, Generic, Bin.Serialize, ToJSON, FromJSON)

