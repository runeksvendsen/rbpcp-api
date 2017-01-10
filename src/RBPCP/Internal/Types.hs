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

data AsHex a = AsHex { asHex :: a } deriving (Eq, Show)
instance (Eq a, Show a, Bin.Serialize a) => ToJSON (AsHex a) where
    toJSON = String . cs . hexEncode . asHex
instance (Eq a, Show a, Bin.Serialize a) => FromJSON (AsHex a) where
    parseJSON = withText "AsHex a" $ either fail return . fmap AsHex . hexDecode . cs

-- | Wraps any client-related datatype (eg. pubkey, signature)
data Client a = Client a deriving (Eq, Show, Generic, Bin.Serialize, ToJSON, FromJSON)

-- | Wraps any server-related datatype (eg. pubkey, signature)
data Server a = Server a deriving (Eq, Show, Generic, Bin.Serialize, ToJSON, FromJSON)

