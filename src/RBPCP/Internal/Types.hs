{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module RBPCP.Internal.Types
(
    module RBPCP.Internal.Types
  , module X
  , ByteString
  , Generic
)
where

import RBPCP.Internal.Util

import Network.Haskoin.Transaction            as X
import Network.Haskoin.Crypto                 as X hiding (PubKey)
import Network.Haskoin.Script                 as X
import Data.ByteString      (ByteString)
import Data.Word                              as X (Word32, Word64)
import           GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Serialize as Bin
import qualified Web.HttpApiData as Web


-- | Compressed Bitcoin public key
type PubKey = PubKeyC

-- | Bitcoin transaction ID
newtype BtcTxId = BtcTxId { btcTxId :: TxHash }
    deriving (Eq, Generic, Bin.Serialize, FromJSON, ToJSON)

instance Show BtcTxId where
    show = cs . castJsonString . toJSON . btcTxId
        where castJsonString (String s) = s
              castJsonString _ = error "BUG: JSON TxHash is not String"

instance Web.FromHttpApiData BtcTxId where
    parseUrlPiece = fmapL cs . hexDecode . cs
instance Web.ToHttpApiData BtcTxId where
    toUrlPiece = cs . hexEncode


newtype JsonHex a = JsonHex { fromHex :: a } deriving (Eq, Show, Generic, Bin.Serialize)
instance Bin.Serialize a => ToJSON (JsonHex a) where
    toJSON = String . cs . hexEncode . fromHex
instance Bin.Serialize a => FromJSON (JsonHex a) where
    parseJSON = withText "JsonHex a" $
        either (fail . (++ "Hex decode fail: ")) return .
            fmap JsonHex . hexDecode . cs

-- | Wraps any client-related datatype (eg. pubkey, signature)
newtype Client a = Client a deriving
    ( Eq
    , Show
    , Generic
    , Bin.Serialize
    , ToJSON
    , FromJSON
    , Web.ToHttpApiData
    , Web.FromHttpApiData
    )

-- | Wraps any server-related datatype (eg. pubkey, signature)
newtype Server a = Server a deriving
    ( Eq
    , Show
    , Generic
    , Bin.Serialize
    , ToJSON
    , FromJSON
    , Web.ToHttpApiData
    , Web.FromHttpApiData
    )

