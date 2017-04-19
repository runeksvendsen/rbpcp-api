module RBPCP.Internal.Orphans where

import RBPCP.Internal.Types
import RBPCP.Internal.Util
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), withText, genericToJSON, genericParseJSON)
import qualified Data.Aeson as JSON
import qualified Web.HttpApiData as Web


instance ToJSON Script where
    toJSON = String . cs . hexEncode

instance FromJSON Script where
    parseJSON = withText "BitcoinScript" (either fail return . hexDecode . cs)

instance ToJSON Hash256 where
    toJSON = String . cs . hexEncode

instance FromJSON Hash256 where
    parseJSON = withText "Hash256" (either fail return . hexDecode . cs)

instance Web.FromHttpApiData Hash256 where
    parseUrlPiece = fmapL cs . hexDecode . cs
instance Web.ToHttpApiData Hash256 where
    toUrlPiece = cs . hexEncode

