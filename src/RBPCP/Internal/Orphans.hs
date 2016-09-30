module RBPCP.Internal.Orphans where

import RBPCP.Internal.Types
import RBPCP.Internal.Util

import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), withText, genericToJSON, genericParseJSON)

instance ToJSON Script where
    toJSON = String . cs . hexEncode

instance FromJSON Script where
    parseJSON = withText "BitcoinScript" (either fail return . hexDecode . cs)
