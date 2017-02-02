{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module RBPCP.Callback.Types where

import           RBPCP.Types
import           RBPCP.Internal.Types
import qualified Data.Text as T
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics


data CallbackInfo = CallbackInfo
  { value_received      :: Word64
  , chan_value_left     :: Word64
  , chan_total_capacity :: Word64
  , full_payment        :: Payment
  , app_data            :: T.Text
  } deriving (Generic, FromJSON, ToJSON)

data CallbackResponse = CallbackResponse
  { resp_app_data       :: T.Text
  , resp_app_error      :: Maybe T.Text
  } deriving (Generic, FromJSON, ToJSON)
