{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module RBPCP.Callback.Types where

import           RBPCP.Internal.Types
import qualified Data.Text as T
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics


data CallbackInfo = CallbackInfo
  { amount              :: BitcoinAmount
  , chan_value_left     :: BitcoinAmount
  , chan_total_value    :: BitcoinAmount
  , client_app_data     :: T.Text
  , full_payment        :: FullPayment
  } deriving (Generic, FromJSON, ToJSON)

data CallbackResponse = CallbackResponse
  { resp_app_data       :: T.Text
  , resp_app_error      :: Maybe T.Text
  } deriving (Generic, FromJSON, ToJSON)
