{-# LANGUAGE DeriveGeneric #-}
module RBPCP.Callback.Types where

import           RBPCP.Internal.Types
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import           GHC.Generics

data PaymentInfo = PaymentInfo
  { amount              :: BitcoinAmount
  , sender              :: SendPubKey
  , chan_value_left     :: BitcoinAmount
  , chan_total_value    :: BitcoinAmount
  } deriving Generic

data PaymentResponse = PaymentResponse
  { app_data    :: T.Text
  } deriving Generic

instance JSON.FromJSON PaymentInfo
instance JSON.ToJSON   PaymentInfo
instance JSON.FromJSON PaymentResponse
instance JSON.ToJSON   PaymentResponse
