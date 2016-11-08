{-# LANGUAGE DeriveGeneric #-}
module RBPCP.Callback.Types where

import           RBPCP.Internal.Types
import qualified Data.Text as T
import qualified Data.Aeson as JSON
import           GHC.Generics

data PaymentInfo = PaymentInfo
  { amount              :: BitcoinAmount
  , chan_value_left     :: BitcoinAmount
  , chan_total_value    :: BitcoinAmount
  , client_app_data     :: T.Text
  , full_payment        :: FullPayment
  } deriving Generic

data PaymentResponse = PaymentResponse
  { resp_app_data       :: T.Text
  } deriving Generic

instance JSON.FromJSON PaymentInfo
instance JSON.ToJSON   PaymentInfo
instance JSON.FromJSON PaymentResponse
instance JSON.ToJSON   PaymentResponse
