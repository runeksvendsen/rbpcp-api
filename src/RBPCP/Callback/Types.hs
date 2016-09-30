{-# LANGUAGE DeriveGeneric #-}
module RBPCP.Callback.Types where

import           RBPCP.Internal.Types
import qualified Data.ByteString as BS
import           GHC.Generics

data PaymentInfo = PaymentInfo
  { value                   :: BitcoinAmount
  , remaining_channel_value :: BitcoinAmount
  }

data PaymentResponse = PaymentResponse
  { app_data    :: BS.ByteString
  } deriving Generic
