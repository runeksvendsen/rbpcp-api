{-# LANGUAGE DeriveGeneric #-}
module RBPCP.Callback.Types where

import qualified Data.ByteString as BS
import           GHC.Generics

type PaymentInfo = ()

data ApplicationData = AppData
  { app_data    :: BS.ByteString
  } deriving Generic
