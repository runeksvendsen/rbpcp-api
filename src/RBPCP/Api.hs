{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module RBPCP.Api where

import           RBPCP.Types
import           RBPCP.Internal.Types
import           Servant.API


type VER = "v2"

-- | Get information about how to fund a payment channel
type FundInfo  = VER :> "funding"  :> Capture "client_pubkey" (Client PubKey) :> Capture "exp_time" BLT :> "info"
                                   :> Get '[JSON] FundingInfo -- '[JSON, PAYREQ]

-- | Open a funded payment channel
type ChanOpen  = VER :> "channels" :> Capture "funding_txid"  BtcTxId    :> Capture "funding_vout" Vout
                                   :> QueryParam "secret"     SharedSecret
                                   :> ReqBody '[JSON] Payment            :> Verb 'POST 201 '[JSON] PaymentResult

-- | Send a payment over an open payment channel
type ChanPay   = VER :> "channels" :> Capture "funding_txid"  BtcTxId    :> Capture "funding_vout" Vout
                                   :> QueryParam "secret"     SharedSecret
                                   :> ReqBody '[JSON]         Payment    :> Put '[JSON] PaymentResult

-- | Close an open payment channel
type ChanClose = VER :> "channels" :> Capture "funding_txid"  BtcTxId    :> Capture "funding_vout" Vout
                                   :> QueryParam "secret"     SharedSecret    :> "close"
                                   :> ReqBody '[JSON] Payment            :> Put '[JSON] PaymentResult


-- | RESTful Bitcoin payment channel protocol (info, open, pay, close)
type RBPCP =
       FundInfo
  :<|> ChanOpen
  :<|> ChanPay
  :<|> ChanClose


