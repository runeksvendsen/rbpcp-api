{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module API.Internal.Callback.AppData where

import           API.Internal.Callback.Types
import           Servant.API


-- |A server implementing this API can be contacted by this server,
--  and receive information about all incoming payments.
--  The response delivered to this request can contain arbitrary application data,
--  which will be included in the PaymentResult response delivered to the paying client.
type AppDataCallback =
    "get_appdata" :> ReqBody '[JSON] PaymentInfo :> Post '[JSON] ApplicationData


