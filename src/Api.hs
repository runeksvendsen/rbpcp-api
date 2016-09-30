{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Api
    ( -- startApp
    ) where

import Servant

type User = String
type API = "users" :> Get '[JSON] [User]

