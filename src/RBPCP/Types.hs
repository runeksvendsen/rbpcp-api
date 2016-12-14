{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RBPCP.Types where

import           RBPCP.Internal.Types hiding (Payment)
import           RBPCP.Internal.Util
import           RBPCP.Internal.Orphans ()
import           Data.Aeson
import qualified Data.Serialize as Bin
import           Data.Word                          (Word32)

-- Generated
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), genericToJSON, genericParseJSON, withText)
import Data.Aeson.Types (Options(..), defaultOptions, camelTo2)
import Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.Function ((&))


type Vout  = Word32
type Hours = Word
type BtcConf = Word


data ChannelStatus = ChannelOpen | ChannelClosed deriving (Show, Eq)
instance FromJSON ChannelStatus where
    parseJSON = withText "ChannelStatus" $
            \s -> case s of
                "open"   -> return ChannelOpen
                "closed" -> return ChannelClosed
                e        -> fail $ "expected \"open\" or \"closed\", not: " ++ show (cs e :: String)
instance ToJSON ChannelStatus where
  toJSON ChannelOpen = String "open"
  toJSON ChannelClosed = String "closed"
instance Bin.Serialize ChannelStatus where
    put ChannelOpen = Bin.putWord8 0x01
    put ChannelClosed = Bin.putWord8 0x02
    get = Bin.getWord8 >>= \w -> case w of
            0x01 -> return ChannelOpen
            0x02 -> return ChannelClosed
            n    -> fail $ "expected 1 or 2, not: " ++ show n

data ErrorType = PaymentError | ApplicationError deriving (Show, Eq)
instance FromJSON ErrorType where
    parseJSON = withText "ErrorType" $
            \s -> case s of
                "payment_error"   -> return PaymentError
                "application_error" -> return ApplicationError
                e        -> fail $ "expected \"payment_error\"" ++
                                   " or \"application_error\", not: " ++ show (cs e :: String)
instance ToJSON ErrorType where
  toJSON PaymentError = String "payment_error"
  toJSON ApplicationError = String "application_error"
instance Bin.Serialize ErrorType where
    put PaymentError = Bin.putWord8 0x01
    put ApplicationError = Bin.putWord8 0x02
    get = Bin.getWord8 >>= \w -> case w of
            0x01 -> return PaymentError
            0x02 -> return ApplicationError
            n    -> fail $ "expected 0x01 or 0x02, not: " ++ show n


-- Generated code with types modified:
-- |
data PaymentResult = PaymentResult
    { paymentResult_channel_status    :: ChannelStatus    -- ^ Equal to \"open\" if the channel is still open, otherwise \"closed\". The channel is automatically closed when there is no value left to send. If a payment sends all remaining channel value to the server, the server will close the channel and set this field to \"closed\".
    , paymentResult_channel_valueLeft :: BitcoinAmount    -- ^ Remaining channel value. This is the amount that the client/sender would receive if the channel was closed now.
    , paymentResult_value_received    :: BitcoinAmount    -- ^ Value of the payment that was just received. This is the additional value assigned to the receiver/server with this payment.
    , paymentResult_settlement_txid   :: Maybe TxHash     -- ^ If channel_status equals \"closed\": the transaction ID of the Bitcoin transaction which settles the channel; otherwise null.
    , paymentResult_application_data  :: T.Text     -- ^ Optional application data
    } deriving (Show, Eq, Generic)

-- |
data FundingInfo = FundingInfo
    { fundingInfoServerPubkey             :: RecvPubKey -- ^ Server/value receiver public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.
    , fundingInfoDustLimit                :: BitcoinAmount  -- ^ (Satoshis) The server will not accept payments where the client change amount is less than this amount. This \"dust limit\" is necessary in order to avoid producing a settlement transaction that will not circulate in the Bitcoin P2P network because it contains an output of minuscule value. Consequently, the maximum amount, that can be sent over the payment channel, is the amount sent to the funding address minus this \"dust limit\".
    , fundingInfoFundingAddressCopy      :: Address    -- ^ Server derived channel funding address. The client will confirm that its own derived funding address matches this one, before paying to it.
    , fundingInfoRedeem_scriptCopy        :: Script     -- ^ Server derived channel redeem script. Defines sender, receiver and channel expiration date. Used to construct the input in the payment transaction. The client will also verify that this matches what it expects. Hex-encoded.
    , fundingInfoOpenPrice                :: BitcoinAmount -- ^ Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time.
    , fundingInfoFunding_tx_min_conf       :: BtcConf -- ^ Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.
    , fundingInfoSettlement_period_hours   :: Hours -- ^ The server reserves the right to close the payment channel this many hours before the specified expiration date. The server hasn't received any actual value until it publishes a payment transaction to the Bitcoin network, so it needs a window of time in which the client can no longer send payments over the channel, and yet the channel refund transaction hasn't become valid.
    , fundingInfoMin_duration_hours        :: Hours
    } deriving (Show, Eq, Generic)

-- | Error response type
data Error = Error
    { errorType     :: ErrorType    -- ^ Either 'payment_error', in case of an invalid payment, or 'application_error', in case of application-related errors (invalid 'application_data' in the supplied **Payment**)
    , errorMessage  :: Text         -- ^ Human-readable error message
    } deriving (Show, Eq, Generic)

-- Just generated code

-- | A wrapper that contains both payment data and application data
data Payment = Payment
    { paymentPaymentData :: FullPayment -- ^ Actual payment
    , paymentApplicationData :: Text -- ^ Optional application data (may be an empty string). The client may wish to include data with the payment, for example an order reference, or any other data which will be used by the server to deliver the appropriate application data in the **PaymentResult** response.
    } deriving (Show, Eq, Generic)

instance FromJSON Payment where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "payment")
instance ToJSON Payment where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "payment")

-- |
data ChannelLocation = ChannelLocation
    { channelInfo_channel_uri :: Text -- ^ The URL of the resource which must the POSTed to in order to open a new payment channel, after which further payments can be PUT on this resource. Close the payment channel by issuing a DELETE request on the resource.
    } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "error")

instance FromJSON FundingInfo where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "fundingInfo")
instance ToJSON FundingInfo where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "fundingInfo")

instance FromJSON PaymentResult where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "paymentResult_")
instance ToJSON PaymentResult where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "paymentResult_")

removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("!", "'Exclamation"), ("<=", "'Less_Than_Or_Equal_To"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("*", "'Star"), ("+", "'Plus"), ("-", "'Dash"), (".", "'Period"), (":", "'Colon"), ("|", "'Pipe"), ("<", "'LessThan"), ("!=", "'Not_Equal"), ("=", "'Equal"), ("^", "'Caret"), (">", "'GreaterThan"), ("_", "'Underscore"), (">=", "'Greater_Than_Or_Equal_To")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace

instance FromJSON ChannelLocation where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "channelInfo_")
instance ToJSON ChannelLocation where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "channelInfo_")

