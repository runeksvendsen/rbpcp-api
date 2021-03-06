{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module RBPCP.Types
(
  module RBPCP.Types
, PAYREQ
, JsonHex(..)
, BaseUrl(..)
, Client(..)
, Server(..)
, BtcTxId(..)
, SharedSecret(..)
, BtcScript(..)
, PubKey
)
where

import           RBPCP.Internal.Types
import           RBPCP.Internal.Util
import           PayProto                   (PAYREQ, PayReqSpec(..), mkPayRequestT)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Encoding         (encodeUtf8, decodeUtf8)
import           Data.Word                  (Word32, Word64)
import qualified Data.Serialize             as Bin
import qualified Data.ByteString.Lazy       as BL
import           Servant.API                (MimeRender(..), JSON)
import           Servant.Client             (BaseUrl(..))
import qualified Web.HttpApiData as Web
import           Data.Time.Clock
-- Generated imports
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T


type BLT = Word32     -- ^ Bitcoin lock-time
type Vout  = Word32
type Hours = Word
type BtcConf = Word


newtype SharedSecret = SharedSecret { ssHash :: Hash256 }
    deriving (Eq, Show, Generic, Bin.Serialize, NFData)

instance ToJSON SharedSecret where
    toJSON = String . cs . hexEncode

instance FromJSON SharedSecret where
    parseJSON = withText "SharedSecret" (either fail return . hexDecode . cs)

instance Web.FromHttpApiData SharedSecret where
    parseUrlPiece = fmapL cs . eitherDecode . cs
instance Web.ToHttpApiData SharedSecret where
    toUrlPiece = cs . encode


newtype BtcScript = BtcScript { bsGetScript :: Script }
    deriving (Eq, Show, Generic, Bin.Serialize)

instance ToJSON BtcScript where
    toJSON = String . cs . hexEncode

instance FromJSON BtcScript where
    parseJSON = withText "BtcScript" (either fail return . hexDecode . cs)


data FundInfoResponse = FundInfoResponse
    { firFundInfo   :: FundingInfo
    , firUserMemo   :: T.Text       -- ^ Memo displayed to user
    , firNow        :: UTCTime      -- ^ Current time
    , firPaymentURL :: BaseUrl      -- ^ Paying Bitcoin transaction is POSTed to this URL
    -- ^ BIP70 @Payment@ POST URL. Will receive message (containing tx paying to the funding address)
    , firExpSecs    :: Word64
    }

instance MimeRender PAYREQ FundInfoResponse where
    mimeRender a FundInfoResponse{..} =
        mimeRender a $
            mkPayRequestT firNow prs
                where prs = PayReqSpec
                          { prsOuts = [(fundingInfoFundingAddressCopy firFundInfo, 0)]
                          , prsPayUrl       = firPaymentURL
                          , prsExpSecs      = firExpSecs
                          , prsMerchMemo    = firUserMemo
                          , prsMerchData    = BL.toStrict $ encode firFundInfo
                          }

instance MimeRender JSON FundInfoResponse where
    mimeRender a FundInfoResponse{..} =
        mimeRender a firFundInfo

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

instance Bin.Serialize Text where
    put = Bin.put . encodeUtf8
    get = decodeUtf8 <$> Bin.get

-- Generated code with some types modified:
-- |
data PaymentResult = PaymentResult
    { paymentResultChannelStatus    :: ChannelStatus    -- ^ Equal to \"open\" if the channel is still open, otherwise \"closed\". The channel is automatically closed when there is no value left to send. If a payment sends all remaining channel value to the server, the server will close the channel and set this field to \"closed\".
    , paymentResultChannelValueLeft :: Word64           -- ^ Remaining channel value. This is the amount that the client/sender would receive if the channel was closed now.
    , paymentResultValueReceived    :: Word64     -- ^ Value of the payment that was just received. This is the additional value assigned to the receiver/server with this payment.
    , paymentResultSettlementTxid   :: Maybe BtcTxId     -- ^ If channel_status equals \"closed\": the transaction ID of the Bitcoin transaction which settles the channel; otherwise null.
    , paymentResultApplicationData  :: Text           -- ^ Optional application data
    } deriving (Show, Eq, Generic, Bin.Serialize)


instance FromJSON PaymentResult where
  parseJSON  = genericParseJSON  (removePrefixCamelUnderscore "paymentResult")
instance ToJSON PaymentResult where
  toJSON     = genericToJSON     (removePrefixCamelUnderscore "paymentResult")


-- |
data FundingInfo = FundingInfo
    { fundingInfoServerPubkey            :: Server PubKey -- ^ Server/value receiver public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.
    , fundingInfoDustLimit               :: Word64        -- ^ (Satoshis) The server will not accept payments where the client change amount is less than this amount. This \"dust limit\" is necessary in order to avoid producing a settlement transaction that will not circulate in the Bitcoin P2P network because it contains an output of minuscule value. Consequently, the maximum amount, that can be sent over the payment channel, is the amount sent to the funding address minus this \"dust limit\".
    , fundingInfoFundingAddressCopy      :: Address       -- ^ Server derived channel funding address. The client will confirm that its own derived funding address matches this one, before paying to it.
    , fundingInfoOpenPrice               :: Word64        -- ^ Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time.
    , fundingInfoFundingTxMinConf        :: BtcConf       -- ^ Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.
    , fundingInfoSettlementPeriodHours   :: Hours         -- ^ The server reserves the right to close the payment channel this many hours before the specified expiration date. The server hasn't received any actual value until it publishes a payment transaction to the Bitcoin network, so it needs a window of time in which the client can no longer send payments over the channel, and yet the channel refund transaction hasn't become valid.
    , fundingInfoMinDurationHours        :: Hours         -- ^ Minimum duration of newly opened channels
    } deriving (Show, Eq, Generic, Bin.Serialize)


instance FromJSON FundingInfo where
  parseJSON  = genericParseJSON  (removePrefixCamelUnderscore "fundingInfo")
instance ToJSON FundingInfo where
  toJSON     = genericToJSON     (removePrefixCamelUnderscore "fundingInfo")

-- | Error response type
data Error = Error
    { errorType     :: ErrorType    -- ^ Either 'payment_error', in case of an invalid payment, or 'application_error', in case of application-related errors (invalid 'application_data' in the supplied **Payment**)
    , errorMessage  :: Text         -- ^ Human-readable error message
    } deriving (Show, Eq, Generic, Bin.Serialize)

-- | A payment comprises a signature over a Bitcoin transaction with a decremented client change value.
-- The Bitcoin transaction redeems the outpoint specified by &#39;funding_txid&#39; and &#39;funding_vout&#39; (a P2SH output governed by &#39;redeem_script&#39;), and pays &#39;change_value&#39; to &#39;change_address&#39;.
data PaymentData = PaymentData
    { paymentDataRedeemScript   :: JsonHex BtcScript    -- ^ The funds sent to the funding address are bound by this contract (Bitcoin script). The data is needed to construct the payment signature. Hex-encoded data.
    , paymentDataFundingTxid    :: BtcTxId              -- ^ The transaction ID of the Bitcoin transaction paying to the channel funding address.
    , paymentDataFundingVout    :: Vout                 -- ^ The output index/\"vout\" of the output (in the transaction) payingto the channel funding address.
    , paymentDataSignatureData  :: JsonHex Signature    -- ^ DER-encoded ECDSA signature (in hex). This is a SIGHASH_SINGLE|ANYONECANPAY signature over the the \"payment transaction\", which is a Bitcoin transaction that: redeems the outpoint specified by 'funding_txid' and 'funding_vout' using the redeem script defined in 'redeem_script', with an output which sends 'change_value' to 'change_address'.
    , paymentDataChangeValue    :: Word64               -- ^ The value sent back to the client in the payment transaction. The total amount transferred to the server is this amount subtracted from the value sent to the channel funding address.
    , paymentDataChangeAddress  :: Address              -- ^ The client change address as used in the only output of the payment transaction.
    , paymentDataSighashFlag    :: JsonHex SigHash      -- ^ Specifies which parts of the payment Bitcoin transaction are signed. Hex-encoded, single byte; in both v1 and v2 always equal to \"83\" (0x83), which is **SIGHASH_SINGLE|ANYONECANPAY**, meaning the client only signs its own output, and also allowing more to be added.
    } deriving (Show, Eq, Generic, Bin.Serialize)

instance FromJSON PaymentData where
  parseJSON  = genericParseJSON  (removePrefixCamelUnderscore "paymentData")
instance ToJSON PaymentData where
  toJSON     = genericToJSON     (removePrefixCamelUnderscore "paymentData")


-- | A wrapper that contains both payment data and application data
data Payment = Payment
    { paymentPaymentData        :: PaymentData  -- ^ Actual payment
    , paymentApplicationData    :: Text         -- ^ Optional application data (may be an empty string). The client may wish to include data with the payment, for example an order reference, or any other data which will be used by the server to deliver the appropriate application data in the **PaymentResult** response.
    } deriving (Show, Eq, Generic, Bin.Serialize)

instance FromJSON Payment where
  parseJSON  = genericParseJSON  (removePrefixCamelUnderscore "payment")
instance ToJSON Payment where
  toJSON     = genericToJSON     (removePrefixCamelUnderscore "payment")

instance FromJSON Error where
  parseJSON  = genericParseJSON  (removePrefixCamelUnderscore "error")
instance ToJSON Error where
  toJSON     = genericToJSON     (removePrefixCamelUnderscore "error")

removePrefixCamelUnderscore :: String -> Options
removePrefixCamelUnderscore prefix =
  defaultOptions
    { fieldLabelModifier =
          camelTo2 '_'
        . fromMaybe (error ("did not find prefix " ++ prefix))
        . stripPrefix prefix
    }
