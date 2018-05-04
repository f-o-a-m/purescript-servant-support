{-- This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where

import Prelude
import Data.Argonaut.Aeson.Options (Options, defaultOptions)
import Data.Argonaut.Aeson.Decode.Generic (class DecodeAeson, genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (class EncodeAeson, genericEncodeAeson)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Global (encodeURIComponent)

jOpts :: Options
jOpts = defaultOptions

-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a r. Generic a r => EncodeAeson r => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a r. Generic a r => DecodeAeson r => Json -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a r. Generic a r => EncodeAeson r => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a r. Generic a r => EncodeAeson r => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a r. Generic a r => EncodeAeson r => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a r. Generic a r => EncodeAeson r => a -> URLPiece
gDefaultEncodeHeader = show <<< genericEncodeAeson jOpts

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a r. Generic a r => EncodeAeson r => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ (genericEncodeAeson jOpts)
  , decodeJson : SPSettingsDecodeJson_ (genericDecodeAeson jOpts)
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}
