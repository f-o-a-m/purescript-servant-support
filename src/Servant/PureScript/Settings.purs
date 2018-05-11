{-- This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where

import Prelude
import Data.Either (Either)
import Data.Argonaut (class EncodeJson, class DecodeJson, Json, decodeJson, encodeJson, foldJson)
import Global (encodeURIComponent)

-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957
newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a. EncodeJson a => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a. DecodeJson a => Json -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. EncodeJson a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. EncodeJson a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

toQueryParam :: forall a. EncodeJson a => a -> String
toQueryParam a =
    let json = encodeJson a
    in foldJson (const "") show show id show show json

defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ encodeJson
  , decodeJson : SPSettingsDecodeJson_ decodeJson
  , toURLPiece : SPSettingsToUrlPiece_ (encodeURIComponent <<< toQueryParam)
  , encodeHeader : SPSettingsEncodeHeader_ (encodeURIComponent <<< toQueryParam)
  , params : params
}
