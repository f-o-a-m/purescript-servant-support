{-- This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where

import Prelude
import Data.Argonaut.Aeson.Options (Options, defaultOptions)
import Data.Argonaut.Aeson.Decode.Generic (class DecodeAeson, genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (class EncodeAeson, genericEncodeAeson)
import Data.Argonaut (class EncodeJson, Json, encodeJson, foldJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Constructor(..), Argument(..), from)
import Global (encodeURIComponent)

jOpts :: Options
jOpts = defaultOptions

-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a r. Generic a r => EncodeAeson r => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a r. Generic a r => DecodeAeson r => Json -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. ToHttpApiData a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. ToHttpApiData a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

class ToHttpApiData a where
  toQueryParam :: a -> String

instance toHttpApiDataString :: ToHttpApiData String where
  toQueryParam = id

instance toHttpApiDataInt :: ToHttpApiData Int where
  toQueryParam = show

instance toHttpApiDataNumber :: ToHttpApiData Number where
  toQueryParam = show

class GenericToHttpApiData a where
  toQueryParam' :: a -> String

instance genericToHttpApiDataNewtype :: EncodeJson a => GenericToHttpApiData (Constructor name (Argument a)) where
  toQueryParam' (Constructor (Argument a)) =
    let json = encodeJson a
    in foldJson (const "") show show id show show json

instance genericToHttpApiData :: (Generic a r, GenericToHttpApiData r) => ToHttpApiData a where
  toQueryParam = toQueryParam' <<< from

defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ (genericEncodeAeson jOpts)
  , decodeJson : SPSettingsDecodeJson_ (genericDecodeAeson jOpts)
  , toURLPiece : SPSettingsToUrlPiece_ toQueryParam
  , encodeHeader : SPSettingsEncodeHeader_ toQueryParam
  , params : params
}
