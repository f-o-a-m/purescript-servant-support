module Servant.PureScript.Util where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (Json, class DecodeJson, class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Data.Foldable (intercalate)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (makeAjaxError, AjaxError, ErrorDescription(DecodingError, ParsingError, UnexpectedHTTPStatus), AjaxRequest)
import Servant.PureScript.Settings (SPSettings_(SPSettings_), SPSettingsToUrlPiece_(SPSettingsToUrlPiece_), SPSettingsEncodeHeader_(SPSettingsEncodeHeader_))

-- | Get the result of a request.
-- |
-- | Reports an error if status code is non success or decoding fails. The taken AjaxRequest is only for error reporting.
getResult
  :: forall a m.
     DecodeJson a
  => MonadError AjaxError m
  => AjaxRequest
  -> (Json -> Either String a)
  -> AffjaxResponse String -> m a
getResult req' decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then pure resp.response
            else throwError $ makeAjaxError req' (UnexpectedHTTPStatus resp)
  jVal <- throwLeft <<< lmap (reportRequestError req' ParsingError fVal) <<< jsonParser $ fVal
  throwLeft <<< lmap (reportRequestError req' DecodingError (show jVal)) <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = pure a


encodeListQuery :: forall a b. EncodeJson a => SPSettings_ b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

class EncodeQueryItem a where
  encodeQueryItem :: forall b. SPSettings_ b -> String -> a -> String

instance jsonEncodeQ :: EncodeJson a => EncodeQueryItem a where
  encodeQueryItem opts'@(SPSettings_ opts) fName val = fName <> "=" <> encodeURLPiece opts' val

instance maybeEncodeQ :: EncodeQueryItem a => EncodeQueryItem (Maybe a) where
  encodeQueryItem opts fName = maybe "" $ encodeQueryItem opts fName

-- | Call opts.toURLPiece and encode the resulting string with encodeURIComponent.
encodeURLPiece :: forall a params. EncodeJson a => SPSettings_ params -> a -> String
encodeURLPiece (SPSettings_ opts) = case opts.toURLPiece of SPSettingsToUrlPiece_ f -> f

encodeHeader :: forall a params. EncodeJson a => SPSettings_ params -> a -> String
encodeHeader (SPSettings_ opts) = case opts.encodeHeader of SPSettingsEncodeHeader_ f -> f

reportRequestError :: AjaxRequest -> (String -> ErrorDescription) -> String -> String -> AjaxError
reportRequestError req' err source msg = makeAjaxError req' $ reportError err source msg

reportError :: forall err. (String -> err) -> String -> String  -> err
reportError err source msg = err $ msg <> ", source: '" <> source <> "'"
