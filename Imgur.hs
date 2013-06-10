{-# LANGUAGE OverloadedStrings #-}

module Imgur
( ClientID()
, Image(..)
, uploadFile
, copyFromUrl
, uploadImageData
, getImage ) where

  import Control.Applicative ( (<$>), (<*>) )
  import Control.Monad ( mzero ) 
  import Control.Monad.Trans.Resource ( ResourceT )
  import Data.Aeson
  import Network.HTTP.Conduit hiding ( method )
  import Network.HTTP.Conduit.MultipartFormData
  import Network.HTTP.Types.Header ( Header )
  import Prelude hiding ( lookup )
  import qualified Data.ByteString.Char8 as BS
  import qualified Data.ByteString.Lazy.Char8 as LBS

  type ClientID = String
  data Imgur = Imgur ClientID
    deriving ( Show )
  data Filetype = JPG | PNG | GIF
    deriving ( Show )

  data Image = Image { imgurHash :: String
                     , imgurDeleteHash :: Maybe String }
    deriving ( Show )

  instance FromJSON Image where
    parseJSON (Object o) = do
      dataObject <- o .: "data"
      case dataObject of 
        Object o' -> 
          Image <$> o' .: "id" <*> o' .:? "deletehash"
        _ -> mzero
    parseJSON _ = mzero

  get :: ClientID -> String -> String -> IO LBS.ByteString
  get clientID method hash =
    withManager $ \manager -> do
      initR <- parseUrl $ generateURL method hash
      let hdrR = initR { requestHeaders = [authHeader clientID] }
      responseBody <$> httpLbs hdrR manager

  generateURL :: String -> String -> String
  generateURL method hash =
    endpoint ++ method ++ "/" ++ hash ++ ".json"

  authHeader :: ClientID -> Header
  authHeader clientID = ("Authorization", BS.pack $ "Client-ID " ++ clientID)

  endpoint :: String
  endpoint = "https://api.imgur.com/3/"

  post :: ClientID -> [Part (ResourceT IO) (ResourceT IO)] -> IO LBS.ByteString
  post clientID formData =
    withManager $ \manager -> do
      initR <- parseUrl $ endpoint ++ "upload.json"
      let hdrR = initR { requestHeaders = [authHeader clientID] }
      responseBody <$> ((formDataBody formData hdrR) >>= flip httpLbs manager)

  uploadFile :: ClientID -> String -> IO (Maybe Image)
  uploadFile clientID filename =
    decode <$> post clientID [partFileSource "image" filename]

  uploadImageData :: ClientID -> BS.ByteString -> IO (Maybe Image)
  uploadImageData clientID imageData =
    decode <$> post clientID [partBS "image" imageData]

  copyFromUrl :: ClientID -> String -> IO (Maybe Image)
  copyFromUrl clientID url =
    decode <$> post clientID [partBS "image" $ BS.pack url]

  getImage :: ClientID -> String -> IO (Maybe Image)
  getImage clientID hash =
    decode <$> get clientID "image" hash