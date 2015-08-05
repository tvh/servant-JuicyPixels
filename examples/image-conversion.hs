{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Codec.Picture
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.JuicyPixels

type ConversionApi
     = ReqBody '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage
    :> Post '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE] DynamicImage

conversionApi :: Proxy ConversionApi
conversionApi = Proxy

server :: Server ConversionApi
server = return

conversion :: Application
conversion = serve conversionApi server

main :: IO ()
main = run 8001 conversion
