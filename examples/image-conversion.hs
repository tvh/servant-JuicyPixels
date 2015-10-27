{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Codec.Picture
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.JuicyPixels

#if MIN_VERSION_JuicyPixels(3,2,6)
type FORMATS = '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE, TGA]
#else
type FORMATS = '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE]
#endif

type ConversionApi
     = ReqBody FORMATS DynamicImage
    :> Post FORMATS DynamicImage

conversionApi :: Proxy ConversionApi
conversionApi = Proxy

server :: Server ConversionApi
server = return

conversion :: Application
conversion = serve conversionApi server

main :: IO ()
main = run 8001 conversion
