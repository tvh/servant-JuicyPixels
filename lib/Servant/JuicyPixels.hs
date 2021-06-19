{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances #-}

module Servant.JuicyPixels where

import Servant.API
import GHC.TypeLits
import qualified Network.HTTP.Media as M
import Codec.Picture
import Codec.Picture.Metadata
import Codec.Picture.Jpg
import Codec.Picture.Saving
import Codec.Picture.Types
import Data.Proxy
import qualified Data.ByteString.Lazy as BL


data BMP

instance Accept BMP where
    contentType _ = "image" M.// "bmp"

instance MimeRender BMP DynamicImage where
    mimeRender _ = imageToBitmap

instance MimeUnrender BMP DynamicImage where
    mimeUnrender _ = decodeBitmap . BL.toStrict


data GIF

instance Accept GIF where
    contentType _ = "image" M.// "gif"

instance MimeRender GIF DynamicImage where
    mimeRender _ = either error id . imageToGif

instance MimeUnrender GIF DynamicImage where
    mimeUnrender _ = decodeGif . BL.toStrict


data JPEG (quality :: Nat)

instance (KnownNat quality, quality <= 100) => Accept (JPEG quality) where
    contentType _ = "image" M.// "jpeg"

instance (KnownNat quality, quality <= 100) => MimeRender (JPEG quality) DynamicImage where
    mimeRender _ img =
      let quality = fromInteger $ natVal (Proxy :: Proxy quality)
      in imageToJpg quality img

instance (KnownNat quality, quality <= 100, ColorSpaceConvertible a PixelYCbCr8) => MimeRender (JPEG quality) (Image a) where
    mimeRender _ = encodeJpegAtQuality quality . convertImage
      where quality = fromInteger $ natVal (Proxy :: Proxy quality)

instance (KnownNat quality, quality <= 100) => MimeUnrender (JPEG quality) DynamicImage where
    mimeUnrender _ = decodeJpeg . BL.toStrict

data JPEG_METADATA (quality :: Nat)

instance (KnownNat quality, quality <= 100) => Accept (JPEG_METADATA quality) where
    contentType _ = "image" M.// "jpeg"

instance (KnownNat quality, quality <= 100) => MimeRender (JPEG_METADATA quality) DynamicImage where
    mimeRender _ img =
      let quality = fromInteger $ natVal (Proxy :: Proxy quality)
      in imageToJpg quality img

instance (KnownNat quality, quality <= 100, ColorSpaceConvertible a PixelYCbCr8) => MimeRender (JPEG_METADATA quality) (Image a) where
    mimeRender _ = encodeJpegAtQuality quality . convertImage
      where quality = fromInteger $ natVal (Proxy :: Proxy quality)

instance (KnownNat quality, quality <= 100, ColorSpaceConvertible a PixelYCbCr8) => MimeRender (JPEG_METADATA quality) (Image a, Metadatas) where
    mimeRender _ (img,metadata) = encodeJpegAtQualityWithMetadata quality metadata $ convertImage img
      where quality = fromInteger $ natVal (Proxy :: Proxy quality)

instance (KnownNat quality, quality <= 100) => MimeUnrender (JPEG_METADATA quality) (DynamicImage, Metadatas) where
    mimeUnrender _ = decodeJpegWithMetadata . BL.toStrict

data PNG

instance Accept PNG where
    contentType _ = "image" M.// "png"

instance MimeRender PNG DynamicImage where
    mimeRender _ = imageToPng

instance PngSavable a => MimeRender PNG (Image a) where
    mimeRender _ = encodePng

instance MimeUnrender PNG DynamicImage where
    mimeUnrender _ = decodePng . BL.toStrict


data TIFF

instance Accept TIFF where
    contentType _ = "image" M.// "tiff"

instance MimeRender TIFF DynamicImage where
    mimeRender _ = imageToTiff

instance TiffSaveable a => MimeRender TIFF (Image a) where
    mimeRender _ = encodeTiff

instance MimeUnrender TIFF DynamicImage where
    mimeUnrender _ = decodeTiff . BL.toStrict


data RADIANCE

instance Accept RADIANCE where
    contentType _ = "image" M.// "vnd.radiance"

instance MimeRender RADIANCE DynamicImage where
    mimeRender _ = imageToRadiance

instance a ~ PixelRGBF => MimeRender RADIANCE (Image a) where
    mimeRender _ = encodeHDR

instance MimeUnrender RADIANCE DynamicImage where
    mimeUnrender _ = decodeHDR . BL.toStrict

#if MIN_VERSION_JuicyPixels(3,2,6)
data TGA

instance Accept TGA where
    contentType _ = "image" M.// "x-targa"

instance MimeRender TGA DynamicImage where
    mimeRender _ = imageToTga

instance TgaSaveable a => MimeRender TGA (Image a) where
    mimeRender _ = encodeTga

instance MimeUnrender TGA DynamicImage where
    mimeUnrender _ = decodeTga . BL.toStrict
#endif
