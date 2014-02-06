-- | methods to render a picture
-- given a 'PixelGen' (a function returning color information for each pixel-coordinate)
-- use 'renderToPng' to generate a generic PNG image
module RayT.Image 
    ( ImageCoords, ImageSize, ColorRGB, PixelGen,
      renderToPng
    ) where

import Codec.Picture (PixelRGB8(..), Image, generateImage, writePng)
import Data.Array.Repa as Repa hiding ((++))
import Data.Functor.Identity


-- * Type synonyms

-- | Pixel-coordinate (X, Y)
type    ImageCoords   = (Int, Int)

-- | Image size (width, height)
type    ImageSize     = (Int, Int)

-- | color information for a pixel (red, green, blue)
type    ColorRGB      = (Int, Int, Int)

-- | use to generate the image
type    PixelGen      = ImageCoords -> ColorRGB


-- * methods

-- | renders a image and writes in PNG format to the file system
-- 
-- for example this will render a all-red image of size 128x128 to ./test.png
-- 
-- >>> renderToPng (128, 128) (const (255,0,0)) "./test.png"
renderToPng :: ImageSize -- ^ the resolution of the generated image
            -> PixelGen  -- ^ called to get the color of each pixel ([0..width-1]x[0..height-1])
            -> FilePath  -- ^ the destination where the image should be written to
            -> IO()
renderToPng sz gen path = do
  let image = renderImageParallel sz gen
  writePng path image


-- * non exported helpers

-- | renders the image in parallel
renderImageParallel :: ImageSize -> PixelGen -> Image PixelRGB8
renderImageParallel sz@(w,h) gen = runIdentity go
    where go             = do
            arr <- stepArray
            return $ generateImage (getPixel arr) w h
          getPixel arr x' y' = colorToPixelRGB8 $ arr ! (Z:.x':.y')
          stepArray :: Identity (Array U DIM2 ColorRGB)
          stepArray          = computeP $ imageArray sz gen

imageArray :: ImageSize -> PixelGen -> Array D DIM2 ColorRGB
imageArray (w, h) gen = fromFunction (Z:.w:.h) calcPixel
    where calcPixel (Z:.x':.y') = gen (x',y')

colorToPixelRGB8 :: ColorRGB -> PixelRGB8
colorToPixelRGB8 (r, g, b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)