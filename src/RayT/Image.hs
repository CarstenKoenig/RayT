-- | methods to render a picture
-- given a 'PixelGen' (a function returning color information for each pixel-coordinate)
-- use 'renderToPng' to generate a generic PNG image
module RayT.Image 
    ( ImageCoords, ImageSize, ColorRGB, PixelGen,
      renderToPng, traceToPng, rasterPoint
    ) where

import RayT
import RayT.Vector
import RayT.Colors

import Codec.Picture (PixelRGB8(..), Image, generateImage, writePng)
import Data.Array.Repa as Repa hiding ((++))
import Data.Functor.Identity


-- * Type synonyms

-- | use to generate the image
type PixelGen = ImageCoords -> Color

-- * methods

-- | renders a ray-traced image and writes in PNG format to the file system
traceToPng :: ImageSize -- ^ the resolution of the generated image
           -> Camera    -- ^ the camera used for tracing
           -> Scene     -- ^ the scene to be traced
           -> FilePath  -- ^ the destination where the image should be written to
           -> IO()
traceToPng sz cam sc path = saveImage path $ traceImage sz cam sc

-- | renders a image and writes in PNG format to the file system
-- 
-- for example this will render a all-red image of size 128x128 to ./test.png
-- 
-- >>> renderToPng (128, 128) (const (255,0,0)) "./test.png"
renderToPng :: ImageSize -- ^ the resolution of the generated image
            -> PixelGen  -- ^ called to get the color of each pixel ([0..width-1]x[0..height-1])
            -> FilePath  -- ^ the destination where the image should be written to
            -> IO()
renderToPng sz gen path = saveImage path $ renderImageParallel sz gen

-- * non exported helpers

saveImage :: FilePath -> Image PixelRGB8 -> IO ()
saveImage path img = writePng path img

traceImage :: ImageSize -> Camera -> Scene -> Image PixelRGB8
traceImage sz cam sc = renderImageParallel sz gen
  where gen = traceRay sc . rasterRay cam sz

-- | renders the image in parallel
renderImageParallel :: ImageSize -> PixelGen -> Image PixelRGB8
renderImageParallel sz@(w,h) gen = runIdentity go
    where go             = do
            arr <- stepArray
            return $ generateImage (getPixel arr) w h
          getPixel arr x' y'  = decodeColor $ arr ! (Z:.x':.y')
          stepArray :: Identity (Array U DIM2 ColorRGB)
          stepArray           = computeP $ imageArray sz gen
          decodeColor (r,g,b) = PixelRGB8 r g b

imageArray :: ImageSize -> PixelGen -> Array D DIM2 ColorRGB
imageArray (w, h) gen = fromFunction (Z:.w:.h) calcPixel
    where calcPixel (Z:.x':.y') = toColorRGB $ gen (x',y')

-- | determine a point on the screen by rastering the complete screen
rasterPoint :: Screen -> ImageSize -> ImageCoords -> R3
rasterPoint sc (iW, iH) (iX, iY) = start + x.*aX - y.*aY
  where x = (0.5 + fromIntegral iX) / fromIntegral iW
        y = (0.5 + fromIntegral iY) / fromIntegral iH
        aX = axisX sc
        aY = axisY sc
        start = center sc - 0.5.*(aX - aY)

-- | uses `rasterPoint` to get a start-ray from the camaras eye to a rasterized point on the screen
rasterRay :: Camera -> ImageSize -> ImageCoords -> Ray
rasterRay cam sz = rayTo (eyePos cam) . rasterPoint (screen cam) sz

