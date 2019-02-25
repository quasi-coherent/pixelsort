{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad (when)
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Options.Applicative


main :: IO ()
main = do
  opts@Opts {..} <- execParser optsParser
  orig <- either (error "Couldn't read image") convertRGBA8 <$> readImage oImgPath
  when oRed (writePng ("sorted-r-" <> oImgPath) $ makeSortedImage compareRed orig)
  when oGreen (writePng ("sorted-g-" <> oImgPath) $ makeSortedImage compareGreen orig)
  when oBlue (writePng ("sorted-b-" <> oImgPath) $ makeSortedImage compareBlue orig)
  when oAlpha (writePng ("sorted-a-" <> oImgPath) $ makeSortedImage compareAlpha orig)
  when oLuminance (writePng ("sorted-L-" <> oImgPath) $ makeSortedImage compareLuminance orig)
  when oHue (writePng ("sorted-H-" <> oImgPath) $ makeSortedImage compareHue orig)
  where
    optsParser = info (helper <*> parseOpts) (header "pixelsort")

    parseOpts = Opts
      <$> strOption (long "file" <> help "Image to sort")
      <*> switch (short 'r' <> help "Sort by red")
      <*> switch (short 'g' <> help "Sort by green")
      <*> switch (short 'b' <> help "Sort by blue")
      <*> switch (short 'a' <> help "Sort by alpha")
      <*> switch (short 'L' <> help "Sort by luminance")
      <*> switch (short 'H' <> help "Sort by hue")


-- | CLI.
data Opts = Opts
  { oImgPath   :: FilePath
  , oRed       :: Bool
  , oGreen     :: Bool
  , oBlue      :: Bool
  , oAlpha     :: Bool
  , oLuminance :: Bool
  , oHue       :: Bool
  } deriving (Eq, Show)


-- | Sort the image with the given ordering.
makeSortedImage :: PixelOrdering -> Image PixelRGBA8 -> Image PixelRGBA8
makeSortedImage f Image {..} = runST $ do
  mimg <- newMutableImage imageWidth imageHeight
  go 0 imageData mimg
  where
    go r d mimg
      | r >= imageHeight = unsafeFreezeImage mimg
      | otherwise = do
          let row = makeRow (4 * imageWidth) (VS.take (4 * imageWidth) d)
              sortedRow = V.modify (VA.sortBy f) row
          writeRow 0 r sortedRow mimg
          go (r + 1) (VS.drop (4 * imageWidth) d) mimg

    writeRow c r v mimg
      | c >= imageWidth = unsafeFreezeImage mimg
      | otherwise = do
          writePixel mimg c r (v V.! c)
          writeRow (c + 1) r v mimg


-- | Make one row of 'PixelRGBA8's from the image's raw representation.
makeRow :: Int -> VS.Vector Word8 -> V.Vector PixelRGBA8
makeRow = go V.empty
  where
    go !acc !w !d
      | w == 0    = acc
      | otherwise = go (acc V.++ makePixel (VS.take 4 d)) (w - 4) (VS.drop 4 d)

    makePixel d = V.singleton (PixelRGBA8 (d VS.! 0) (d VS.! 1) (d VS.! 2) (d VS.! 3))


-- | How to arrange pixels.
type PixelOrdering = PixelRGBA8 -> PixelRGBA8 -> Ordering


-- | Which pixel is more red.
compareRed :: PixelOrdering
compareRed (PixelRGBA8 r1 _ _ _) (PixelRGBA8 r2 _ _ _) = compare r1 r2


-- | Which pixel is more green.
compareGreen :: PixelOrdering
compareGreen (PixelRGBA8 _ g1 _ _) (PixelRGBA8 _ g2 _ _) = compare g1 g2


-- | Which pixel is more blue.
compareBlue :: PixelOrdering
compareBlue (PixelRGBA8 _ _ b1 _) (PixelRGBA8 _ _ b2 _) = compare b1 b2


-- | Which pixel is less opaque.
compareAlpha :: PixelOrdering
compareAlpha (PixelRGBA8 _ _  _ a1) (PixelRGBA8 _ _ _ a2) = compare a1 a2


-- | Which pixel is brighter.
--
-- https://en.wikipedia.org/wiki/Relative_luminance
compareLuminance :: PixelOrdering
compareLuminance a b = compare (relativeLuminance a) (relativeLuminance b)
  where
    relativeLuminance (PixelRGBA8 r g b _)
      = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b


-- | Which pixel is more hue-y.
--
-- https://en.wikipedia.org/wiki/Hue
compareHue :: PixelOrdering
compareHue a b = compare (hue a) (hue b)
  where
    hue (PixelRGBA8 r g b _) =
      atan2 (sqrt 3 * (fromIntegral g - fromIntegral b)) (2 * fromIntegral r - fromIntegral g - fromIntegral b)
