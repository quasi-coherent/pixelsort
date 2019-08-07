{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Options.Applicative
import           System.FilePath.Lens
import           System.Random


main :: IO ()
main = do
  Opts {..} <- execParser optsParser
  orig <- either (error "Couldn't read image") convertRGBA8 <$> readImage oImgPath
  let baseDir  = oImgPath ^. directory
      fileName = oImgPath ^. filename
  when oRed (writePng (baseDir <> "/sorted-r-" <> fileName) $ makeSortedImage compareRed orig)
  when oGreen (writePng (baseDir <> "/sorted-g-" <> fileName) $ makeSortedImage compareGreen orig)
  when oBlue (writePng (baseDir <> "/sorted-b-" <> fileName) $ makeSortedImage compareBlue orig)
  when oAlpha (writePng (baseDir <> "/sorted-a-" <> fileName) $ makeSortedImage compareAlpha orig)
  when oAverage (writePng (baseDir <> "/sorted-M-" <> fileName) $ makeSortedImage compareAverage orig)
  when oLuminance (writePng (baseDir <> "/sorted-L-" <> fileName) $ makeSortedImage compareLuminance orig)
  when oHue (writePng (baseDir <> "/sorted-H-" <> fileName) $ makeSortedImage compareHue orig)
  when oRandom $ compareRandomly >>= \ord ->
    writePng (baseDir <> "/sorted-rand-" <> fileName) $ makeSortedImage ord orig
  where
    optsParser = info (helper <*> parseOpts) (header "pixelsort")

    parseOpts = Opts
      <$> strOption (long "file" <> help "Image to sort")
      <*> switch (short 'r' <> help "Sort by red")
      <*> switch (short 'g' <> help "Sort by green")
      <*> switch (short 'b' <> help "Sort by blue")
      <*> switch (short 'a' <> help "Sort by alpha")
      <*> switch (short 'M' <> help "Sort by average of pixel values")
      <*> switch (short 'L' <> help "Sort by luminance")
      <*> switch (short 'H' <> help "Sort by hue")
      <*> switch (long "rand" <> help "Sort by random comparison of pixel properties")


-- | CLI.
data Opts = Opts
  { oImgPath   :: FilePath
  , oRed       :: Bool
  , oGreen     :: Bool
  , oBlue      :: Bool
  , oAlpha     :: Bool
  , oAverage   :: Bool
  , oLuminance :: Bool
  , oHue       :: Bool
  , oRandom    :: Bool
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


-- | Which pixel has a greater average of values.
compareAverage :: PixelOrdering
compareAverage (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) =
  compare (fromIntegral (r1 + g1 + b1 + a1) / 4) (fromIntegral (r2 + g2 + b2 + a2) / 4)


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


-- | Random comparison.
compareRandomly :: IO PixelOrdering
compareRandomly = do
  n1 <- randomRIO (0, 3)
  n2 <- randomRIO (0, 3)
  return $ compare' n1 n2
  where
    compare' :: Int -> Int -> PixelRGBA8 -> PixelRGBA8 -> Ordering
    compare' 0 0 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare r1 r2
    compare' 0 1 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare r1 g2
    compare' 0 2 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare r1 b2
    compare' 0 3 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare r1 a2
    compare' 1 0 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare g1 r2
    compare' 1 1 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare g1 g2
    compare' 1 2 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare g1 b2
    compare' 1 3 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare g1 a2
    compare' 2 0 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare b1 r2
    compare' 2 1 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare b1 g2
    compare' 2 2 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare b1 b2
    compare' 2 3 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare b1 a2
    compare' 3 0 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare a1 r2
    compare' 3 1 (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare a1 g2
    compare' 3 2 (PixelRGBA8 r1 gf b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare a1 b2
    compare' 3 3 (PixelRGBA8 r1 gf b1 a1) (PixelRGBA8 r2 g2 b2 a2) = compare a1 a2
    compare' _ _ _ _ = error "The impossible has happened"
