{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad (when)
import           Control.Monad.ST
import           Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Options.Applicative


main :: IO ()
main = do
  opts@Opts {..} <- execParser optsParser
  orig <- either (error "Couldn't read image") convertRGB8 <$> readImage oImgPath
  when oRed (writePng ("sorted-r-" <> oImgPath) $ makeSortedImage compareRed orig)
  when oGreen (writePng ("sorted-g-" <> oImgPath) $ makeSortedImage compareGreen orig)
  when oBlue (writePng ("sorted-b-" <> oImgPath) $ makeSortedImage compareBlue orig)
  when oLuminance (writePng ("sorted-L-" <> oImgPath) $ makeSortedImage compareLuminance orig)
  where
    optsParser = info (helper <*> parseOpts) (header "pixelsort")

    parseOpts = Opts
      <$> strOption (long "file" <> help "Image to sort")
      <*> switch (short 'r' <> help "Sort by red")
      <*> switch (short 'g' <> help "Sort by green")
      <*> switch (short 'b' <> help "Sort by blue")
      <*> switch (short 'L' <> help "Sort by luminance")


-- | CLI.
data Opts = Opts
  { oImgPath   :: FilePath
  , oRed       :: Bool
  , oGreen     :: Bool
  , oBlue      :: Bool
  , oLuminance :: Bool
  } deriving (Eq, Show)


-- | Sort the image with the given ordering.
makeSortedImage :: PixelOrdering -> Image PixelRGB8 -> Image PixelRGB8
makeSortedImage f Image {..} = runST $ do
  mimg <- newMutableImage imageWidth imageHeight
  go 0 imageData mimg
  where
    go r d mimg
      | r >= imageHeight = unsafeFreezeImage mimg
      | otherwise = do
          let row = makeRow (3*imageWidth) (VS.take (3*imageWidth) d)
              sortedRow = V.modify (VA.sortBy f) row
          writeRow 0 r sortedRow mimg
          go (r+1) (VS.drop (3*imageWidth) d) mimg

    writeRow c r v mimg
      | c >= imageWidth = unsafeFreezeImage mimg
      | otherwise = do
          writePixel mimg c r (v V.! c)
          writeRow (c+1) r v mimg


-- | Make one row of 'PixelRGB8's from the image's raw representation.
makeRow :: Int -> VS.Vector Word8 -> V.Vector PixelRGB8
makeRow = go V.empty
  where
    go !acc !w !d
      | w == 0    = acc
      | otherwise = go (acc V.++ makePixel (VS.take 3 d)) (w-3) (VS.drop 3 d)

    makePixel d = V.singleton (PixelRGB8 (d VS.! 0) (d VS.! 1) (d VS.! 2))


-- | How to arrange pixels.
type PixelOrdering = PixelRGB8 -> PixelRGB8 -> Ordering


-- | Composition of user sort choices.
userSortChoice :: Opts -> PixelOrdering
userSortChoice Opts {..} = if length userOpts /= 1
  then error "Only one option can be supplied at a time"
  else head userOpts
  where
    os = [oRed, oGreen, oBlue, oLuminance]
    fs = [compareRed, compareGreen, compareBlue, compareLuminance]
    userOpts = [f | (o, f) <- zip os fs, o]


-- | Which pixel is more red.
compareRed :: PixelOrdering
compareRed (PixelRGB8 r1 _ _) (PixelRGB8 r2 _ _) = compare r1 r2


-- | Which pixel is more green.
compareGreen :: PixelOrdering
compareGreen (PixelRGB8 _ g1 _) (PixelRGB8 _ g2 _) = compare g1 g2


-- | Which pixel is more blue.
compareBlue :: PixelOrdering
compareBlue (PixelRGB8 _ _ b1) (PixelRGB8 _ _ b2) = compare b1 b2


-- | Which pixel is brighter.
--
-- https://en.wikipedia.org/wiki/Relative_luminance
compareLuminance :: PixelOrdering
compareLuminance a b = compare (relativeLuminance a) (relativeLuminance b)
  where
    relativeLuminance (PixelRGB8 r g b)
      = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b
