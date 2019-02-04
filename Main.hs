{-# LANGUAGE RecordWildCards #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad.ST
import qualified Data.List as List
import qualified Data.Vector.Storable as V
import           Options.Applicative


main :: IO ()
main = do
  opts@Opts {..} <- execParser optsParser
  orig <- either (error "Couldn't read image") convertRGB8 <$> readImage optImgPath
  let tbl = pixelSorter opts $ makeTabRep orig
      new = fillCanvas (Canvas (imageWidth orig) (imageHeight orig)) tbl

  writePng ("sorted-" <> optImgPath) new

  where
    optsParser = info (helper <*> parseOpts) (header "pixelsort")

    parseOpts = Opts
      <$> strOption (long "file" <> help "Image to sort")
      <*> switch (short 'r' <> help "Sort by red")
      <*> switch (short 'g' <> help "Sort by green")
      <*> switch (short 'b' <> help "Sort by blue")
      <*> switch (short 'L' <> help "Sort by luminance")


data Opts = Opts
  { optImgPath   :: FilePath
  , optRed       :: Bool
  , optGreen     :: Bool
  , optBlue      :: Bool
  , optLuminance :: Bool
  } deriving (Eq, Show)


---------------------------------------------------------
-- Drawing images
---------------------------------------------------------


-- | A blank canvas.
data Canvas = Canvas
  { cWidth  :: Int
  , cHeight :: Int
  } deriving (Eq, Show)


-- | Tabular representation of an image.
type TabRep = [[PixelRGB8]]


-- | Create an image from a table of pixels.
fillCanvas :: Canvas -> TabRep -> Image PixelRGB8
fillCanvas Canvas {..} tbl = runST $ do
  mimg <- newMutableImage cWidth cHeight
  go 0 0 mimg
  where
    go x y mimg
        | x >= cWidth  = go 0 (y+1) mimg
        | y >= cHeight = unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg x y ((tbl !! x) !! y)
            go (x+1) y mimg


-- | Represent an image as a list of lists of RGB pixels.
makeTabRep :: Image PixelRGB8 -> TabRep
makeTabRep img = go [] (imageHeight img) (imageWidth img) (imageData img)
  where
    -- Make lists of rows
    go acc 0 _ _ = acc
    go acc h w d =
      go (acc <> [makeRow w (V.take (3*w) d)]) (h-1) w (V.drop (3*w) d)

    -- Make one row
    makeRow w = go [] (3*w)
      where
        go acc 0 _ = acc
        go acc w d =
          go (acc <> makePixel (V.toList $ V.take 3 d)) (w-3) (V.drop 3 d)

        makePixel [r, g, b] = [PixelRGB8 r g b]


---------------------------------------------------------
-- Sorting pixels
---------------------------------------------------------


-- | Comparing pixels.
type PixelOrdering = PixelRGB8 -> PixelRGB8 -> Ordering


-- | Convert CLI options to a transformation of a tabular representation of an image.
pixelSorter :: Opts -> (TabRep -> TabRep)
pixelSorter = foldr (.) id . userSortChoice


-- | Sort a decomposition by a given ordering.
sortBy :: PixelOrdering -> TabRep -> TabRep
sortBy = fmap . List.sortBy


-- | User supplied flags to corresponding sort functions.
userSortChoice :: Opts -> [TabRep -> TabRep]
userSortChoice Opts {..} = zipWith (\o f -> if o then f else id) opts pixelSort
  where
    opts = [optRed, optGreen, optBlue, optLuminance]
    pixelSort = sortBy <$> [compareRed, compareBlue, compareGreen, compareLuminance]


-- | Which pixel is more red.
compareRed :: PixelRGB8 -> PixelRGB8 -> Ordering
compareRed (PixelRGB8 r1 _ _) (PixelRGB8 r2 _ _) = compare r1 r2


-- | Which pixel is more green.
compareGreen :: PixelRGB8 -> PixelRGB8 -> Ordering
compareGreen (PixelRGB8 _ g1 _) (PixelRGB8 _ g2 _) = compare g1 g2


-- | Which pixel is more blue.
compareBlue :: PixelRGB8 -> PixelRGB8 -> Ordering
compareBlue (PixelRGB8 _ _ b1) (PixelRGB8 _ _ b2) = compare b1 b2


-- | Which pixel is brighter.
--
-- https://en.wikipedia.org/wiki/Relative_luminance
compareLuminance :: PixelRGB8 -> PixelRGB8 -> Ordering
compareLuminance a b = compare (relativeLuminance a) (relativeLuminance b)
  where
    relativeLuminance (PixelRGB8 r g b)
      = 0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b
