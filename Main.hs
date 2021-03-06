{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Data.Foldable (toList)
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Options.Applicative
import           System.FilePath.Lens
import           System.Random


main :: IO ()
main = do
  CLI {..} <- execParser cliParser
  orig <- either (error "Couldn't read image") convertRGBA8 <$> readImage cliPath
  when (invalidMask cliMask orig)
    (error $ "Image dimension is " <> show (imageHeight orig) <> " by " <> show (imageWidth orig) <> ". "
          <> "Row/column min/max must be constrained by these bounds, and min must be less than, or equal to, max.")
  let sortOptions = filter (/= Inactive)
        [cliRed, cliGreen, cliBlue, cliAlpha, cliLuminance, cliHue, cliNorm, cliStep, cliRandom]
  writeSortedImages cliPath orig
    (if cliUnbroken then makeUnbrokenSortedImage else makeSortedImage cliMask)
    sortOptions
  where
    invalidMask ImgMask {..} orig =
      let rMin = fromMaybe 0 imRowMin
          rMax = fromMaybe (imageHeight orig) imRowMax
          cMin = fromMaybe 0 imColMin
          cMax = fromMaybe (imageWidth orig) imColMax
      in rMin < 0 || rMax > imageHeight orig || cMin < 0 || cMax > imageWidth orig || (cMin > cMax || rMin > rMax)

    cliParser = info (helper <*> parseCli) (header "pixelsort")

    parseCli = CLI
      <$> strOption (long "file" <> help "Image to sort")
      <*> parseImgMask
      <*> switch (long "unbroken" <> help "Sort image that is not broken into rows")
      <*> flag'' Red (short 'r' <> help "Sort by red")
      <*> flag'' Green (short 'g' <> help "Sort by green")
      <*> flag'' Blue (short 'b' <> help "Sort by blue")
      <*> flag'' Alpha (short 'a' <> help "Sort by alpha")
      <*> flag'' Average (short 'M' <> help "Sort by average of pixel values")
      <*> flag'' Luminance (short 'L' <> help "Sort by luminance")
      <*> flag'' Hue (short 'H' <> help "Sort by hue")
      <*> flag'' Norm (short 'N' <> help "Sort by norm of the pixels considered as points in 4-dimensional space")
      <*> flag'' Step (short 'S' <> help "Sort by a step function (with 8 steps) of hue, luminance, and maximum pixel value")
      <*> flag'' Random (long "rand" <> help "Sort by random comparison of pixel properties")

    flag'' opt modOpts = flag' opt modOpts <|> pure Inactive

    parseImgMask = ImgMask
      <$> optional (option auto $ long "row-min" <> help "Row to start pixel sorting")
      <*> optional (option auto $ long "row-max" <> help "Row to end pixel sorting")
      <*> optional (option auto $ long "col-min" <> help "Column to start pixel sorting")
      <*> optional (option auto $ long "col-max" <> help "Column to end pixel sorting")


-- | The possible ways to sort a row.
data SortOption
  = Red | Green | Blue | Alpha | Average | Luminance | Hue | Norm | Step | Random | Inactive
  deriving (Eq, Show)


-- | The subset of the image to sort.
data ImgMask = ImgMask
  { imRowMin :: Maybe Int
  , imRowMax :: Maybe Int
  , imColMin :: Maybe Int
  , imColMax :: Maybe Int
  } deriving (Eq, Show)


-- | CLI.
data CLI = CLI
  { cliPath      :: FilePath
  , cliMask      :: ImgMask
  , cliUnbroken  :: Bool
  , cliRed       :: SortOption
  , cliGreen     :: SortOption
  , cliBlue      :: SortOption
  , cliAlpha     :: SortOption
  , cliAverage   :: SortOption
  , cliLuminance :: SortOption
  , cliHue       :: SortOption
  , cliNorm      :: SortOption
  , cliStep      :: SortOption
  , cliRandom    :: SortOption
  } deriving (Eq, Show)


-- | Write all images according to command line sort options.
writeSortedImages
  :: FilePath -- ^ Path to the original image.
  -> Image PixelRGBA8 -- ^ Original image.
  -> (PixelOrdering -> Image PixelRGBA8 -> Image PixelRGBA8) -- ^ Function producing the sorted image.
  -> [SortOption] -- ^ Collection of sort options.
  -> IO ()
writeSortedImages path orig sort = mapM_ (writeSortedImage path orig sort)


-- | Sort, given an option, and write the sorted image to the filesystem.
writeSortedImage
  :: FilePath -- ^ Path the original image.
  -> Image PixelRGBA8 -- ^ Original image.
  -> (PixelOrdering -> Image PixelRGBA8 -> Image PixelRGBA8) -- ^ Function producing the sorted image.
  -> SortOption -- ^ How to sort.
  -> IO ()
writeSortedImage path orig sort = \case
  Red       -> sortAndWritePng compareRed "-sorted-r"
  Green     -> sortAndWritePng compareGreen "-sorted-g"
  Blue      -> sortAndWritePng compareBlue "-sorted-b"
  Alpha     -> sortAndWritePng compareAlpha "-sorted-a"
  Average   -> sortAndWritePng compareAverage "-sorted-M"
  Luminance -> sortAndWritePng compareLuminance "-sorted-L"
  Hue       -> sortAndWritePng compareHue "-sorted-H"
  Norm      -> sortAndWritePng compareNorm "-sorted-N"
  Step      -> sortAndWritePng compareStep "-sorted-S"
  Random    -> compareRandomly >>= flip sortAndWritePng "-sorted-rand"
  Inactive  -> error "Attempted to write a sorted image with no sort option provided."
  where
    makeFileName imgPath suffix =
      let baseDir     = imgPath ^. directory
          [name, ext] = case splitOn "." $ imgPath ^. filename of
            (n:x:_) -> [n, x]
            _       -> error "Invalid filename/extension."
      in baseDir <> "/" <> name <> suffix <> "." <> ext

    sortAndWritePng ord suffix = do
      let outPath = makeFileName path suffix
      writePng outPath $ sort ord orig
      putStrLn outPath


-- | Sort the image with the given ordering.
makeSortedImage
  :: ImgMask -- ^ Subset of the image to sort.
  -> PixelOrdering -- ^ Sorting function.
  -> Image PixelRGBA8 -- ^ Image to sort.
  -> Image PixelRGBA8
makeSortedImage ImgMask {..} f img@Image {..} = runST $ do
  mimg <- unsafeThawImage img
  let rMin = fromMaybe 0 imRowMin
      rMax = fromMaybe imageHeight imRowMax
      cMin = fromMaybe 0 imColMin
      cMax = fromMaybe imageWidth imColMax
  go rMin rMax cMin cMax imageWidth imageData mimg
  where
    go r r' c c' iw d mimg
      | r >= r' = unsafeFreezeImage mimg
      | otherwise = do
          let row = makeRow (4 * c') (VS.take (4 * c') d)
              sortedRow = V.modify (VA.sortBy f) $ V.drop c row
          void $ writeRow c c' c r sortedRow mimg
          go (r + 1) r' c c' iw (VS.drop (4 * iw) d) mimg

    writeRow c c' ic r v mimg
      | c >= c' = unsafeFreezeImage mimg
      | otherwise = do
          writePixel mimg c r (v V.! (c - ic))
          writeRow (c + 1) c' ic r v mimg


-- | Sort the image without breaking it into rows of pixels.
makeUnbrokenSortedImage
  :: PixelOrdering -- ^ Sorting function.
  -> Image PixelRGBA8 -- ^ Image to sort.
  -> Image PixelRGBA8
makeUnbrokenSortedImage f img@Image {..} = runST $ do
  mimg <- unsafeThawImage img
  let rawImage = makeRow (VS.length imageData) imageData
      sortedImage = V.modify (VA.sortBy f) rawImage
  go 0 sortedImage mimg
  where
    go r d mimg
      | r >= imageHeight = unsafeFreezeImage mimg
      | otherwise = do
          void $ writeRow r 0 (V.take imageWidth d) mimg
          go (r + 1) (V.drop imageWidth d) mimg

    writeRow r c v mimg
      | c >= imageWidth = unsafeFreezeImage mimg
      | otherwise = do
          writePixel mimg c r (v V.! c)
          writeRow r (c + 1) v mimg


-- | Make one row of 'PixelRGBA8's from the image's raw representation.
makeRow
  :: Int
  -> VS.Vector Word8
  -> V.Vector PixelRGBA8
makeRow = go Seq.empty
  where
    go !acc !w !d
      | w == 0    = V.fromList $ toList acc
      | otherwise =
        go (acc Seq.|> makePixel (VS.take 4 d)) (w - 4) (VS.drop 4 d)

    makePixel d = PixelRGBA8 (d VS.! 0) (d VS.! 1) (d VS.! 2) (d VS.! 3)


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


-- | Calculate luminance.
relativeLuminance :: Fractional a => PixelRGBA8 -> a
relativeLuminance (PixelRGBA8 r g b _) =
  0.2126 * fromIntegral r + 0.7152 * fromIntegral g + 0.0722 * fromIntegral b


-- | Calculate hue.
hue :: RealFloat a => PixelRGBA8 -> a
hue (PixelRGBA8 r g b _)
  | degrees < 0 = (degrees + 360) / 360
  | otherwise   = degrees / 360
  where
    degrees =
      60 * atan2 (sqrt 3 * (fromIntegral g - fromIntegral b)) (2 * fromIntegral r - fromIntegral g - fromIntegral b)


-- | Calculate the maximum pixel value.
maxPixelValue :: (Fractional a, Ord a) => PixelRGBA8 -> a
maxPixelValue (PixelRGBA8 r g b _) = maximum [fromIntegral r, fromIntegral g, fromIntegral b]


-- | Which pixel is brighter.
--
-- https://en.wikipedia.org/wiki/Relative_luminance
compareLuminance :: PixelOrdering
compareLuminance x y = compare (relativeLuminance x) (relativeLuminance y)


-- | Which pixel is more hue-y.
--
-- https://en.wikipedia.org/wiki/Hue
compareHue :: PixelOrdering
compareHue x y = compare (hue x) (hue y)


-- | Compare by norm of the pixel considered as a point in 4-dimensional space.
compareNorm :: PixelOrdering
compareNorm x y = compare (norm x) (norm y)
  where
    norm (PixelRGBA8 r g b a) =
      sqrt $ fromIntegral r ^ 2 + fromIntegral g ^ 2 + fromIntegral b ^ 2 + fromIntegral a ^ 2


-- | Compare by a step function (with 8 steps) of hue, luminance, and maximum value.
compareStep :: PixelOrdering
compareStep x y = compare (step x) (step y)
  where
    step (PixelRGBA8 r g b a)
      | newHue `mod` 2 == 0 = 10^9 * newHue + 10^4 * newLuminance + newColorValue
      | otherwise           = 10^9 * newHue + 10^4 * (255 - newLuminance) + (8-newColorValue)
        where
          newHue = round (hue (PixelRGBA8 r g b a) * 8)
          newLuminance = round (relativeLuminance (PixelRGBA8 r g b a) )
          newColorValue = round (maxPixelValue (PixelRGBA8 r g b a) / 255 * 8)


-- | Random comparison.
compareRandomly :: IO PixelOrdering
compareRandomly = do
  n1 <- randomRIO (0, 3)
  n2 <- randomRIO (0, 3)
  return $ compare' n1 n2
    where
      compare' :: Int -> Int -> PixelRGBA8 -> PixelRGBA8 -> Ordering
      compare' 0 0 (PixelRGBA8 r1 _ _ _) (PixelRGBA8 r2 _ _ _) = compare r1 r2
      compare' 0 1 (PixelRGBA8 r1 _ _ _) (PixelRGBA8 _ g2 _ _) = compare r1 g2
      compare' 0 2 (PixelRGBA8 r1 _ _ _) (PixelRGBA8 _ _ b2 _) = compare r1 b2
      compare' 0 3 (PixelRGBA8 r1 _ _ _) (PixelRGBA8 _ _ _ a2) = compare r1 a2
      compare' 1 0 (PixelRGBA8 _ g1 _ _) (PixelRGBA8 r2 _ _ _) = compare g1 r2
      compare' 1 1 (PixelRGBA8 _ g1 _ _) (PixelRGBA8 _ g2 _ _) = compare g1 g2
      compare' 1 2 (PixelRGBA8 _ g1 _ _) (PixelRGBA8 _ _ b2 _) = compare g1 b2
      compare' 1 3 (PixelRGBA8 _ g1 _ _) (PixelRGBA8 _ _ _ a2) = compare g1 a2
      compare' 2 0 (PixelRGBA8 _ _ b1 _) (PixelRGBA8 r2 _ _ _) = compare b1 r2
      compare' 2 1 (PixelRGBA8 _ _ b1 _) (PixelRGBA8 _ g2 _ _) = compare b1 g2
      compare' 2 2 (PixelRGBA8 _ _ b1 _) (PixelRGBA8 _ _ b2 _) = compare b1 b2
      compare' 2 3 (PixelRGBA8 _ _ b1 _) (PixelRGBA8 _ _ _ a2) = compare b1 a2
      compare' 3 0 (PixelRGBA8 _ _ _ a1) (PixelRGBA8 r2 _ _ _) = compare a1 r2
      compare' 3 1 (PixelRGBA8 _ _ _ a1) (PixelRGBA8 _ g2 _ _) = compare a1 g2
      compare' 3 2 (PixelRGBA8 _ _ _ a1) (PixelRGBA8 _ _ b2 _) = compare a1 b2
      compare' 3 3 (PixelRGBA8 _ _ _ a1) (PixelRGBA8 _ _ _ a2) = compare a1 a2
      compare' _ _ _ _                                         = error "The impossible has happened"
