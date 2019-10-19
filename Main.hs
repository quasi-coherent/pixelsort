{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.Foldable (toList)
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable as VS
import           Data.Word (Word8)
import           Debug.Trace
import           Options.Applicative
import           System.FilePath.Lens
import           System.Random


main :: IO ()
main = do
  CLI {..} <- execParser cliParser
  orig <- either (error "Couldn't read image") convertRGBA8 <$> readImage cliPath
  when (invalidMask cliMask orig)
    (error $ "Image dimension is " <> show (imageHeight orig) <> " by " <> show (imageWidth orig) <> ". "
          <> "Row/column min/max must be constrained by these bounds, and min must be less than max.")
  let sortOptions = filter (/= Inactive)
        [cliRed, cliGreen, cliBlue, cliAlpha, cliLuminance, cliHue, cliNorm, cliStep, cliRandom]
  writeSortedImages cliPath orig
    (case (cliHParts, cliVParts) of
       (Just val, _) -> makeHPartsSortedImage val cliMask
       (_, Just val) -> makeVPartsSortedImage val cliMask
       (_, _)        -> makeHPartsSortedImage 0 cliMask)
    sortOptions
  where
    invalidMask ImgMask {..} orig =
      let rMin = fromMaybe 0 imRowMin
          rMax = fromMaybe ((imageHeight orig)-1) imRowMax
          cMin = fromMaybe 0 imColMin
          cMax = fromMaybe ((imageWidth orig)-1) imColMax
      in rMin < 0 || rMax >= imageHeight orig || cMin < 0 || cMax >= imageWidth orig || (cMin > cMax || rMin > rMax)

    cliParser = info (helper <*> parseCli) (header "pixelsort")

    parseCli = CLI
      <$> strOption (long "file" <> help "Image to sort")
      <*> parseImgMask
      <*> optional (option auto $ long "h-par" <> help "Sort image that is broken into X horizontal partitions" <> metavar "INT")
      <*> optional (option auto $ long "v-par" <> help "Sort image that is broken into X vertical partitions" <> metavar "INT")
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
      <$> optional (option auto $ long "row-min" <> help "Row to start pixel sorting" <> metavar "INT")
      <*> optional (option auto $ long "row-max" <> help "Row to end pixel sorting" <> metavar "INT")
      <*> optional (option auto $ long "col-min" <> help "Column to start pixel sorting" <> metavar "INT")
      <*> optional (option auto $ long "col-max" <> help "Column to end pixel sorting" <> metavar "INT")

data Rectangle = Rectangle
  { x1 :: Int
  , y1 :: Int
  , x2 :: Int
  , y2 :: Int
} deriving (Show)

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
  , cliHParts    :: Maybe Int
  , cliVParts    :: Maybe Int
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
  Red       -> writePng (makeFileName path "-sorted-r") $ sort compareRed orig
  Green     -> writePng (makeFileName path "-sorted-g") $ sort compareGreen orig
  Blue      -> writePng (makeFileName path "-sorted-b") $ sort compareBlue orig
  Alpha     -> writePng (makeFileName path "-sorted-a") $ sort compareAlpha orig
  Average   -> writePng (makeFileName path "-sorted-M") $ sort compareAverage orig
  Luminance -> writePng (makeFileName path "-sorted-L") $ sort compareLuminance orig
  Hue       -> writePng (makeFileName path "-sorted-H") $ sort compareHue orig
  Norm      -> writePng (makeFileName path "-sorted-N") $ sort compareNorm orig
  Step      -> writePng (makeFileName path "-sorted-S") $ sort compareStep orig
  Random    -> compareRandomly >>= \ord ->
    writePng (makeFileName path "-sorted-rand") $ sort ord orig
  Inactive  -> error "Attempted to write a sorted image with no sort option provided."
  where
    makeFileName imgPath suffix =
      let baseDir     = imgPath ^. directory
          [name, ext] = case splitOn "." $ imgPath ^. filename of
            (n:x:_) -> [n, x]
            _       -> error "Invalid filename/extension."
      in baseDir <> "/" <> name <> suffix <> "." <> ext

convertToPXVec :: VS.Vector Word8 -> V.Vector PixelRGBA8
convertToPXVec = go Seq.empty
  where
    go !acc !d
      | VS.length d == 0    = V.fromList $ toList acc
      | otherwise =
        go (acc Seq.|> makePixel (VS.take 4 d)) (VS.drop 4 d)

    makePixel d = PixelRGBA8 (d VS.! 0) (d VS.! 1) (d VS.! 2) (d VS.! 3)


-- | Take n and skip m of the image's raw represantation
-- | and make one row 'PixelRGBA8 from that - O(numofpixels)
chopAndMakeRow
  :: Int
  -> Int
  -> V.Vector PixelRGBA8
  -> V.Vector PixelRGBA8
chopAndMakeRow = go Seq.empty 0
  where
    go !acc !nn !n !m !d
      | V.length d == 0 = V.fromList $ toList acc
      | n == 0 = go acc nn nn m (V.drop m d)
      | otherwise =
        go (acc Seq.|> (d V.! 0)) (max nn n) (n-1) m (V.drop 1 d)


getRectangleAsRow :: Int -> V.Vector PixelRGBA8 -> Rectangle -> V.Vector PixelRGBA8
getRectangleAsRow width d rt =
  chopAndMakeRow recWidth (width - recWidth) (traceShow (show (V.length d)) V.drop (x1 rt) choppedD)
  where recWidth = x2 rt - x1 rt + 1
        choppedD = chopOfTopAndBottom (y1 rt) (y2 rt) width d

getRectangleFromCols :: Int -> Int -> Int -> Int -> Rectangle
getRectangleFromCols cMin cMax rMin rMax = Rectangle cMin rMin cMax rMax

chopOfTopAndBottom :: Int -> Int -> Int -> V.Vector PixelRGBA8 -> V.Vector PixelRGBA8
chopOfTopAndBottom rMin rMax iw d = v
  where v = V.drop (traceShow dro dro) $ V.take (traceShow iw tak) d
        tak = (rMax+1) * iw
        dro = iw * rMin

writeRectangle :: (Control.Monad.Primitive.PrimMonad m, Pixel a)
  => MutableImage (Control.Monad.Primitive.PrimState m) a -> V.Vector a -> Rectangle -> m (Image a)
writeRectangle mmg vc rt =
  writeRow (x1 rt) (x2 rt) (x1 rt) (y1 rt) (y2 rt) vc mmg
  where
    writeRow c c' ic r r' v mimg
      -- | VS.length (traceShow ("nn: " ++ show nn ++ " n: " ++ show n ++ " m: " ++ show m) d) == 0 = V.fromList $ toList acc
        | V.length v == 0 = unsafeFreezeImage mimg
        | c > (traceShow ("c:" ++show c ++ "c':" ++show c' ++ "ic:" ++show ic ++ "r:" ++show r ++ "r':" ++show r' ++ "vl:" ++show (V.length v))
          c') = writeRow ic c' ic (r + 1) r' (V.drop (c'-ic+1) v) mimg
        | otherwise = do
            writePixel mimg c r (v V.! (c - ic))
            writeRow (c + 1) c' ic r r' v mimg


-- | Sort the image partitioned into horizontal partitions
makeHPartsSortedImage
  :: Int
  -> ImgMask
  -> PixelOrdering -- ^ Sorting function.
  -> Image PixelRGBA8 -- ^ Image to sort.
  -> Image PixelRGBA8
makeHPartsSortedImage ch ImgMask {..} f img@Image {..} = runST $ do
  mimg <- unsafeThawImage img
  let rMin = fromMaybe 0 imRowMin
      rMax = fromMaybe (imageHeight-1) imRowMax
      cMin = fromMaybe 0 imColMin
      cMax = fromMaybe (imageWidth-1) imColMax
  let ct = getRectangleFromCols cMin cMax rMin rMax
  let width = (x2 ct) - (x1 ct) + 1
  let height = (y2 ct) - (y1 ct) + 1
  let pxData = convertToPXVec imageData
  let cutoutData = getRectangleAsRow imageWidth pxData ct
  let c = case ch < 1 || ch > height of
        True  -> height
        False -> ch
  --let chunkHeight = height `div` (traceShow (imageHeight, imageWidth, height, width, ct ) c)
  let chunkHeight = height `div` (traceShow (show (V.length cutoutData) ++ "---" ++ show ct) c)
  go 0 chunkHeight cutoutData height width (x1 ct) (y1 ct) mimg
  where
    go r chh d h w offx offy mimg
      | r + 1 > h = unsafeFreezeImage mimg
      | otherwise = do
          let rows = if chh * 2 > h - r then h - r else chh
          let rt = getRectangleFromCols 0 (w - 1 ) r (r + rows -1)
          let dd = getRectangleAsRow w d (traceShow ("dddd-" ++ show rt ++ "www" ++ show w ++ " l " ++show (V.length d) ++ " h " ++ show h ++ " r " ++ show r ) rt)
              --sortedChunk = dd
              sortedChunk =  V.modify (VA.sortBy f) (traceShow "aaaa" dd)
          void $ writeRectangle mimg sortedChunk (traceShow ("sdlkfjdslkfj" ++ show (V.length sortedChunk)) Rectangle (x1 rt + offx) (y1 rt + offy) (x2 rt + offx ) (y2 rt + offy))
          go (r + chh) chh d h w offx offy mimg

-- | Sort the image partitioned into vertical partitions
makeVPartsSortedImage
  :: Int
  -> ImgMask
  -> PixelOrdering -- ^ Sorting function.
  -> Image PixelRGBA8 -- ^ Image to sort.
  -> Image PixelRGBA8
makeVPartsSortedImage ch ImgMask {..} f img@Image {..} = runST $ do
  mimg <- unsafeThawImage img
  let rMin = fromMaybe 0 imRowMin
      rMax = fromMaybe (imageHeight-1) imRowMax
      cMin = fromMaybe 0 imColMin
      cMax = fromMaybe (imageWidth-1) imColMax
  let ct = getRectangleFromCols cMin cMax rMin rMax
  let width = (x2 ct) - (x1 ct) + 1
  let height = (y2 ct) - (y1 ct) + 1
  let pxData = convertToPXVec imageData
  let cutoutData = getRectangleAsRow imageWidth pxData ct
  let c = case ch < 1 || ch > height of
        True  -> height
        False -> ch
  let chunkWidth = width `div` (traceShow ("cutoutdata" ++ show (V.length cutoutData)) c)
  go 0 chunkWidth cutoutData height width (x1 ct) (y1 ct) mimg
  where
    go c chw d h w offx offy mimg
      | c + 1 > w = unsafeFreezeImage mimg
      | otherwise = do
          let rl = if chw * 2 > w - c then w - c else chw
          let rt = getRectangleFromCols c (c+rl-1) 0 (h-1)
          let dd = getRectangleAsRow w d (traceShow ("dddd-" ++ show rt ++ show rl) rt)
              --sortedChunk = dd
              sortedChunk = V.modify (VA.sortBy f) dd
              -- sortedChunk = V.modify (VA.sortBy f) (traceShow (show chw ++ "aaaa" ) dd)
          --void $ writeRectangle mimg sortedChunk rt
          void $ writeRectangle mimg sortedChunk (traceShow ("sdlkfjdslkfj" ++ show (V.length sortedChunk)) Rectangle (x1 rt + offx) (y1 rt + offy) (x2 rt + offx ) (y2 rt + offy))
          go (c + rl) chw d h w offx offy mimg

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
