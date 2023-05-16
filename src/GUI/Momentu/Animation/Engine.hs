{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Animation.Engine
    ( Config(..), acTimePeriod, acRemainingRatioInPeriod, acSpiral
    , SpiralAnimConf(..), sTan, sThreshold
    , currentFrame
    , Dest(..)
    , State
    , initialState, isAnimating
    , AdvancedAnimation(..), _AnimationComplete, _NewState
    , advanceAnimation, clockedAdvanceAnimation
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GUI.Momentu.Animation (Image, iRect, iElemId, iUnitImage, Frame(..), frameImages, images, R)
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

data SpiralAnimConf = SpiralAnimConf
    { _sTan :: R
    , _sThreshold :: R
    }
Lens.makeLenses ''SpiralAnimConf

data Config = Config
    { _acTimePeriod :: NominalDiffTime
    , _acRemainingRatioInPeriod :: R
    , _acSpiral :: SpiralAnimConf
    }
Lens.makeLenses ''Config

data Interpolation
    = Deleting Image
      -- ^ An image that is interpolating from its current state to nothingness
    | Modifying {-cur-}Image {-dest-}Rect
      -- ^ An image that is interpolating from the cur Rect towards the dest Image
    | Final Image
      -- ^ An image that finished interpolating
Lens.makePrisms ''Interpolation

interpolationImage :: Lens' Interpolation Image
interpolationImage f (Deleting img) = f img <&> Deleting
interpolationImage f (Modifying curImg destRect) = f curImg <&> (`Modifying` destRect)
interpolationImage f (Final img) = f img <&> Final

-- | Destination: where the animation is going to.
data Dest = Dest
    { _dTime :: UTCTime
        -- ^ Time of queueing the destination.
        -- If for any reason the engine didn't get to react immediately it should jump ahead in animation,
        -- or even skip animation, instead (to preserve proper response times).
    , _dFrame :: Frame
    }
Lens.makeLenses ''Dest

data State = State
    { _asCurSpeedHalfLife :: !NominalDiffTime
    , _asCurTime :: !UTCTime
    , _asInterpolations :: [Interpolation]
    }
Lens.makeLenses ''State

initialState :: Frame -> IO State
initialState initFrame =
    getCurrentTime <&>
    \curTime -> State
    { _asCurSpeedHalfLife = 0
    , _asCurTime = curTime
    , _asInterpolations = initFrame ^. frameImages <&> Final
    }

data AdvancedAnimation = AnimationComplete | NewState State
Lens.makePrisms ''AdvancedAnimation

isAnimating :: State -> Bool
isAnimating = Lens.has (asInterpolations . traverse . Lens.filtered (Lens.nullOf _Final))

rot90 :: Num a => Vector2 a -> Vector2 a
rot90 (Vector2 x y) = Vector2 y (negate x)

advanceInterpolation :: SpiralAnimConf -> R -> Interpolation -> Maybe Interpolation
advanceInterpolation _ _ x@Final{} = Just x
advanceInterpolation spiral movement (Modifying curImage destRect)
    | rectDistance (curImage ^. iRect) destRect < equalityThreshold =
        curImage & iRect .~ destRect & Final & Just
    | otherwise =
        curImage
        & iRect .~
            Rect
            (curTopLeft +
                animSpeed *
                ( posDiff + (rot90 (trim <$> posDiff <*> destSize)
                    <&> (* spiral ^. sTan))))
            (animSpeed * destSize + (1 - animSpeed) * curSize)
        & (`Modifying` destRect) & Just
    where
        equalityThreshold = 0.2
        animSpeed = pure movement
        posDiff = destTopLeft - curTopLeft
        trim x s
            | abs x < t = 0
            | otherwise = x - t * signum x
            where
                t = s * spiral ^. sThreshold
        Rect destTopLeft destSize = destRect
        Rect curTopLeft curSize = curImage ^. iRect
advanceInterpolation _ movement (Deleting img)
    | Vector2.sqrNorm (img ^. iRect . Rect.size) < 1 = Nothing
    | otherwise =
        img
        & iRect . Rect.centeredSize *~ pure (1 - movement)
        & Deleting & Just

advanceInterpolations :: SpiralAnimConf -> R -> [Interpolation] -> [Interpolation]
advanceInterpolations spiral = mapMaybe . advanceInterpolation spiral

nextInterpolations :: SpiralAnimConf -> R -> Maybe Frame -> [Interpolation] -> Maybe [Interpolation]
nextInterpolations spiral movement Nothing interpolations
    | all (Lens.has _Final) interpolations = Nothing
    | otherwise = advanceInterpolations spiral movement interpolations & Just
nextInterpolations spiral movement (Just dest) interpolations =
    setNewDest dest interpolations & advanceInterpolations spiral movement & Just

advanceAnimation ::
    Real a => SpiralAnimConf -> a -> Maybe Frame -> UTCTime -> State -> AdvancedAnimation
advanceAnimation spiral elapsed mNewDestFrame curTime animState =
    nextInterpolations spiral progress mNewDestFrame (animState ^. asInterpolations)
    <&> (\newInterpolations -> animState & asInterpolations .~ newInterpolations)
    <&> asCurTime .~ curTime
    & maybe AnimationComplete NewState
    where
        progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac (animState ^. asCurSpeedHalfLife))

desiredFrameRate :: Double
desiredFrameRate = 60

clockedAdvanceAnimation ::
    Config -> Maybe Dest -> State -> IO AdvancedAnimation
clockedAdvanceAnimation (Config timePeriod ratio spiral) mNewFrame animState =
    getCurrentTime <&>
    \curTime ->
    case mNewFrame of
    Just dest ->
        animState
        & asCurSpeedHalfLife .~ timeRemaining / realToFrac (logBase 0.5 ratio)
        & advanceAnimation spiral elapsed (Just (dest ^. dFrame)) curTime
        where
            -- Retroactively pretend animation started a little bit
            -- sooner so there's already a change in the first frame
            elapsed = 1.0 / desiredFrameRate
            timeRemaining =
                max 0 $
                diffUTCTime
                (addUTCTime timePeriod (dest ^. dTime))
                curTime
    Nothing ->
        advanceAnimation spiral (curTime `diffUTCTime` (animState ^. asCurTime))
        Nothing curTime animState

frameOfInterpolations :: [Interpolation] -> Frame
frameOfInterpolations interpolations =
    interpolations ^.. traverse . interpolationImage & Frame

currentFrame :: State -> Frame
currentFrame = frameOfInterpolations . _asInterpolations

rectDistance :: Rect -> Rect -> R
rectDistance ra rb =
    liftA2 max
    (abs (ra ^. Rect.topLeft - rb ^. Rect.topLeft))
    (abs (ra ^. Rect.bottomRight - rb ^. Rect.bottomRight))
    & Vector2.uncurry max

setNewDest :: Frame -> [Interpolation] -> [Interpolation]
setNewDest destFrame interpolations =
    go (curFrame ^. frameImages) (destFrame ^. frameImages)
    where
        go (c:cs) (d:ds)
            | c ^. iElemId == d ^. iElemId =
                modifying d (c ^. iRect) : go cs ds
        go (c:cs) ds
            | destIds ^. Lens.contains (c ^. iElemId) =
                go cs ds -- c will be treated in its new position
            | otherwise =
                Deleting c : go cs ds
        go [] ds = map goDest ds
        goDest d =
            curRects ^. Lens.at (d ^. iElemId)
            & fromMaybe (Rect (d ^. iRect . Rect.center) 0)
            & modifying d
        modifying destImage prevRect =
            Modifying (rImg & iRect .~ prevRect) (destImage ^. iRect)
            where
                rImg
                    | duplicateDestIds ^. Lens.contains (destImage ^. iElemId)
                        = destImage & iUnitImage %~ (<>) redX
                    | otherwise = destImage
                redX = Draw.tint red unitX
        curFrame = frameOfInterpolations interpolations
        curRects =
            do
                img <- curFrame ^. frameImages
                [(img ^. iElemId, img ^. iRect)]
            & Map.fromList
        sortedDestIds = destFrame ^.. images . iElemId & List.sort
        duplicateDestIds = List.group sortedDestIds >>= tail & Set.fromAscList
        destIds = Set.fromAscList sortedDestIds

unitX :: Draw.Image ()
unitX =
    Draw.line (0, 0) (1, 1) <>
    Draw.line (1, 0) (0, 1)
    & void

red :: Draw.Color
red = Draw.Color 1 0 0 1

