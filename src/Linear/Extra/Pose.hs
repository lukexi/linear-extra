{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}

module Linear.Extra.Pose where

import Linear
import Data.Binary
import GHC.Generics
import Control.Lens

data Pose a = Pose
  { _posPosition    :: !(V3 a)
  , _posOrientation :: !(Quaternion a)
  } deriving (Generic, Binary, Show, Eq, Ord)
makeLenses ''Pose

newPose :: (Floating a, Epsilon a, Num a) => Pose a
newPose = Pose 
  { _posPosition = V3 0 0 0
  , _posOrientation = axisAngle (V3 0 1 0) 0
  }

transformationFromPose :: (Floating a, Epsilon a, Num a) => Pose a -> M44 a
transformationFromPose pose = mkTransformation (pose ^. posOrientation) (pose ^. posPosition)

shiftBy :: (Num a, RealFloat a, Conjugate a) => V3 a -> Pose a -> Pose a
shiftBy vec pose = pose & posPosition +~ rotate (pose ^. posOrientation) vec

addPoses :: (Num a, RealFloat a) => Pose a -> Pose a -> Pose a
addPoses basePose addedPose = 
  let Pose basePosition baseOrientation = basePose
      Pose addPosition  addOrientation  = addedPose
  in  Pose 
    (addPosition    + basePosition) 
    (addOrientation * baseOrientation) 
    -- quat rotation order must be rotation*original


interpolatePoses :: (Num a, Fractional a, RealFloat a) => Pose a -> Pose a -> Pose a
interpolatePoses (Pose p1 o1) (Pose p2 o2) =
  Pose (lerp 0.5 p1 p2) (slerp o1 o2 0.5)
