module Linear.Extra
  ( module Linear.Extra
  , module Exports
  ) where

import Linear                             as Exports
import Linear.Extra.Pose                  as Exports
import Linear.Extra.CompressedQuaternion  as Exports


scaleMatrix :: (Num a) => V3 a -> M44 a
scaleMatrix (V3 x y z) = V4 (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)


interpolatePoses :: (Num a, Fractional a, RealFloat a) => Pose a -> Pose a -> Pose a
interpolatePoses (Pose p1 o1) (Pose p2 o2) =
  Pose (lerp 0.5 p1 p2) (slerp o1 o2 0.5)
