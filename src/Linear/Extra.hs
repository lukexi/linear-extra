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


