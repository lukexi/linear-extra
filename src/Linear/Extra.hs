module Linear.Extra
  ( module Linear.Extra
  , module Exports
  ) where

import Linear                             as Exports
import Linear.Extra.Pose                  as Exports
import Linear.Extra.CompressedQuaternion  as Exports

import Data.Maybe

scaleMatrix :: (Num a) => V3 a -> M44 a
scaleMatrix (V3 x y z) = V4 (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)


m44FromList :: [a] -> M44 a
m44FromList [a,b,c,d
            ,e,f,g,h
            ,i,j,k,l
            ,m,n,o,p] = V4 (V4 a b c d)
                           (V4 e f g h)
                           (V4 i j k l)
                           (V4 m n o p)
m44FromList _ = error "Invalid list length for m44FromList"

safeInv44 :: (Fractional a, Epsilon a) => M44 a -> M44 a
safeInv44 matrix = fromMaybe matrix (inv44 matrix)

