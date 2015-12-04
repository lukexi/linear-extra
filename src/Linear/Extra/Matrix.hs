module Linear.Extra.Matrix where
import Linear
import Data.Maybe
import Linear.Extra.Pose
import Control.Lens
scaleMatrix :: (Num a) => V3 a -> M44 a
scaleMatrix (V3 x y z) = V4 (V4 x 0 0 0)
                            (V4 0 y 0 0)
                            (V4 0 0 z 0)
                            (V4 0 0 0 1)

translateMatrix :: (Num a) => V3 a -> M44 a
translateMatrix v = identity & translation .~ v

m44FromList :: [a] -> M44 a
m44FromList [a,b,c,d
            ,e,f,g,h
            ,i,j,k,l
            ,m,n,o,p] = V4 (V4 a b c d)
                           (V4 e f g h)
                           (V4 i j k l)
                           (V4 m n o p)
m44FromList _ = error "Invalid list length for m44FromList"

-- quaternionFromMatrix :: M44 a -> Quaternion a
quaternionFromMatrix matrix = Quaternion w (V3 x y z)
  where
    V3 (V3 m00 m01 m02)
       (V3 m10 m11 m12)
       (V3 m20 m21 m22) = matrix ^. _m33

    w = sqrt (max 0 (1 + m00 + m11 + m22)) / 2
    x = copySignOf (m21 - m12) $ sqrt (max 0 (1 + m00 - m11 - m22)) / 2
    y = copySignOf (m02 - m20) $ sqrt (max 0 (1 - m00 + m11 - m22)) / 2
    z = copySignOf (m10 - m01) $ sqrt (max 0 (1 - m00 - m11 + m22)) / 2

copySignOf b a = if signum a == signum b then a else negate a


poseFromMatrix matrix = Pose 
  (matrix ^. translation) 
  (quaternionFromMatrix matrix)


worldPointToModelPoint model worldPoint = pointOnModel
  where
    invModel     = inv44 model
    pointOnModel = normalizePoint (invModel !* point worldPoint)
