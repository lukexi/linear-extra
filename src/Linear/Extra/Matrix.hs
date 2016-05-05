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

-- Subtract b from a
subtractPose :: (Floating a, Ord a, Epsilon a) => Pose a -> Pose a -> Pose a
subtractPose a b = poseFromMatrix $
    transformationFromPose a `subtractMatrix` transformationFromPose b

addPose :: (Floating a, Ord a, Epsilon a) => Pose a -> Pose a -> Pose a
addPose a b = poseFromMatrix $
    transformationFromPose a `addMatrix` transformationFromPose b


addMatrix :: (Floating a, Ord a, Epsilon a) => M44 a -> M44 a -> M44 a
addMatrix a b = a !*! b

subtractMatrix :: (Floating a, Ord a, Epsilon a) => M44 a -> M44 a -> M44 a
subtractMatrix a b = inv44 b !*! a


quatToAxisAngle :: (RealFloat a) => Quaternion a -> (V3 a, a)
quatToAxisAngle (Quaternion qw (V3 qx qy qz)) = (V3 x y z, angle)
    where
        angle = 2 * acos qw
        x = qx / s'
        y = qy / s'
        z = qz / s'
        s = sqrt (1-qw*qw)
        s' | s < 0.001 = 1
           | otherwise = s

quatToEuler ::  (RealFloat a) => Quaternion a -> V3 a
quatToEuler (Quaternion qw (V3 qx qy qz)) = V3 pitch yaw roll
    where
        qz2   = qz^2
        yaw   = atan2 (2 * qy * qw - 2 * qx * qz) (1 - 2 * qy^2 - 2 * qz2)
        pitch = asin  (2 * qx * qy + 2 * qz * qw)
        roll  = atan2 (2 * qx * qw - 2 * qy * qz) (1 - 2 * qx^2 - 2 * qz2)
