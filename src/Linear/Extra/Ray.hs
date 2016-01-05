{-# LANGUAGE RecordWildCards #-}
module Linear.Extra.Ray where
import Linear.Extra.Pose
import Linear
import Control.Lens

data Ray a = Ray { rayOrigin    :: !(V3 a)
                 , rayDirection :: !(V3 a)
                 } deriving (Show, Eq)

-- | Returns a ray from the given Pose's position along the Pose's orientation
poseToRay :: (RealFloat a, Conjugate a) => Pose a -> V3 a -> Ray a
poseToRay pose basis = Ray origin direction
  where origin    = pose ^. posPosition
        direction = rotate (pose ^. posOrientation) basis

pointOnRay :: (RealFloat a) => Ray a -> a -> V3 a
pointOnRay Ray{..} magnitude = (rayOrigin + rayDirection * realToFrac magnitude)

-- | Ray to Oriented Bounding Box intersection.
-- AABB argument should be untransformed.
-- Returns distance from the ray origin if a collision is found.
-- adapted from http://www.opengl-tutorial.org/miscellaneous/clicking-on-objects/picking-with-custom-ray-obb-function/
rayOBBIntersection :: (RealFloat a, Epsilon a) => Ray a -> (V3 a, V3 a) -> M44 a -> Maybe a
rayOBBIntersection ray@Ray{..} (aabbMin, aabbMax) model44 = result
    where
        (tMin0, tMax0) = (0, 1000000)
        boxPositionWorld = model44 ^. translation
        delta            = boxPositionWorld - rayOrigin
        xAxis            = model44 ^. column _x . _xyz
        yAxis            = model44 ^. column _y . _xyz
        zAxis            = model44 ^. column _z . _xyz

        (tMin1, tMax1, rX) = testAxis (aabbMin ^. _x, aabbMax ^. _x) (tMin0, tMax0) xAxis
        (tMin2, tMax2, rY) = testAxis (aabbMin ^. _y, aabbMax ^. _y) (tMin1, tMax1) yAxis
        (tMin3, tMax3, rZ) = testAxis (aabbMin ^. _z, aabbMax ^. _z) (tMin2, tMax2) zAxis

        result             = if and [rX,rY,rZ] then Just tMin3 else Nothing

        testAxis (aabbAxisMin, aabbAxisMax) (tMin, tMax) axis =
            let e = dot axis         delta -- Intersection with the "left" plane
                f = dot rayDirection axis  -- Intersection with the "right" plane
            in if nearZero f
                then 
                    ( tMin
                    , tMax
                    , not (aabbAxisMin - e > 0 || aabbAxisMax - e < 0)
                    )
                else 
                    let t1 = (e + aabbAxisMin) / f
                        t2 = (e + aabbAxisMax) / f
                        (t1', t2') = if t1 > t2 then (t2, t1) else (t1, t2)
                        tMax' = if t2' < tMax then t2' else tMax
                        tMin' = if t1' > tMin then t1' else tMin
                    in ( tMin'
                       , tMax'
                       , not (tMin' > tMax')
                       )

