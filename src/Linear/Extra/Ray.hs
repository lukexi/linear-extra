module Linear.Extra.Ray where
import Linear.Extra.Pose
import Linear
import Control.Lens

data Ray a = Ray { rayFrom :: !(V3 a)
                 , rayTo   :: !(V3 a)
                 } deriving (Show, Eq)

-- | Returns a ray from the given Pose's position along the Pose's orientation
poseToRay :: (RealFloat a, Conjugate a) => Pose a -> Ray a
poseToRay pose = Ray fromPos toPos
  where fromPos = pose ^. posPosition
        toPos   = rotate (pose ^. posOrientation) (fromPos & _z -~ 1000)
