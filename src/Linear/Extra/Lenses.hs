module Linear.Extra.Lenses where
import Linear
import Control.Lens

-- | A set of swizzlers from shader land
_xyy, _yxy, _yyx :: R3 t => Getter (t a) (V3 a)
_xyy f = _xyz $ \(V3 a b c) -> f (V3 a b b) <&> \(V3 a' b' _) -> V3 a' b' b'
{-# INLINE _xyy #-}

_yxy f = _xyz $ \(V3 a b c) -> f (V3 b a b) <&> \(V3 a' b' _) -> V3 b' a' b'
{-# INLINE _yxy #-}

_yyx f = _xyz $ \(V3 a b c) -> f (V3 b b a) <&> \(V3 a' b' _) -> V3 b' b' a'
{-# INLINE _yyx #-}
