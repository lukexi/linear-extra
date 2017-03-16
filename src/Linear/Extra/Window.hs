{-# LANGUAGE ViewPatterns #-}
module Linear.Extra.Window where
import Linear
import Control.Lens
windowPosToWorldPos :: (Epsilon a, Real a, Floating a)
                    => V2 a
                    -> M44 a
                    -> V2 a
                    -> a
                    -> V3 a
windowPosToWorldPos winSize viewProj coord depth = rayStart + rayDir * realToFrac depth
    where
        V2 xNDC yNDC = win2Ndc coord winSize
        rayStart = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
        rayEnd   = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
        rayDir   = normalize (rayEnd ^-^ rayStart)
        -- Converts from window coordinates (origin top-left) to normalized device coordinates
        win2Ndc (V2 x y) (V2 w h) =
            V2 ((x / w - 0.5) * 2)
               ((((h - y) / h) - 0.5) * 2)
        -- Converts from normalized device coordinates to world coordinates
        ndc2Wld i = hom2Euc (invViewProj !* i)
        -- Converts from homogeneous coordinates to Euclidean coordinates
        hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
        invViewProj = inv44 viewProj
