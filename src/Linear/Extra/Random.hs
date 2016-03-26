{-# OPTIONS_GHC -fno-warn-orphans #-}
module Linear.Extra.Random where

import Control.Monad.Random
import Linear
import Control.Lens

instance Random r => Random (V2 r) where
    randomR (a,b) g = flip runRand g $ do
        x <- getRandomR (a^._x,b^._x)
        y <- getRandomR (a^._y,b^._y)
        return (V2 x y)
    random g = flip runRand g $ do
        x <- getRandom
        y <- getRandom
        return (V2 x y)

instance Random r => Random (V3 r) where
    randomR (a,b) g = flip runRand g $ do
        x <- getRandomR (a^._x,b^._x)
        y <- getRandomR (a^._y,b^._y)
        z <- getRandomR (a^._z,b^._z)
        return (V3 x y z)
    random g = flip runRand g $ do
        x <- getRandom
        y <- getRandom
        z <- getRandom
        return (V3 x y z)

instance Random r => Random (V4 r) where
    randomR (a,b) g = flip runRand g $ do
        x <- getRandomR (a^._x,b^._x)
        y <- getRandomR (a^._y,b^._y)
        z <- getRandomR (a^._z,b^._z)
        w <- getRandomR (a^._w,b^._w)
        return (V4 x y z w)
    random g = flip runRand g $ do
        x <- getRandom
        y <- getRandom
        z <- getRandom
        w <- getRandom
        return (V4 x y z w)
