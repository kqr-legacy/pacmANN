{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module IPoint (IPoint, point) where

import Graphics.Gloss       (Point)


-- Integer version of Graphics.Gloss.Point, which is Float-based.
type IPoint = (Int, Int)

instance Num IPoint where
    (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    (-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
    (*) (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
    signum (x, y)         = (signum x, signum y)
    abs    (x, y)         = (abs x, abs y)
    negate (x, y)         = (negate x, negate y)
    fromInteger x         = (fromInteger x, fromInteger x)

point :: IPoint -> Point
point (x, y) = (fromIntegral x, fromIntegral y)

