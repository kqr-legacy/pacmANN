{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Control.Monad.Random (StdGen)
import Lens.Family2
import Lens.Family2.TH
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

toPoint :: IPoint -> Point
toPoint (x, y) = (fromIntegral x, fromIntegral y)


data Simulation = Simulation
    { _tick      :: Float
    , _randomGen :: StdGen
    , _level     :: Level
    } deriving Show

data Level = Level
    { _walls  :: [IPoint]
    , _ghosts :: [Ghost]
    } deriving Show


data Ghost = Ghost
    { _position  :: IPoint
    , _direction :: Direction
    , _progress  :: Float
    } deriving Show


data Direction = DLeft | DUp | DRight | DDown deriving (Show, Enum, Eq)
allDirections = [DLeft, DUp, DRight, DDown]


-- Lens all the things!!
$(makeLenses ''Simulation)
$(makeLenses ''Level)
$(makeLenses ''Ghost)


-- Get the next position of the ghost, if it continues in the current direction
-- from the current position
nextPos :: Ghost -> IPoint
nextPos ghost =
    ghost ^. position + dirVector (ghost ^. direction)

-- Convert a direction to a relative position
dirVector :: Direction -> IPoint
dirVector dir =
    case dir of
        DLeft  -> (-1,  0)
        DUp    -> ( 0, -1)
        DRight -> ( 1,  0)
        DDown  -> ( 0,  1)


-- Get the opposite direction of the current one
dirOpposite :: Direction -> Direction
dirOpposite dir =
    case dir of
        DLeft -> DRight
        DUp -> DDown
        DRight -> DLeft
        DDown -> DUp

