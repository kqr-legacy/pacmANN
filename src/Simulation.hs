{-# LANGUAGE TemplateHaskell #-}

module Simulation
  ( Simulation(Simulation), tick, speed, randomGen, level
  , Level(Level), walls, ghosts
  ) where

import Control.Monad.Random (StdGen)
import Lens.Family2.TH      (makeLenses)

import IPoint               (IPoint)
import Ghost                (Ghost())


data Level = Level
    { _walls  :: [IPoint]
    , _ghosts :: [Ghost]
    } deriving Show

$(makeLenses ''Level)


data Simulation = Simulation
    { _tick      :: Float
    , _speed     :: Float
    , _randomGen :: StdGen
    , _level     :: Level
    } deriving Show

$(makeLenses ''Simulation)

