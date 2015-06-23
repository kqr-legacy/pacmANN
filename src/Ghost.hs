{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Ghost
  ( Ghost(Ghost), position, validExits, progress
  , mkGhost, nextPos, prevPos)
  where

import Data.Maybe      (fromMaybe)
import Lens.Family2
import Lens.Family2.TH (makeLenses)

import IPoint          (IPoint)


data Ghost = Ghost
    { _position   :: IPoint
    , _validExits :: [IPoint]
    , _progress   :: Float
    } deriving Show

$(makeLenses ''Ghost)


mkGhost :: IPoint -> Ghost
mkGhost p =
  Ghost p [] 0


nextPos :: Getter' Ghost IPoint
nextPos =
  getPos firstOf


prevPos :: Getter' Ghost IPoint
prevPos =
  getPos lastOf


getPos :: (Fold' Ghost IPoint -> Ghost -> Maybe IPoint) -> Getter' Ghost IPoint
getPos pick =
  to (fromMaybe . view position <*> pick (validExits . traverse))

