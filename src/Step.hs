module Step (step) where

import Data.List             (delete)
import Control.Monad         (guard)
import Control.Monad.Random  (StdGen, Rand, runRand)
import System.Random.Shuffle (shuffleM)
import Lens.Family2

import IPoint                (IPoint)
import Simulation
import Ghost


step :: anytype -> Float -> Simulation -> Simulation
step _ delta sim =
    let
        (newSim, newGen) =
            flip runRand (sim ^. randomGen) $ do
                newGhosts <- moveGhosts delta (sim ^. speed) (sim ^. level)
                return (sim & level . ghosts .~ newGhosts)

    in
        newSim & tick +~ delta
               & randomGen .~ newGen


moveGhosts :: Float -> Float -> Level -> Rand StdGen [Ghost]
moveGhosts delta speed lvl =
    mapM (ghostMove delta speed lvl) (lvl ^. ghosts)


-- Ugly as fuck code for ghost AI. Needs to be refactored eventually
ghostMove :: Float -> Float -> Level -> Ghost -> Rand StdGen Ghost
ghostMove delta speed lvl ghost =
    if ghost ^. progress >= 0.5 then do
        next <- pickExits ghost (lvl ^. walls)
        return $
            ghost & position .~ (ghost ^. nextPos)
                  & progress .~ (-0.5)
                  & validExits .~ next
    else
        return (ghost & progress +~ delta*speed*2)


pickExits :: Ghost -> [IPoint] -> Rand StdGen [IPoint]
pickExits ghost occupied =
    case exits (ghost ^. nextPos) occupied of
        []  -> return []
        [p] -> return [p]
        ps  -> fmap (++[ghost ^. position]) (shuffleM (delete (ghost ^. position) ps))


-- Get all the tiles adjacent to `from` that are not `occupied`.
exits :: IPoint -> [IPoint] -> [IPoint]
exits from occupied = do
    tile <- map (from+) [(-1,0),(0,-1),(0,1),(1,0)]
    guard $ all (tile /=) occupied
    return tile


