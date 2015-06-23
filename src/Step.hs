module Step (step, exits) where

import Control.Monad        (guard)
import Control.Monad.Random (StdGen, newStdGen, Rand, runRand, getRandomR)
import Lens.Family2

import Types


step :: anytype -> Float -> Simulation -> Simulation
step _ delta sim =
    let
        (newSim, newGen) =
            flip runRand (sim ^. randomGen) $ do
                newGhosts <- moveGhosts delta (sim ^. level)
                return (sim & level . ghosts .~ newGhosts)

    in
        newSim & tick +~ delta
               & randomGen .~ newGen

  where
    moveGhosts :: Float -> Level -> Rand StdGen [Ghost]
    moveGhosts delta lvl =
        mapM (ghostMove delta lvl) (lvl ^. ghosts)

    -- Ugly as fuck code for ghost AI. Needs to be refactored eventually
    ghostMove :: Float -> Level -> Ghost -> Rand StdGen Ghost
    ghostMove delta lvl ghost =
        let
            newGhost =
                ghost & progress +~ (delta * 2)
            newPos =
                if newGhost ^. progress >= 1 then nextPos ghost else ghost ^. position
            switchDir =
                or [ any (== nextPos newGhost) (lvl ^. walls)
                   , newGhost ^. progress >= 1
                   ]
            possible =
                exits newPos (lvl ^. walls)

        in
            if not switchDir then
                return newGhost
            else do
                next <-
                    case length possible of
                        0 -> return (ghost ^. direction)
                        1 -> return (possible !! 0)
                        _ -> choose (filter (/= dirOpposite (ghost ^. direction)) possible)
                return $
                    newGhost & position .~ newPos
                             & progress .~ 0
                             & direction .~ next



-- Get all the directions you can go from `from` without bumping into any of
-- the positions `occupied`. Currently used to figure out which ways a ghost
-- can go without bumping into walls
exits :: IPoint -> [IPoint] -> [Direction]
exits from occupied = do
    dir <- allDirections
    guard $ all (\p -> p /= from + dirVector dir) occupied 
    return dir


-- Partially (!!) pick a random element from a list
choose :: [a] -> Rand StdGen a
choose [] = error "Trying to choose element from empty list!"
choose xs = do { i <- getRandomR (0, length xs - 1); return (xs !! i) }

