module Main where

import Control.Monad        (guard)
import Control.Monad.Random (newStdGen)
import Graphics.Gloss       (simulate, Display(InWindow), black)

import LevelConf            (level1, levelWidth, levelHeight, tileSize)
import IPoint               (IPoint)
import Simulation           (Simulation(Simulation), Level(Level))
import Ghost                (Ghost, mkGhost)
import Step                 (step)
import Draw                 (draw)


main :: IO ()
main = do
    gen <- newStdGen
    let sim = Simulation 0 1 gen (parseLevel level1)

    simulate (InWindow "PacmANN" (levelWidth*tileSize, levelHeight*tileSize) (0, 0))
        black 60
        sim draw step


parseLevel :: [[Char]] -> Level
parseLevel source =
    Level (parseWalls source) (parseGhosts source)

  where
    parseWalls :: [[Char]] -> [IPoint]
    parseWalls = parseTile '#' id

    parseGhosts :: [[Char]] -> [Ghost]
    parseGhosts = parseTile 'm' mkGhost

    parseTile :: Char -> (IPoint -> a) -> [[Char]] -> [a]
    parseTile target constructor source = do
        (y, row) <- zip [0..] source
        (x, char) <- zip [0..] row
        guard (char == target)
        return (constructor (x, y))


