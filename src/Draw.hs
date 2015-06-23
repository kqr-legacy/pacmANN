module Draw (draw) where

import Lens.Family2
import Graphics.Gloss
import Graphics.Gloss.Data.Vector (mulSV)

import LevelConf (levelWidth, levelHeight, tileSize)
import Step (exits)
import Types

draw :: Simulation -> Picture
draw sim =
    centreOrigin
        [ color (greyN 0.3) (pictures (sim ^.. level . walls . traverse . to wall))
        , pictures (sim ^.. level . ghosts . traverse . to ghost)
        ]

  where
    -- Takes coordinates spanning from (0,0) to (width,height) and displays them
    -- right-side-up with (0,0) in the middle.
    centreOrigin :: [Picture] -> Picture
    centreOrigin =
        translate (-levelWidth*tileSize/2) (levelHeight*tileSize/2) . scale 1 (-1) . mconcat

    wall :: IPoint -> Picture
    wall p = 
        translateTile (toPoint p) (rectangleSolid tileSize tileSize)

    -- Uuuugly code to draw a ghost moving smoothly across the level, with its
    -- direction points around it.
    ghost :: Ghost -> Picture
    ghost ghost =
        mconcat
            [ color white (translateTile (toPoint (ghost ^. position) + (mulSV (ghost ^. progress) (toPoint (dirVector (ghost ^. direction))))) (circleSolid 12))
            , color green (pictures [translateTile (toPoint (ghost ^. position) + toPoint (dirVector d)) (circleSolid 6) | d <- exits (ghost ^. position) (sim ^. level . walls)])
            , color red (translateTile (toPoint (nextPos ghost)) (circleSolid 6))
            ]

    translateTile :: Point -> Picture -> Picture
    translateTile p =
        translate (fst p * tileSize) (snd p * tileSize)


