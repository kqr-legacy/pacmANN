module Draw (draw) where

import Lens.Family2
import Graphics.Gloss
import Graphics.Gloss.Data.Vector (mulSV)

import LevelConf    (levelWidth, levelHeight, tileSize)
import IPoint       (IPoint, point)
import Simulation
import Ghost


draw :: Simulation -> Picture
draw sim =
    centreOrigin
        [ color (greyN 0.3) (pictures (sim ^.. level . walls . traverse . to wall))
        , pictures (sim ^.. level . ghosts . traverse . to ghost)
        ]


-- Takes coordinates spanning from (0,0) to (width,height) and displays them
-- right-side-up with (0,0) in the middle.
centreOrigin :: [Picture] -> Picture
centreOrigin =
    shift . invert . combine
  where
    shift = translate (-levelWidth*tileSize/2) (levelHeight*tileSize/2)
    invert = scale 1 (-1)
    combine = mconcat


wall :: IPoint -> Picture
wall p = 
    translateTile (point p) (rectangleSolid tileSize tileSize)


-- Draws the ghost including little circles indicating possible/taken directions
ghost :: Ghost -> Picture
ghost ghost =
    mconcat
        [ color white (translateTile (ghostDrawPoint ghost) (circleSolid 12))
        , color green (pictures [translateTile (point d) (circleSolid 6) | d <- ghost ^. validExits])
        , color red (translateTile (point (ghost ^. nextPos)) (circleSolid 6))
        ]
  where
    ghostDrawPoint ghost =
        let
            curPos = ghost ^. position . to point
            along =
                if ghost ^. progress >= 0 then
                    ghost ^. nextPos . to point - curPos
                else
                    curPos - ghost ^. prevPos . to point
        in
            curPos + mulSV (ghost ^. progress) along


translateTile :: Point -> Picture -> Picture
translateTile p =
    translate (fst p * tileSize) (snd p * tileSize)


