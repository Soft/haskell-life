module Life.Logic
  (Cell(..), Coords, Grid, NeighborMapper, toggle, step, boundedNeighbors
  , infiniteNeighbors, stepCell, emptyGrid) where

import Data.Array.IArray
import Control.Monad (liftM2, mapM_)
import Control.Arrow ((***), (&&&))
import Data.List (delete)

data Cell = Dead | Living
  deriving (Eq, Enum)
type Coords = (Int, Int)
type Grid = Array Coords Cell
type NeighborMapper = (Coords, Coords) -> Coords -> [Coords]

toggle :: Cell -> Cell
toggle Dead = Living
toggle Living = Dead

step :: NeighborMapper -> Grid -> Grid
step mapper grid = listArray size $ map newState (indices grid)
  where
    newState = uncurry stepCell . ((grid !) &&& livingCount)
    livingCount = sum . map fromEnum . neighbors
    neighbors = map (grid !) . mapper size
    size = bounds grid

boundedNeighbors :: NeighborMapper
boundedNeighbors bounds = filter (inRange bounds) . neighbors'

infiniteNeighbors :: NeighborMapper
infiniteNeighbors (_, (mx, my)) = map wrapAround . neighbors'
  where
    wrapAround (x, y) = (wrap mx x, wrap my y)
    wrap max n = n `mod` max

neighbors' :: Coords -> [Coords]
neighbors' (x, y) = map ((+x) *** (+y)) offsets
  where
    offsets = delete (0,0) $ liftM2 (,) [-1..1] [-1..1]

stepCell :: Cell -> Int -> Cell
stepCell Living neighbors
  | neighbors < 2 = Dead
  | neighbors > 3 = Dead
  | otherwise = Living
stepCell Dead neighbors
  | neighbors == 3 = Living
  | otherwise = Dead

emptyGrid :: (Int, Int) -> Grid
emptyGrid size = listArray ((0, 0), size) $ repeat Dead


