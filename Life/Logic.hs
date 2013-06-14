module Life.Logic
  (Cell(..), Coords(..), Grid, toggle, step, boundedNeighbors, stepCell, emptyGrid) where

import Data.Array.IArray
import Control.Monad (liftM2, mapM_)
import Control.Arrow ((***), (&&&))
import Data.List (delete, transpose)

data Cell = Dead | Living
  deriving (Eq, Enum)
type Coords = (Int, Int)
type Grid = Array Coords Cell

toggle :: Cell -> Cell
toggle Dead = Living
toggle Living = Dead

step :: Grid -> Grid
step grid = listArray size $ map newState (indices grid)
  where
    newState = uncurry stepCell . ((grid !) &&& livingCount)
    livingCount = sum . map fromEnum . neighbors
    neighbors = map (grid !) . boundedNeighbors size
    size = bounds grid

boundedNeighbors :: (Coords, Coords) -> Coords -> [Coords]
boundedNeighbors bounds = filter (inRange bounds) . neighbors'

-- infiniteNeighbors :: (Coords, Coords) -> Coords -> [Coords]

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


