module Life.Main
  (main) where

import System.Environment (getArgs)
import Control.Monad (mapM_)
import Life.Interface

help = [ "Conway's Game of Life"
       , "Flags:"
       , "  -h\t\tDisplay this help"
       , "Keys:"
       , "  Space\t\tStep simulation"
       , "  Backspace\tPrevious state"
       , "  Return\tStart simulation"
       , "  Plus\t\tIncrease speed"
       , "  Minus\t\tDecrease speed"
       , "  c\t\tClear grid"
       , "  q\t\tQuit" ]

main :: IO ()
main = do
  args <- getArgs
  if elem "-h" args then
    mapM_ putStrLn help
    else
    initializeScreen

