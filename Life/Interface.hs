module Life.Interface
  (initializeScreen) where

import Data.Array.IArray (assocs, (//), (!))
import Control.Monad (when, sequence_)
import System.Exit (exitFailure)
import Graphics.UI.SDL as SDL
import Data.Word (Word32, Word8)
import Life.Logic

data GameState = GameState {
    grids :: [Grid]
  , interval :: Word32
  , running :: Bool
  , auto :: Bool
  }

defaultState grid = GameState [grid] 1000 True False

initializeScreen :: IO ()
initializeScreen = do
  SDL.init flags
  screen <- SDL.trySetVideoMode width height 32 surfaceFlags
  SDL.setCaption "Life" "life"
  case screen of
    Nothing -> exitFailure
    Just s -> gameLoop s $ defaultState (emptyGrid gridSize)
  where
    flags = [SDL.InitVideo]
    surfaceFlags = [SDL.HWSurface]
    gridSize = (width `quot` 10, height `quot` 10)
    width = 640
    height = 480

gameLoop :: SDL.Surface -> GameState -> IO ()
gameLoop screen state = do
  render screen state
  newState <- SDL.waitEvent >>= (handleEvents state)
  when (running newState) $ gameLoop screen newState

handleEvents :: GameState -> SDL.Event -> IO GameState
handleEvents state SDL.Quit = SDL.quit >> return (state { running = False })
handleEvents state (SDL.KeyDown key) = keyDown state (symKey key)
handleEvents state (SDL.MouseButtonDown _ _ button) = mouseButtonDown state button
handleEvents state _ = return state

keyDown :: GameState -> SDL.SDLKey -> IO GameState
keyDown state SDLK_SPACE = return $ state { grids = next : grids state }
  where next = step . head . grids $ state
keyDown state SDLK_BACKSPACE = return $ state { grids = prev . grids $ state}
  where prev xs
          | null . tail $ xs = xs
          | otherwise = tail xs
keyDown state SDLK_RETURN = return $ state { auto = not . auto $ state }
keyDown state SDLK_q = return $ state { running = False }
keyDown state _ = return state

mouseButtonDown :: GameState -> SDL.MouseButton -> IO GameState
mouseButtonDown state SDL.ButtonLeft = do
  (x, y, _) <- SDL.getMouseState
  coords <- return $ posToCoords x y
  new <- return $ grid // [(coords, toggle $ grid ! coords)]
  return $ state { grids = new : grids state }
  where
    posToCoords x y = (x `quot` 10, y `quot` 10)
    grid = head . grids $ state
mouseButtonDown state _ = return state

createColor :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
createColor surface r g b = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b

drawGrid :: SDL.Surface -> Grid -> IO ()
drawGrid surface grid = do
  backgroundColor <- createColor surface 0xff 0xff 0xff
  borderColor <- createColor surface 0xef 0xef 0xef
  cellColor <- createColor surface 0x00 0x00 0x00
  fillRect surface Nothing backgroundColor
  sequence_ [hline 0 y 640 borderColor | y <- [0, 10..480]]
  sequence_ [vline x 0 480 borderColor | x <- [0, 10..640]]
  sequence_ [cell state coords cellColor | (coords, state) <- assocs grid]
  where
    hline x y length color = fillRect surface (Just $ Rect x y length 1) color
    vline x y length color = fillRect surface (Just $ Rect x y 1 length) color
    cell Living coords color = fillRect surface (Just $ cellRect coords) color
    cell Dead _ _ = return True
    cellRect coords = Rect ((fst coords) * 10) ((snd coords) * 10) 10 10

render :: SDL.Surface -> GameState -> IO ()
render screen state = do
  drawGrid screen $ head $ grids state
  SDL.flip screen

