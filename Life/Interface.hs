{-# LANGUAGE TemplateHaskell #-}
module Life.Interface
  (initializeScreen) where

import Data.Array.IArray (assocs, bounds, (//), (!))
import Control.Monad (when)
import System.Exit (exitFailure)
import Graphics.UI.SDL as SDL
import Data.Word (Word32, Word8)
import Control.Lens
import Life.Logic

data GameState = GameState {
    _grids :: [Grid]
  , _interval :: Word32
  , _running :: Bool
  , _auto :: Bool
  , _infinite :: Bool
  }

makeLenses ''GameState

defaultState :: Grid -> GameState
defaultState grid = GameState [grid] 100 True False True

initializeScreen :: IO ()
initializeScreen = do
  SDL.init flags
  screen <- SDL.trySetVideoMode width height 32 surfaceFlags
  SDL.setCaption "Life" "life"
  time <- getTicks
  case screen of
    Nothing -> exitFailure
    Just s -> gameLoop s time $ defaultState (emptyGrid gridSize)
  where
    flags = [SDL.InitVideo]
    surfaceFlags = [SDL.HWSurface]
    gridSize = (width `quot` 10, height `quot` 10)
    width = 640
    height = 480

gameLoop :: SDL.Surface -> Word32 -> GameState -> IO ()
gameLoop screen time state = do
  (lastUpdate, state') <- performTimedEvents
  render screen state'
  newState <- SDL.pollEvent >>= handleEvents state'
  delay 10
  when (newState ^. running) $ gameLoop screen lastUpdate newState
  where
    performTimedEvents = do
      now <- getTicks
      let needsUpdate = (now - time) >= state ^. interval
      return $ if needsUpdate && state ^. auto then
                  (now, stepState state)
                else
                  (time, state)

handleEvents :: GameState -> SDL.Event -> IO GameState
handleEvents state SDL.Quit = SDL.quit >> return (running .~ False $ state)
handleEvents state (SDL.KeyDown key) = keyDown state (symKey key)
handleEvents state (SDL.MouseButtonDown _ _ button) = mouseButtonDown state button
handleEvents state _ = return state

stepState :: GameState -> GameState
stepState state = grids %~ (next :) $ state
  where
    next = step mapper . head $ state ^. grids
    mapper = if state ^. infinite then infiniteNeighbors else boundedNeighbors

keyDown :: GameState -> SDL.SDLKey -> IO GameState
keyDown state SDLK_SPACE = return $ stepState state
keyDown state SDLK_BACKSPACE = return (grids %~ prev $ state)
  where prev xs
          | null . tail $ xs = xs
          | otherwise = tail xs
keyDown state SDLK_RETURN = return (auto %~ not $ state)
keyDown state SDLK_PLUS = return (interval %~ substract $ state)
  where substract n = if 10 < n then n - 10 else n
keyDown state SDLK_MINUS = return (interval %~ add $ state)
  where add n = if n < 1500 then n + 10 else n
keyDown state SDLK_c = return (grids .~ [emptyGrid gridSize] $ state)
  where gridSize = let (_, size) = bounds . head $ state ^. grids in size
keyDown state SDLK_t = return (infinite %~ not $ state)
keyDown state SDLK_q = return (running .~ False $ state)
keyDown state _ = return state

mouseButtonDown :: GameState -> SDL.MouseButton -> IO GameState
mouseButtonDown state SDL.ButtonLeft = do
  (x, y, _) <- SDL.getMouseState
  let coords = posToCoords x y
  let new = grid // [(coords, toggle $ grid ! coords)]
  return (grids %~ (new :) $ state)
  where
    posToCoords x y = (x `quot` 10, y `quot` 10)
    grid = head $ state ^. grids
mouseButtonDown state _ = return state

createColor :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
createColor surface = SDL.mapRGB $ SDL.surfaceGetPixelFormat surface

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
    hline x y l = fillRect surface $ Just $ Rect x y l 1
    vline x y l = fillRect surface $ Just $ Rect x y 1 l
    cell Living coords = fillRect surface $ Just $ cellRect coords
    cell Dead _ = const $ return True
    cellRect coords = Rect (fst coords * 10) (snd coords * 10) 10 10

render :: SDL.Surface -> GameState -> IO ()
render screen state = do
  drawGrid screen $ head $ state ^. grids
  SDL.tryFlip screen
  return ()

