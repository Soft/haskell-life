{-# LANGUAGE TemplateHaskell #-}
module Life.Interface
  (initializeScreen) where

import Prelude hiding (head, tail)
import qualified Graphics.UI.SDL as SDL
import System.Exit (exitFailure)
import Data.Array.IArray (assocs, bounds, (//), (!))
import Data.Word (Word32, Word8)
import Data.List.NonEmpty
import Control.Monad (when)
import Control.Lens hiding ((<|), uncons)
import Life.Logic

data GameState = GameState {
    _grids :: NonEmpty Grid
  , _interval :: Word32
  , _running :: Bool
  , _auto :: Bool
  , _infinite :: Bool
  }

makeLenses ''GameState

defaultState :: Grid -> GameState
defaultState grid = GameState (grid :| []) 100 True False True

initializeScreen :: IO ()
initializeScreen = do
  SDL.init flags
  screen <- SDL.trySetVideoMode width height 32 surfaceFlags
  SDL.setCaption "Life" "life"
  time <- SDL.getTicks
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
  newState <- SDL.pollEvent >>= flip handleEvents state'
  SDL.delay 10
  when (newState ^. running) $ gameLoop screen lastUpdate newState
  where
    performTimedEvents = do
      now <- SDL.getTicks
      let needsUpdate = (now - time) >= state ^. interval
      return $ if needsUpdate && state ^. auto then
                  (now, stepState state)
                else
                  (time, state)

handleEvents :: SDL.Event -> GameState -> IO GameState
handleEvents SDL.Quit = \s -> SDL.quit >> return (running .~ False $ s)
handleEvents (SDL.KeyDown key) = return . (keyDown $ SDL.symKey key)
handleEvents (SDL.MouseButtonDown _ _ button) = mouseButtonDown button
handleEvents _ = return

stepState :: GameState -> GameState
stepState state = grids %~ (next <|) $ state
  where
    next = step mapper . head $ state ^. grids
    mapper = if state ^. infinite then infiniteNeighbors else boundedNeighbors

keyDown :: SDL.SDLKey -> GameState -> GameState
keyDown SDL.SDLK_SPACE = stepState
keyDown SDL.SDLK_BACKSPACE = grids %~ prev
  where prev xs = case uncons xs of
                    (_, Nothing) -> xs
                    (_, Just s) -> s
keyDown SDL.SDLK_RETURN = auto %~ not
keyDown SDL.SDLK_PLUS = interval %~ substract
  where substract n = if 10 < n then n - 10 else n
keyDown SDL.SDLK_MINUS = interval %~ add
  where add n = if n < 1500 then n + 10 else n
keyDown SDL.SDLK_c = \s -> let (_, size) = bounds . head $ s ^. grids
  in grids .~ (emptyGrid size :| []) $ s
keyDown SDL.SDLK_t = infinite %~ not
keyDown SDL.SDLK_q = running .~ False
keyDown _ = id

mouseButtonDown :: SDL.MouseButton -> GameState -> IO GameState
mouseButtonDown SDL.ButtonLeft state = do
  (x, y, _) <- SDL.getMouseState
  let coords = posToCoords x y
  let new = grid // [(coords, toggle $ grid ! coords)]
  return (grids %~ (new <|) $ state)
  where
    posToCoords x y = (x `quot` 10, y `quot` 10)
    grid = head $ state ^. grids
mouseButtonDown _ state = return state

createColor :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO SDL.Pixel
createColor surface = SDL.mapRGB $ SDL.surfaceGetPixelFormat surface

drawGrid :: SDL.Surface -> Grid -> IO ()
drawGrid surface grid = do
  backgroundColor <- createColor surface 0xff 0xff 0xff
  borderColor <- createColor surface 0xef 0xef 0xef
  cellColor <- createColor surface 0x00 0x00 0x00
  SDL.fillRect surface Nothing backgroundColor
  sequence_ [hline 0 y 640 borderColor | y <- [0, 10..480]]
  sequence_ [vline x 0 480 borderColor | x <- [0, 10..640]]
  sequence_ [cell state coords cellColor | (coords, state) <- assocs grid]
  where
    hline x y l = SDL.fillRect surface $ Just $ SDL.Rect x y l 1
    vline x y l = SDL.fillRect surface $ Just $ SDL.Rect x y 1 l
    cell Living coords = SDL.fillRect surface $ Just $ cellRect coords
    cell Dead _ = const $ return True
    cellRect coords = SDL.Rect (fst coords * 10) (snd coords * 10) 10 10

render :: SDL.Surface -> GameState -> IO ()
render screen state = do
  drawGrid screen $ head $ state ^. grids
  SDL.tryFlip screen
  return ()

