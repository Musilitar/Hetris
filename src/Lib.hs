module Lib where

import           Data.Function                            ( (&) )
import qualified Debug.Trace                   as Trace
import qualified System.Random                 as Random
import qualified Data.Map.Strict               as Map
import qualified Graphics.Gloss                as GLS
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as GLS

type Resolution = (Int, Int)

type Position = (Int, Int)

type Offset = (Float, Float)

type Well = Map.Map Position GLS.Color

data Display
  = FullScreen
  | WindowedMax
  | WindowedExact Resolution

data GameState = NotStarted | Playing | Paused

data PiecePicker
  = TrueRandom
  | BagRandom

data PiecePreview
  = NoPreview
  | PreviewOne
  | PreviewTwo
  | PreviewThree

data Piece =
  Piece [Position]
        GLS.Color

data Options = Options
  { display :: Display
  , rows :: Int
  , columns :: Int
  , showShadow :: Bool
  , showGrid :: Bool
  , allowInstantDrop :: Bool
  , allowPieceSlide :: Bool
  , allowPieceHold :: Bool
  , piecePreview :: PiecePreview
  , piecePicker :: PiecePicker
  }

data State = State
  { resolution :: Resolution
  , options :: Options
  , score :: Int
  , well :: Well
  , randomGenerator :: Random.StdGen
  , time :: Float
  , deltaTime :: Float
  , secondsToNextMove :: Float
  , gravity :: Int -- cells/second
  , gameState :: GameState
  , currentPiece :: Piece
  , currentPosition :: Position
  }

defaultOptions :: Options
defaultOptions = Options
  { display          = WindowedExact (1280, 720)
  , rows             = 20
  , columns          = 10
  , showShadow       = True
  , showGrid         = False
  , allowInstantDrop = True
  , allowPieceSlide  = True
  , allowPieceHold   = True
  , piecePreview     = PreviewOne
  , piecePicker      = BagRandom
  }

emptyWell :: Options -> Well
emptyWell options = Map.fromList keyValues
 where
  currentRows    = rows options - 1
  currentColumns = columns options - 1
  rowsRange      = [0 .. currentRows]
  columnsRange   = [0 .. currentColumns]
  keys           = map (\row -> map (\column -> (column, row)) columnsRange) rowsRange & concat
  keyValues      = map (\position -> (position, GLS.white)) keys

initialState :: Random.StdGen -> State
initialState randomGenerator = State
  { resolution        = (1280, 720)
  , options           = defaultOptions
  , score             = 0
  , well              = emptyWell defaultOptions
  , randomGenerator   = randomGenerator
  , time              = 0
  , deltaTime         = 0
  , secondsToNextMove = 0
  , gravity           = 1
  , gameState         = NotStarted
  , currentPiece      = tetrominoL
  , currentPosition   = (0, 0)
  }

handleFrame :: Float -> State -> State
handleFrame newTime state = advanceState newState
 where
  newState = state { time              = (time state + newTime)
                   , deltaTime         = newTime
                   , secondsToNextMove = (secondsToNextMove state - newTime)
                   }

advanceState :: State -> State
advanceState state | secondsToNextMove state <= 0 = state
                   | otherwise                    = state

canPieceMoveTo :: Piece -> Position -> Well -> Bool
canPieceMoveTo piece position well = True

movePiece :: State -> State
movePiece state | isNewPositionValid = state
                | otherwise          = state
 where
  newPosition        = (fst (currentPosition state), snd (currentPosition state) - 1)
  isNewPositionValid = True

startNewGame :: State -> State
startNewGame state = state { randomGenerator = (snd newRandom)
                           , currentPiece    = newPiece
                           , currentPosition = (0, 0)
                           }
 where
  newRandom = Random.randomR (0, 6) (randomGenerator state)
  newPiece  = randomPiece (fst newRandom)

drawRectangle :: Float -> Float -> Float -> Float -> GLS.Color -> GLS.Picture
drawRectangle width height offsetX offsetY color =
  GLS.rectangleSolid width height & GLS.color color & GLS.translate offsetX offsetY

wellPositionToPixelOffset :: Position -> Float -> Float -> Float -> Offset
wellPositionToPixelOffset (x, y) size wellWidth wellHeight = (offsetX, offsetY)
 where
  wellX   = realToFrac x
  wellY   = realToFrac y
  offsetX = (wellWidth / 2.0 & negate) + (wellX * size) + (size / 2.0)
  offsetY = (wellHeight / 2.0 & negate) + (wellY * size) + (size / 2.0)

renderCell :: Position -> GLS.Color -> Float -> Float -> Float -> Bool -> GLS.Picture
renderCell position color sizeOuter wellWidth wellHeight showGrid = if showGrid
  then GLS.pictures [outerCell (GLS.greyN 0.9), innerCell]
  else outerCell GLS.white
 where
  outerCell outerColor = drawRectangle sizeOuter sizeOuter offsetX offsetY outerColor
  innerCell = drawRectangle sizeInner sizeInner offsetX offsetY color
  offset    = wellPositionToPixelOffset position sizeOuter wellWidth wellHeight
  offsetX   = fst offset
  offsetY   = snd offset
  sizeInner = sizeOuter * 0.8

renderWell :: Well -> Float -> Float -> Float -> Bool -> GLS.Picture
renderWell well cellSize wellWidth wellHeight showGrid =
  Map.foldrWithKey
      (\position color pictures ->
        renderCell position color cellSize wellWidth wellHeight showGrid : pictures
      )
      []
      well

    & GLS.pictures

render :: State -> GLS.Picture
render state = GLS.pictures [wellBorder, wellCells]
 where
  wellBorder = drawRectangle (wellWidth + border) (wellHeight + border) 0 0 (GLS.greyN 0.25)
  wellCells = renderWell (well state) cellSize wellWidth wellHeight (showGrid currentOptions)
  currentResolution    = resolution state
  currentOptions       = options state
  resolutionHorizontal = fst currentResolution & realToFrac
  resolutionVertical   = snd currentResolution & realToFrac
  border               = resolutionVertical * 0.05
  cellSize             = wellHeight / (rows currentOptions & realToFrac)
  wellWidth            = cellSize * (columns currentOptions & realToFrac)
  wellHeight           = resolutionVertical * 0.8

handleEvent :: GLS.Event -> State -> State
handleEvent (GLS.EventKey (GLS.Char 'n') GLS.Up _ _) state = startNewGame state
handleEvent _ state = state

randomPiece :: Int -> Piece
randomPiece random = case random of
  0 -> tetrominoI
  1 -> tetrominoJ
  2 -> tetrominoL
  3 -> tetrominoO
  4 -> tetrominoS
  5 -> tetrominoT
  6 -> tetrominoZ

tetrominoI :: Piece
tetrominoI = Piece [(0, 0), (1, 0), (2, 0), (3, 0)] GLS.cyan

tetrominoJ :: Piece
tetrominoJ = Piece [(0, 1), (1, 1), (2, 1), (2, 0)] GLS.blue

tetrominoL :: Piece
tetrominoL = Piece [(0, 0), (0, 1), (1, 1), (2, 1)] GLS.orange

tetrominoO :: Piece
tetrominoO = Piece [(0, 0), (1, 0), (0, 1), (1, 1)] GLS.yellow

tetrominoS :: Piece
tetrominoS = Piece [(0, 0), (1, 0), (1, 1), (2, 1)] GLS.green

tetrominoT :: Piece
tetrominoT = Piece [(1, 0), (0, 1), (1, 1), (2, 1)] GLS.magenta

tetrominoZ :: Piece
tetrominoZ = Piece [(1, 0), (2, 0), (0, 1), (1, 1)] GLS.red
