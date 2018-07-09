module Lib where

import Data.Function ((&))
import qualified Debug.Trace as Trace
import qualified Data.Map.Strict as Map
import qualified Graphics.Gloss as GLS
import qualified Graphics.Gloss.Interface.Pure.Game as GLS

type Resolution = (Int, Int)

type Position = (Int, Int)

type Offset = (Float, Float)

type Well = Map.Map Position GLS.Color

data Display
  = FullScreen
  | WindowedMax
  | WindowedExact Resolution

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
  }

defaultOptions :: Options
defaultOptions = Options
    { display          = WindowedExact (1280, 720)
    , rows             = 20
    , columns          = 10
    , showShadow       = True
    , showGrid = True
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

initialState :: State
initialState = State {resolution = (1280, 720), options = defaultOptions, score = 0, well = emptyWell defaultOptions}

updateState :: Float -> State -> State
updateState time state = state

drawRectangle :: Float -> Float -> Float -> Float -> GLS.Color -> GLS.Picture
drawRectangle width height offsetX offsetY color =
    GLS.rectangleSolid width height & GLS.color color & GLS.translate offsetX offsetY

wellPositionToPixelOffset :: Position -> Float -> Float -> Float -> Offset
wellPositionToPixelOffset (x, y) size  wellWidth wellHeight = (offsetX, offsetY)
  where
    wellX = realToFrac x
    wellY = realToFrac y
    offsetX = (wellWidth / 2.0 & negate) + (wellX * size) + (size / 2.0) 
    offsetY = (wellHeight / 2.0 & negate) + (wellY * size) + (size / 2.0)

renderCell :: Position -> GLS.Color -> Float -> Float -> Float -> Bool -> GLS.Picture
renderCell position color sizeOuter wellWidth wellHeight showGrid = 
  case showGrid of
    True -> GLS.pictures [outerCell (GLS.greyN 0.9), innerCell]
    False -> outerCell GLS.white
  where
    outerCell outerColor = drawRectangle sizeOuter sizeOuter offsetX offsetY outerColor
    innerCell = drawRectangle sizeInner sizeInner offsetX offsetY color
    offset = wellPositionToPixelOffset position sizeOuter wellWidth wellHeight
    offsetX = fst offset
    offsetY = snd offset
    sizeInner = sizeOuter * 0.8

renderWell :: Well -> Float -> Float -> Float -> Bool -> GLS.Picture
renderWell well cellSize wellWidth wellHeight showGrid =
    Map.foldrWithKey (\position color pictures -> renderCell position color cellSize wellWidth wellHeight showGrid : pictures) [] well
        & GLS.pictures

render :: State -> GLS.Picture
render state = GLS.pictures [wellBorder, wellCells]
  where
    wellBorder  = drawRectangle (wellWidth + border) (wellHeight + border) 0 0 (GLS.greyN 0.25)
    wellCells      = renderWell (well state) cellSize wellWidth wellHeight (showGrid currentOptions)
    currentResolution    = resolution state
    currentOptions       = options state
    resolutionHorizontal = fst currentResolution & realToFrac
    resolutionVertical   = snd currentResolution & realToFrac
    border = resolutionVertical * 0.05
    cellSize             = wellHeight / (rows currentOptions & realToFrac)
    wellWidth            = cellSize * (columns currentOptions & realToFrac)
    wellHeight           = resolutionVertical * 0.8

handleEvent :: GLS.Event -> State -> State
handleEvent _ state = state

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
