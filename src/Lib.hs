module Lib where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Graphics.Gloss as GLS
import qualified Graphics.Gloss.Interface.Pure.Game as GLS

type Resolution = (Int, Int)

type Position = (Int, Int)

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
defaultOptions =
  Options
    { display = WindowedExact (1280, 720)
    , rows = 20
    , columns = 10
    , showShadow = True
    , allowInstantDrop = True
    , allowPieceSlide = True
    , allowPieceHold = True
    , piecePreview = PreviewOne
    , piecePicker = BagRandom
    }

emptyWell :: Options -> Well
emptyWell options = Map.fromList keyValues
  where
    currentRows = rows options - 1
    currentColumns = columns options - 1
    rowsRange = [0 .. currentRows]
    columnsRange = [0 .. currentColumns]
    keys =
      map (\row -> map (\column -> (column, row)) columnsRange) rowsRange &
      concat
    keyValues = map (\position -> (position, GLS.red)) keys

initialState :: State
initialState =
  State
    { resolution = (1280, 720)
    , options = defaultOptions
    , score = 0
    , well = emptyWell defaultOptions
    }

updateState :: Float -> State -> State
updateState time state = state

drawRectangle :: Float -> Float -> Float -> Float -> GLS.Color -> GLS.Picture
drawRectangle width height offsetX offsetY color =
  GLS.rectangleSolid width height & GLS.color color &
  GLS.translate offsetX offsetY

renderCell :: Position -> GLS.Color -> Float -> GLS.Picture
renderCell position color size = drawRectangle size size 0 0 color

renderWell :: Well -> Float -> GLS.Picture
renderWell well cellSize = drawRectangle cellSize cellSize 0 0 GLS.white

render :: State -> GLS.Picture
render state = GLS.pictures [wellBackground]
  where
    wellBackground = drawRectangle wellWidth wellHeight 0 0 GLS.white
      where
        currentResolution = resolution state
        currentOptions = options state
        resolutionHorizontal = fst currentResolution & realToFrac
        resolutionVertical = snd currentResolution & realToFrac
        cellSize = wellHeight / (rows currentOptions & realToFrac)
        wellWidth = cellSize * (columns currentOptions & realToFrac)
        wellHeight = resolutionVertical * 0.9

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
