module Lib where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Graphics.Gloss as GLS
import qualified Graphics.Gloss.Interface.Pure.Game as GLS

type Resolution = (Int, Int)

type Position = (Int, Int)

type Well = Map.Map Position Bool

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
  deriving (Show)

data Options = Options
  { display :: Display
  , cellsHorizontal :: Int
  , cellsVertical :: Int
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
    , cellsHorizontal = 10
    , cellsVertical = 20
    , showShadow = True
    , allowInstantDrop = True
    , allowPieceSlide = True
    , allowPieceHold = True
    , piecePreview = PreviewOne
    , piecePicker = BagRandom
    }

initialState :: State
initialState =
  State
    { resolution = (1280, 720)
    , options = defaultOptions
    , score = 0
    , well = Map.empty
    }

updateState :: Float -> State -> State
updateState time state = state

drawRectangle :: Float -> Float -> GLS.Color -> GLS.Picture
drawRectangle width height color =
  GLS.rectangleSolid width height & GLS.color color

render :: State -> GLS.Picture
render state = GLS.pictures [well]
  where
    well = drawRectangle wellWidth wellHeight GLS.white
      where
        currentResolution = resolution state
        currentOptions = options state
        resolutionHorizontal = fst currentResolution & realToFrac
        resolutionVertical = snd currentResolution & realToFrac
        cellSize = wellHeight / (cellsVertical currentOptions & realToFrac)
        wellWidth = cellSize * (cellsHorizontal currentOptions & realToFrac)
        wellHeight = resolutionVertical * 0.8

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
