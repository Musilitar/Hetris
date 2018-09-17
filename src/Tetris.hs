module Tetris where

import           Data.Function                            ( (&) )
import qualified Graphics.Gloss                as GLS
import qualified Data.Map.Strict               as Map

type Resolution = (Int, Int)

type Position = (Int, Int)

type Offset = (Float, Float)

type Well = Map.Map Position GLS.Color

data Display
    = FullScreen
    | WindowedMax
    | WindowedExact Resolution

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

data PiecePicker
    = TrueRandom
    | BagRandom

data PiecePreview
    = NoPreview
    | PreviewOne
    | PreviewTwo
    | PreviewThree

data Piece =
    Piece PieceType
            [Position]
            GLS.Color

data PieceType = TetrominoI | TetrominoJ | TetrominoL | TetrominoO | TetrominoS | TetrominoT | TetrominoZ

-- OPTIONS, WELLS & PIECES

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

maxX :: Options -> Int
maxX options = columns options - 1

maxY :: Options -> Int
maxY options = rows options

centerX :: Options -> Int
centerX options = columns options `quot` 2

tetrominoI :: Piece
tetrominoI = Piece TetrominoI [(-2, 0), (-1, 0), (0, 0), (1, 0)] GLS.cyan

tetrominoJ :: Piece
tetrominoJ = Piece TetrominoJ [(-1, 1), (-1, 0), (0, 0), (1, 0)] GLS.blue

tetrominoL :: Piece
tetrominoL = Piece TetrominoL [(-1, 0), (0, 0), (1, 0), (1, 1)] GLS.orange

tetrominoO :: Piece
tetrominoO = Piece TetrominoO [(-1, 1), (-1, 0), (0, 1), (0, 0)] GLS.yellow

tetrominoS :: Piece
tetrominoS = Piece TetrominoS [(-1, 0), (0, 0), (0, 1), (1, 1)] GLS.green

tetrominoT :: Piece
tetrominoT = Piece TetrominoT [(-1, 0), (0, 1), (0, 0), (1, 0)] GLS.magenta

tetrominoZ :: Piece
tetrominoZ = Piece TetrominoZ [(-1, 1), (0, 1), (0, 0), (1, 0)] GLS.red

-- CHECKING POSITIONS

isPiecePositionValid :: Piece -> Position -> Options -> Bool
isPiecePositionValid (Piece _ positions _) (wellX, wellY) options =
    map isValidPosition positions & and
  where
    isValidPosition (pieceX, pieceY) =
        (pieceX + wellX >= 0)
            && (pieceX + wellX <= maxX options)
            && (pieceY + wellY >= 0)
            && (pieceY + wellY <= maxY options)

isPiecePositionColliding :: Piece -> Position -> Well -> Bool
isPiecePositionColliding piece position well = Map.size filledCells > 0
  where
    wellWithPiece    = pieceAtPositionToWellPart piece position
    wellIntersection = Map.intersection well wellWithPiece
    filledCells      = Map.filter (\color -> color /= GLS.white) wellIntersection

canPieceMoveTo :: Piece -> Position -> Well -> Options -> Bool
canPieceMoveTo piece position well options = isWithinWell && not isColliding
  where
    isWithinWell = isPiecePositionValid piece position options
    isColliding  = isPiecePositionColliding piece position well

-- ROTATING & ADDING

rotatePieceClockwise :: Piece -> Piece
rotatePieceClockwise (Piece pieceType positions color) = case pieceType of
    TetrominoI -> Piece pieceType (map (\(x, y) -> (y, ((x * (-1)) - 1))) positions) color
    TetrominoO -> Piece pieceType positions color
    _          -> Piece pieceType (map (\(x, y) -> (y, -x)) positions) color

rotatePieceCounterClockwise :: Piece -> Piece
rotatePieceCounterClockwise (Piece pieceType positions color) = case pieceType of
    TetrominoI -> Piece pieceType (map (\(x, y) -> ((y * (-1)) - 1, x)) positions) color
    TetrominoO -> Piece pieceType positions color
    _          -> Piece pieceType (map (\(x, y) -> (-y, x)) positions) color

addPieceAtPositionToWell :: Piece -> Position -> Well -> Well
addPieceAtPositionToWell (Piece _ positions color) (wellX, wellY) well = Map.union newPieceWell
                                                                                   well
  where
    newPieceWell =
        map (\(pieceX, pieceY) -> ((pieceX + wellX, pieceY + wellY), color)) positions
            & Map.fromList

-- CREATING WELLS & PIECES

pieceAtPositionToWellPart :: Piece -> Position -> Well
pieceAtPositionToWellPart (Piece _ positions color) (wellX, wellY) = wellPart
  where
    wellPart =
        map (\(pieceX, pieceY) -> ((pieceX + wellX, pieceY + wellY), color)) positions
            & Map.fromList

randomPiece :: Int -> Piece
randomPiece random = case random of
    0 -> tetrominoI
    1 -> tetrominoJ
    2 -> tetrominoL
    3 -> tetrominoO
    4 -> tetrominoS
    5 -> tetrominoT
    6 -> tetrominoZ
