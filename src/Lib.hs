module Lib
     where

import qualified Graphics.Gloss as GLS

data Resolution = FullScreen | WindowedMax | WindowedExact (Int, Int)
data PiecePicker = TrueRandom | BagRandom
data PiecePreview = NoPreview | PreviewOne | PreviewTwo | PreviewThree
data Piece = Piece [(Int, Int)] GLS.Color deriving (Show)
data Options = Options
    { resolution       :: Resolution
    , wellWidth        :: Int
    , wellHeight       :: Int
    , showShadow       :: Bool
    , allowInstantDrop :: Bool
    , allowPieceSlide  :: Bool
    , allowPieceHold   :: Bool
    , piecePreview     :: PiecePreview
    , piecePicker      :: PiecePicker
    }

defaultOptions :: Options
defaultOptions = Options
    { resolution = WindowedExact (1280, 720)
    , wellWidth = 10
    , wellHeight = 20
    , showShadow = True
    , allowInstantDrop = True
    , allowPieceSlide = True
    , allowPieceHold = True
    , piecePreview = PreviewOne
    , piecePicker = BagRandom
    }

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
