module State where

import           Data.Function                            ( (&) )
import qualified System.Random                 as Random
import qualified Data.Map.Strict               as Map
import qualified Graphics.Gloss                as GLS

import           Tetris

data GameState = NotStarted | Playing | Paused | GameOver

data State = State
    { resolution :: Resolution
    , options :: Options
    , score :: Int
    , well :: Well
    , randomGenerator :: Random.StdGen
    , time :: Float
    , deltaTime :: Float
    , secondsToNextGravity :: Float
    , secondsToNextMovement :: Float
    , gravity :: Float -- cells/second
    , applyExtraGravity :: Bool
    , gameState :: GameState
    , level :: Int
    , piece :: Piece
    , piecePosition :: Position
    , movePieceRight :: Bool
    , movePieceLeft :: Bool
    }

fps :: Int
fps = 60

initialState :: Random.StdGen -> State
initialState randomGenerator = State
    { resolution            = (1280, 720)
    , options               = defaultOptions
    , score                 = 1234567890
    , well                  = emptyWell defaultOptions
    , randomGenerator       = randomGenerator
    , time                  = 0
    , deltaTime             = 0
    , secondsToNextGravity  = 1
    , secondsToNextMovement = 0
    , gravity               = 1.0
    , applyExtraGravity     = False
    , gameState             = NotStarted
    , level                 = 1
    , piece                 = tetrominoL
    , piecePosition         = (0, 0)
    , movePieceRight        = False
    , movePieceLeft         = False
    }
