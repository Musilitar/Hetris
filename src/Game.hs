module Game where

import           Data.Function                            ( (&) )
import qualified System.Random                 as Random
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Graphics.Gloss                as GLS
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as GLS

import           State
import           Tetris

-- GRAVITY & MOVEMENT

actualGravity :: State -> Float
actualGravity state
    | applyExtraGravity state = if options state & allowInstantDrop then instantDrop else fastDrop
    | otherwise               = gravity state
  where
    instantDrop = realToFrac fps
    fastDrop    = instantDrop / 2.0


actualSecondsToNextGravity :: State -> Float
actualSecondsToNextGravity state = 1.0 / actualGravity state

actualSecondsToNextMovement :: Float
actualSecondsToNextMovement = 0.25

-- HANDLING

handleFrame :: Float -> State -> State
handleFrame newTime state = case gameState state of
    Playing -> advanceState newState
    _       -> state
  where
    newState = state { time                  = time state + newTime
                     , deltaTime             = newTime
                     , secondsToNextGravity  = secondsToNextGravity state - newTime
                     , secondsToNextMovement = secondsToNextMovement state - newTime
                     }

handleEvent :: GLS.Event -> State -> State
handleEvent (GLS.EventKey (GLS.Char '1') GLS.Down _ _) state = state
handleEvent (GLS.EventKey (GLS.Char 'n') GLS.Down _ _) state = startNewGame state
handleEvent (GLS.EventKey (GLS.Char 'p') GLS.Down _ _) state = pauseOrUnpause state
handleEvent (GLS.EventKey (GLS.Char 'w') GLS.Down _ _) state =
    applyRotation rotatePieceCounterClockwise state
handleEvent (GLS.EventKey (GLS.Char 'x') GLS.Down _ _) state =
    applyRotation rotatePieceClockwise state
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyRight) GLS.Down _ _) state =
    state { movePieceRight = True }
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyRight) GLS.Up _ _) state =
    state { movePieceRight = False }
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyDown) GLS.Down _ _) state =
    state { applyExtraGravity = True }
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyDown) GLS.Up _ _) state =
    state { applyExtraGravity = False }
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyLeft) GLS.Down _ _) state =
    state { movePieceLeft = True }
handleEvent (GLS.EventKey (GLS.SpecialKey GLS.KeyLeft) GLS.Up _ _) state =
    state { movePieceLeft = False }
handleEvent _ state = state

-- GAME MANAGEMENT

startNewGame :: State -> State
startNewGame state =
    (spawnNewPiece state) { well = emptyWell (options state), gameState = Playing }

pauseOrUnpause :: State -> State
pauseOrUnpause state = case gameState state of
    Paused  -> state { gameState = Playing }
    Playing -> state { gameState = Paused }
    _       -> state

-- ADVANCING & APPLYING

advanceState :: State -> State
advanceState state
    | secondsToNextGravity state <= 0 = applyGravity newState
        { secondsToNextGravity = actualSecondsToNextGravity state
        }
    | secondsToNextMovement state <= 0 = applyMovement newState
        { secondsToNextMovement = actualSecondsToNextMovement
        }
    | otherwise = newState
  where
    newState = state { secondsToNextGravity  = (secondsToNextGravity state) - (deltaTime state)
                     , secondsToNextMovement = (secondsToNextMovement state) - (deltaTime state)
                     }

applyGravity :: State -> State
applyGravity state | isNewPositionValid = state { piecePosition = newPosition }
                   | otherwise          = lockPiece state & clearFilledRows
  where
    newPosition        = (fst (piecePosition state), snd (piecePosition state) - 1)
    isNewPositionValid = canPieceMoveTo (piece state) newPosition (well state) (options state)

applyMovement :: State -> State
applyMovement state | isNewPositionValid = state { piecePosition = newPosition }
                    | otherwise          = state
  where
    cellsToShift = if movePieceRight state && not (movePieceLeft state)
        then 1
        else if movePieceLeft state && not (movePieceRight state) then -1 else 0
    newPosition        = (fst (piecePosition state) + cellsToShift, snd (piecePosition state))
    isNewPositionValid = canPieceMoveTo (piece state) newPosition (well state) (options state)

applyRotation :: (Piece -> Piece) -> State -> State
applyRotation rotater state | isNewPieceValid = state { piece = newPiece }
                            | otherwise       = state
  where
    newPiece        = piece state & rotater
    isNewPieceValid = canPieceMoveTo newPiece (piecePosition state) (well state) (options state)

-- PIECE MANAGEMENT

lockPiece :: State -> State
lockPiece state
    | snd (piecePosition state) >= maxY (options state) = state { gameState = GameOver }
    | otherwise = (spawnNewPiece state)
        { well = addPieceAtPositionToWell (piece state) (piecePosition state) (well state)
        }

spawnNewPiece :: State -> State
spawnNewPiece state = state { randomGenerator = (snd newRandom)
                            , piece           = newPiece
                            , piecePosition   = (centerX (options state), maxY (options state))
                            }
  where
    newRandom = Random.randomR (0, 6) (randomGenerator state)
    newPiece  = randomPiece (fst newRandom)

-- CLEARING & SCORING

clearFilledRows :: State -> State
clearFilledRows state = state { score = newScore, well = newWell }
  where
    currentWell       = well state
    fillTarget        = options state & columns
    filledCellsPerRow = Map.foldrWithKey
        (\(x, y) color cellsPerRow ->
            if color /= GLS.white then Map.insertWith (+) y 1 cellsPerRow else cellsPerRow
        )
        Map.empty
        currentWell
    filledRows =
        Map.filterWithKey (\y amountFilled -> amountFilled == fillTarget) filledCellsPerRow
    highestFilledRow = case Map.keys filledRows & reverse & Maybe.listToMaybe of
        Just a  -> a
        Nothing -> 0
    amountRowsFilled = Map.size filledRows
    newWellPart      = Map.foldrWithKey
        (\targetY amountFilled wellPart ->
            Map.filterWithKey (\(x, y) color -> y == targetY) currentWell
        )
        Map.empty
        filledRows
    wellWithoutFilledRows = Map.difference currentWell newWellPart
    newScore              = calculateUpdatedScore amountRowsFilled state
    newWell               = Map.mapKeys
        (\(x, y) -> if y > highestFilledRow then (x, y - (1 * amountRowsFilled)) else (x, y))
        wellWithoutFilledRows

calculateUpdatedScore :: Int -> State -> Int
calculateUpdatedScore amountRowsFilled state = score state + scoreToAdd
  where
    scoreToAdd = case amountRowsFilled of
        1 -> level state * 100
        2 -> level state * 300
        3 -> level state * 500
        4 -> level state * 800
        _ -> 0

