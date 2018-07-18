module Lib where

import           Data.Function                            ( (&) )
import qualified Debug.Trace                   as Trace
import qualified System.Random                 as Random
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
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

data GameState = NotStarted | Playing | Paused | GameOver

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
  , secondsToNextGravity :: Float
  , secondsToNextMovement :: Float
  , gravity :: Float -- cells/second

  , applyExtraGravity :: Bool
  , gameState :: GameState
  , piece :: Piece
  , piecePosition :: Position
  , movePieceRight :: Bool
  , movePieceLeft :: Bool
  }

fps :: Int
fps = 60

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
  { resolution            = (1280, 720)
  , options               = defaultOptions
  , score                 = 0
  , well                  = emptyWell defaultOptions
  , randomGenerator       = randomGenerator
  , time                  = 0
  , deltaTime             = 0
  , secondsToNextGravity  = 1
  , secondsToNextMovement = 0
  , gravity               = 1.0
  , applyExtraGravity     = False
  , gameState             = NotStarted
  , piece                 = tetrominoL
  , piecePosition         = (0, 0)
  , movePieceRight        = False
  , movePieceLeft         = False
  }

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

isPiecePositionValid :: Piece -> Position -> Options -> Bool
isPiecePositionValid (Piece _ positions _) (wellX, wellY) options =
  map isValidPosition positions & and
 where
  xMax = columns options - 1
  yMax = rows options - 1
  isValidPosition (pieceX, pieceY) =
    (pieceX + wellX >= 0)
      && (pieceX + wellX <= xMax)
      && (pieceY + wellY >= 0)
      && (pieceY + wellY <= yMax)

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


maxY :: State -> Int
maxY state = rows (options state) - 1

centerX :: State -> Int
centerX state = columns (options state) `quot` 2

lockPiece :: State -> State
lockPiece state
  | snd (piecePosition state) > maxY state = state { gameState = GameOver }
  | otherwise = (spawnNewPiece state)
    { well = addPieceAtPositionToWell (piece state) (piecePosition state) (well state)
    }

spawnNewPiece :: State -> State
spawnNewPiece state = state { randomGenerator = (snd newRandom)
                            , piece           = newPiece
                            , piecePosition   = (centerX state, maxY state)
                            }
 where
  newRandom = Random.randomR (0, 6) (randomGenerator state)
  newPiece  = randomPiece (fst newRandom)

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

clearFilledRows :: State -> State
clearFilledRows state = state { well = newWell }
 where
  currentWell       = well state
  fillTarget        = options state & columns
  filledCellsPerRow = Map.foldrWithKey
    (\(x, y) color cellsPerRow ->
      if color /= GLS.white then Map.insertWith (+) y 1 cellsPerRow else cellsPerRow
    )
    Map.empty
    currentWell
  filledRows = Map.filterWithKey (\y amountFilled -> amountFilled == fillTarget) filledCellsPerRow
  highestFilledRow = case Map.keys filledRows & reverse & Maybe.listToMaybe of
    Just a  -> a
    Nothing -> 0
  amountRowsFilled = Map.size filledRows
  newWellPart      = Map.foldrWithKey
    (\targetY amountFilled wellPart -> Map.filterWithKey (\(x, y) color -> y == targetY) currentWell
    )
    Map.empty
    filledRows
  wellWithoutFilledRows = Map.difference currentWell newWellPart
  newWell               = Map.mapKeys
    (\(x, y) -> if y > highestFilledRow then (x, y - (1 * amountRowsFilled)) else (x, y))
    wellWithoutFilledRows

startNewGame :: State -> State
startNewGame state =
  (spawnNewPiece state) { well = emptyWell (options state), gameState = Playing }

addPieceAtPositionToWell :: Piece -> Position -> Well -> Well
addPieceAtPositionToWell (Piece _ positions color) (wellX, wellY) well = Map.union newPieceWell
                                                                                   well
 where
  newPieceWell =
    map (\(pieceX, pieceY) -> ((pieceX + wellX, pieceY + wellY), color)) positions & Map.fromList

pieceAtPositionToWellPart :: Piece -> Position -> Well
pieceAtPositionToWellPart (Piece _ positions color) (wellX, wellY) = wellPart
 where
  wellPart =
    map (\(pieceX, pieceY) -> ((pieceX + wellX, pieceY + wellY), color)) positions & Map.fromList


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
  else outerCell color
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
        cellToPicture position color cellSize wellWidth wellHeight showGrid : pictures
      )
      []
      well

    & GLS.pictures
 where
  cellToPicture position color cellSize wellWidth wellHeight showGrid
    | color == GLS.white = GLS.pictures []
    | otherwise          = renderCell position color cellSize wellWidth wellHeight showGrid

render :: State -> GLS.Picture
render state = case gameState state of
  NotStarted -> GLS.pictures [wellBorder, wellCells]
  Playing    -> GLS.pictures [wellBorder, wellCells, activePiece]
  Paused     -> GLS.pictures [wellBorder, wellCells, activePiece, pausedOverlay]
  GameOver   -> GLS.pictures [wellBorder, wellCells, activePiece, gameOverOverlay]
 where
  wellBorder = drawRectangle (wellWidth + border) (wellHeight + border) 0 0 (GLS.greyN 0.25)
  wellCells  = GLS.pictures
    [ drawRectangle wellWidth wellHeight 0 0 GLS.white
    , renderWell (well state) cellSize wellWidth wellHeight (showGrid currentOptions)
    ]
  activePiece =
    renderWell wellWithActivePiece cellSize wellWidth wellHeight (showGrid currentOptions)
  pausedOverlay        = drawRectangle wellWidth wellHeight 0 0 (GLS.greyN 0.25 & GLS.withAlpha 0.5)
  gameOverOverlay      = drawRectangle wellWidth wellHeight 0 0 (GLS.red & GLS.withAlpha 0.5)
  currentResolution    = resolution state
  currentOptions       = options state
  resolutionHorizontal = fst currentResolution & realToFrac
  resolutionVertical   = snd currentResolution & realToFrac
  border               = resolutionVertical * 0.05
  cellSize             = wellHeight / (rows currentOptions & realToFrac)
  wellWidth            = cellSize * (columns currentOptions & realToFrac)
  wellHeight           = resolutionVertical * 0.8
  wellWithActivePiece =
    addPieceAtPositionToWell (piece state) (piecePosition state) (emptyWell currentOptions)

pauseOrUnpause :: State -> State
pauseOrUnpause state = case gameState state of
  Paused  -> state { gameState = Playing }
  Playing -> state { gameState = Paused }
  _       -> state

handleEvent :: GLS.Event -> State -> State
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
