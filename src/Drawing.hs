module Drawing where

import           Data.Function                            ( (&) )
import qualified Data.Map.Strict               as Map
import qualified Graphics.Gloss                as GLS

import           State
import           Tetris

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

renderWell :: Well -> Float -> Float -> Float -> Options -> Bool -> GLS.Picture
renderWell well cellSize wellWidth wellHeight options showGrid =
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
        | snd position >= maxY options = GLS.pictures []
        | otherwise = renderCell position color cellSize wellWidth wellHeight showGrid

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
        , renderWell (well state)
                     cellSize
                     wellWidth
                     wellHeight
                     currentOptions
                     (showGrid currentOptions)
        ]
    activePiece = renderWell wellWithActivePiece
                             cellSize
                             wellWidth
                             wellHeight
                             currentOptions
                             (showGrid currentOptions)
    pausedOverlay = drawRectangle wellWidth wellHeight 0 0 (GLS.greyN 0.25 & GLS.withAlpha 0.5)
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
