module Drawing where

import           Data.Function                            ( (&) )
import qualified Data.Map.Strict               as Map
import qualified Graphics.Gloss                as GLS

import           State
import           Tetris

type AlphaNumeral = [Position] -- filled positions in a 3 wide by 5 high matrix

-- ALPHANUMERALS

alphaNumeral0 :: AlphaNumeral
alphaNumeral0 =
    [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (2, 2), (0, 3), (2, 3), (0, 4), (1, 4), (2, 4)]

-- POSITIONS & PIXELS

alphaNumeralPositionToPixelOffset :: Offset -> Position -> Float -> Offset
alphaNumeralPositionToPixelOffset (baseOffsetX, baseOffsetY) (x, y) size = (offsetX, offsetY)
  where
    alphaNumeralX = realToFrac x
    alphaNumeralY = realToFrac y
    offsetX       = baseOffsetX + (alphaNumeralX * size) + (size / 2.0)
    offsetY       = baseOffsetY + (alphaNumeralY * size) + (size / 2.0)

wellPositionToPixelOffset :: Position -> Float -> Float -> Float -> Offset
wellPositionToPixelOffset (x, y) size wellWidth wellHeight = (offsetX, offsetY)
  where
    wellX   = realToFrac x
    wellY   = realToFrac y
    offsetX = (wellWidth / 2.0 & negate) + (wellX * size) + (size / 2.0)
    offsetY = (wellHeight / 2.0 & negate) + (wellY * size) + (size / 2.0)

-- DRAWING

drawRectangle :: Float -> Float -> Float -> Float -> GLS.Color -> GLS.Picture
drawRectangle width height offsetX offsetY color =
    GLS.rectangleSolid width height & GLS.color color & GLS.translate offsetX offsetY

-- RENDERING

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
    horizontalResolution = fst currentResolution & realToFrac
    verticalResolution   = snd currentResolution & realToFrac
    minimumResolution    = min horizontalResolution verticalResolution
    fontSize             = minimumResolution * 0.025
    border               = minimumResolution * 0.05
    cellSize             = wellHeight / (rows currentOptions & realToFrac)
    wellWidth            = cellSize * (columns currentOptions & realToFrac)
    wellHeight           = minimumResolution * 0.8
    wellWithActivePiece =
        addPieceAtPositionToWell (piece state) (piecePosition state) (emptyWell currentOptions)

renderAlphaNumeral :: AlphaNumeral -> Offset -> GLS.Color -> Float -> GLS.Picture
renderAlphaNumeral alphaNumeral offset color size =
    foldr
            (\position pictures ->
                renderAlphaNumeralCell offset position color (size / 5.0) : pictures
            )
            []
            alphaNumeral
        & GLS.pictures

renderAlphaNumeralCell :: Offset -> Position -> GLS.Color -> Float -> GLS.Picture
renderAlphaNumeralCell baseOffset position color size = drawRectangle size
                                                                      size
                                                                      offsetX
                                                                      offsetY
                                                                      color
  where
    offset  = alphaNumeralPositionToPixelOffset baseOffset position size
    offsetX = fst offset
    offsetY = snd offset

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
        | otherwise = renderWellCell position color cellSize wellWidth wellHeight showGrid

renderWellCell :: Position -> GLS.Color -> Float -> Float -> Float -> Bool -> GLS.Picture
renderWellCell position color sizeOuter wellWidth wellHeight showGrid = if showGrid
    then GLS.pictures [outerCell (GLS.greyN 0.9), innerCell]
    else outerCell color
  where
    outerCell outerColor = drawRectangle sizeOuter sizeOuter offsetX offsetY outerColor
    innerCell = drawRectangle sizeInner sizeInner offsetX offsetY color
    offset    = wellPositionToPixelOffset position sizeOuter wellWidth wellHeight
    offsetX   = fst offset
    offsetY   = snd offset
    sizeInner = sizeOuter * 0.8
