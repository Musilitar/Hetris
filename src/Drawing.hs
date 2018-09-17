module Drawing where

import           Data.Function                            ( (&) )
import qualified Data.Map.Strict               as Map
import qualified Data.Char                     as Char
                                                          ( toUpper )
import qualified Data.List.Index               as LI
                                                          ( imap )
import qualified Graphics.Gloss                as GLS

import           Font
import           State
import           Tetris

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

drawString :: String -> Float -> Float -> Float -> GLS.Picture
drawString characters fontSize baseOffsetX offsetY =
    LI.imap (stringToPicture baseOffsetX offsetY) characters & GLS.pictures
  where
    stringToPicture baseOffsetX offsetY index character = renderAlphaNumeral character
                                                                             offset
                                                                             GLS.white
                                                                             fontSize
      where
        inlineOffset = realToFrac index * ((fontSize * 0.55) + 3.75)
        offset       = (baseOffsetX + inlineOffset, offsetY)

drawOption :: Bool -> String -> Float -> Float -> Float -> Float -> GLS.Picture
drawOption isOn characters fontSize baseTextOffsetX baseToggleOffsetX offsetY = GLS.pictures
    [text, toggleBox]
  where
    text      = drawString characters fontSize baseTextOffsetX offsetY
    boxWidth  = 4 * fontSize
    boxHeight = fontSize
    toggleBox = drawToggleBox isOn
                              boxWidth
                              boxHeight
                              (baseToggleOffsetX - (boxWidth / 2.0))
                              baseToggleOffsetX
                              (offsetY + (fontSize / 2))

drawToggleBox :: Bool -> Float -> Float -> Float -> Float -> Float -> GLS.Picture
drawToggleBox isOn boxWidth boxHeight onOffsetX offOffsetX offsetY = GLS.pictures [box, indicator]
  where
    indicatorSize    = boxHeight
    indicatorOffsetX = if isOn
        then onOffsetX - (boxWidth / 2.0) + (indicatorSize / 2.0)
        else offOffsetX - (indicatorSize / 2.0)
    color     = if isOn then GLS.green else GLS.red
    box       = drawRectangle boxWidth boxHeight onOffsetX offsetY color
    indicator = drawRectangle indicatorSize indicatorSize indicatorOffsetX offsetY GLS.white

-- RENDERING

render :: State -> GLS.Picture
render state = case gameState state of
    NotStarted -> GLS.pictures [wellBorder, wellCells, interface]
    Playing    -> GLS.pictures [wellBorder, wellCells, activePiece, interface]
    Paused     -> GLS.pictures [wellBorder, wellCells, activePiece, pausedOverlay, interface]
    GameOver   -> GLS.pictures [wellBorder, wellCells, activePiece, gameOverOverlay, interface]
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
    interface = renderInterface state
                                horizontalResolution
                                verticalResolution
                                wellWidth
                                wellHeight
                                border
                                fontSize

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

renderInterface :: State -> Float -> Float -> Float -> Float -> Float -> Float -> GLS.Picture
renderInterface state horizontalResolution verticalResolution wellWidth wellHeight border fontSize
    = GLS.pictures [scorePictures, optionShowGridPictures, optionAllowInstantDropPictures]
  where
    scorePictures =
        drawString (score state & show) fontSize ((wellWidth / 2.0) + border) (wellHeight / 2.0)
    optionShowGridPictures = drawOption (options state & showGrid)
                                        "SHOW GRID            = 1"
                                        fontSize
                                        ((wellWidth / 2.0) + border)
                                        ((horizontalResolution / 2.0) - border)
                                        ((wellHeight / 2.0) - (border * 2))
    optionAllowInstantDropPictures = drawOption (options state & allowInstantDrop)
                                                "ALLOW INSTANT DROP   = 2"
                                                fontSize
                                                ((wellWidth / 2.0) + border)
                                                ((horizontalResolution / 2.0) - border)
                                                ((wellHeight / 2.0) - (border * 3))

renderAlphaNumeral :: Char -> Offset -> GLS.Color -> Float -> GLS.Picture
renderAlphaNumeral character offset color size =
    foldr
            (\position pictures ->
                renderAlphaNumeralCell offset position color (size / 9.0) : pictures
            )
            []
            alphaNumeral
        & GLS.pictures
  where
    alphaNumeral = case Char.toUpper character of
        '0' -> alphaNumeral0
        '1' -> alphaNumeral1
        '2' -> alphaNumeral2
        '3' -> alphaNumeral3
        '4' -> alphaNumeral4
        '5' -> alphaNumeral5
        '6' -> alphaNumeral6
        '7' -> alphaNumeral7
        '8' -> alphaNumeral8
        '9' -> alphaNumeral9
        'A' -> alphaNumeralA
        'B' -> alphaNumeralB
        'C' -> alphaNumeralC
        'D' -> alphaNumeralD
        'E' -> alphaNumeralE
        'F' -> alphaNumeralF
        'G' -> alphaNumeralG
        'H' -> alphaNumeralH
        'I' -> alphaNumeralI
        'J' -> alphaNumeralJ
        'K' -> alphaNumeralK
        'L' -> alphaNumeralL
        'M' -> alphaNumeralM
        'N' -> alphaNumeralN
        'O' -> alphaNumeralO
        'P' -> alphaNumeralP
        'Q' -> alphaNumeralQ
        'R' -> alphaNumeralR
        'S' -> alphaNumeralS
        'T' -> alphaNumeralT
        'U' -> alphaNumeralU
        'V' -> alphaNumeralV
        'W' -> alphaNumeralW
        'X' -> alphaNumeralX
        'Y' -> alphaNumeralY
        'Z' -> alphaNumeralZ
        ' ' -> []
        '=' -> alphaNumeralEquals
        _   -> alphaNumeralUnknown

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
