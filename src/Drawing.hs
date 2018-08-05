module Drawing where

import           Data.Function                            ( (&) )
import qualified Data.Map.Strict               as Map
import qualified Data.Char                     as Char
                                                          ( toUpper )
import qualified Data.List.Index               as LI
                                                          ( imap )
import qualified Graphics.Gloss                as GLS

import           State
import           Tetris

type AlphaNumeral = [Position] -- filled positions in a 5 wide by 9 high matrix

-- ALPHANUMERALS

alphaNumeralUnknown :: AlphaNumeral
alphaNumeralUnknown =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (1, 1)
    , (2, 1)
    , (3, 1)
    , (4, 1)
    , (0, 2)
    , (1, 2)
    , (2, 2)
    , (3, 2)
    , (4, 2)
    , (0, 3)
    , (1, 3)
    , (2, 3)
    , (3, 3)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (1, 5)
    , (2, 5)
    , (3, 5)
    , (4, 5)
    , (0, 6)
    , (1, 6)
    , (2, 6)
    , (3, 6)
    , (4, 6)
    , (0, 7)
    , (1, 7)
    , (2, 7)
    , (3, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral0 :: AlphaNumeral
alphaNumeral0 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (2, 3)
    , (4, 3)
    , (0, 4)
    , (2, 4)
    , (4, 4)
    , (0, 5)
    , (2, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral1 :: AlphaNumeral
alphaNumeral1 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (2, 1)
    , (2, 2)
    , (2, 3)
    , (2, 4)
    , (2, 5)
    , (2, 6)
    , (2, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    ]

alphaNumeral2 :: AlphaNumeral
alphaNumeral2 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (4, 5)
    , (4, 6)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral3 :: AlphaNumeral
alphaNumeral3 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (4, 5)
    , (4, 6)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral4 :: AlphaNumeral
alphaNumeral4 =
    [ (4, 0)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeral5 :: AlphaNumeral
alphaNumeral5 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral6 :: AlphaNumeral
alphaNumeral6 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral7 :: AlphaNumeral
alphaNumeral7 =
    [ (4, 0)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (4, 4)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral8 :: AlphaNumeral
alphaNumeral8 =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeral9 :: AlphaNumeral
alphaNumeral9 =
    [ (4, 0)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralA :: AlphaNumeral
alphaNumeralA =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralB :: AlphaNumeral
alphaNumeralB =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (1, 1)
    , (4, 1)
    , (1, 2)
    , (4, 2)
    , (1, 3)
    , (4, 3)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (1, 5)
    , (4, 5)
    , (1, 6)
    , (4, 6)
    , (1, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralC :: AlphaNumeral
alphaNumeralC =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralD :: AlphaNumeral
alphaNumeralD =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (1, 1)
    , (4, 1)
    , (1, 2)
    , (4, 2)
    , (1, 3)
    , (4, 3)
    , (1, 4)
    , (4, 4)
    , (1, 5)
    , (4, 5)
    , (1, 6)
    , (4, 6)
    , (1, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralE :: AlphaNumeral
alphaNumeralE =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralF :: AlphaNumeral
alphaNumeralF =
    [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralG :: AlphaNumeral
alphaNumeralG =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralH :: AlphaNumeral
alphaNumeralH =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralI :: AlphaNumeral
alphaNumeralI =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (2, 1)
    , (2, 2)
    , (2, 3)
    , (2, 4)
    , (2, 5)
    , (2, 6)
    , (2, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralJ :: AlphaNumeral
alphaNumeralJ =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (4, 3)
    , (4, 4)
    , (4, 5)
    , (4, 6)
    , (4, 7)
    , (4, 8)
    ]

alphaNumeralK :: AlphaNumeral
alphaNumeralK =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (3, 1)
    , (0, 2)
    , (2, 2)
    , (0, 3)
    , (1, 3)
    , (0, 4)
    , (0, 5)
    , (1, 5)
    , (0, 6)
    , (2, 6)
    , (0, 7)
    , (3, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralL :: AlphaNumeral
alphaNumeralL =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (0, 8)
    ]

alphaNumeralM :: AlphaNumeral
alphaNumeralM =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (2, 5)
    , (4, 5)
    , (0, 6)
    , (2, 6)
    , (4, 6)
    , (0, 7)
    , (2, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralN :: AlphaNumeral
alphaNumeralN =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralO :: AlphaNumeral
alphaNumeralO =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralP :: AlphaNumeral
alphaNumeralP =
    [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralQ :: AlphaNumeral
alphaNumeralQ =
    [ (2, 0)
    , (2, 1)
    , (0, 2)
    , (1, 2)
    , (2, 2)
    , (3, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralR :: AlphaNumeral
alphaNumeralR =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (3, 1)
    , (0, 2)
    , (2, 2)
    , (0, 3)
    , (1, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralS :: AlphaNumeral
alphaNumeralS =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (4, 2)
    , (4, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (0, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralT :: AlphaNumeral
alphaNumeralT =
    [ (2, 0)
    , (2, 1)
    , (2, 2)
    , (2, 3)
    , (2, 4)
    , (2, 5)
    , (2, 6)
    , (2, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralU :: AlphaNumeral
alphaNumeralU =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (0, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralV :: AlphaNumeral
alphaNumeralV =
    [ (1, 0)
    , (2, 0)
    , (3, 0)
    , (1, 1)
    , (3, 1)
    , (1, 2)
    , (3, 2)
    , (1, 3)
    , (3, 3)
    , (1, 4)
    , (3, 4)
    , (1, 5)
    , (3, 5)
    , (1, 6)
    , (3, 6)
    , (1, 7)
    , (3, 7)
    , (0, 8)
    , (1, 8)
    , (3, 8)
    , (4, 8)
    ]

alphaNumeralW :: AlphaNumeral
alphaNumeralW =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (2, 1)
    , (4, 1)
    , (0, 2)
    , (2, 2)
    , (4, 2)
    , (0, 3)
    , (2, 3)
    , (4, 3)
    , (0, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralX :: AlphaNumeral
alphaNumeralX =
    [ (0, 0)
    , (4, 0)
    , (0, 1)
    , (4, 1)
    , (0, 2)
    , (4, 2)
    , (1, 3)
    , (3, 3)
    , (2, 4)
    , (1, 5)
    , (3, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralY :: AlphaNumeral
alphaNumeralY =
    [ (2, 0)
    , (2, 1)
    , (2, 2)
    , (2, 3)
    , (0, 4)
    , (1, 4)
    , (2, 4)
    , (3, 4)
    , (4, 4)
    , (0, 5)
    , (4, 5)
    , (0, 6)
    , (4, 6)
    , (0, 7)
    , (4, 7)
    , (0, 8)
    , (4, 8)
    ]

alphaNumeralZ :: AlphaNumeral
alphaNumeralZ =
    [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (3, 0)
    , (4, 0)
    , (0, 1)
    , (0, 2)
    , (1, 3)
    , (2, 4)
    , (3, 5)
    , (4, 6)
    , (4, 7)
    , (0, 8)
    , (1, 8)
    , (2, 8)
    , (3, 8)
    , (4, 8)
    ]

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
    interface = renderInterface state wellWidth wellHeight border fontSize

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

renderInterface :: State -> Float -> Float -> Float -> Float -> GLS.Picture
renderInterface state wellWidth wellHeight border fontSize = GLS.pictures
    [scorePictures, optionShowGridPictures0, optionShowGridPictures1, optionShowGridPictures2]
  where
    stringToPicture baseOffsetX offsetY index character = renderAlphaNumeral character
                                                                             offset
                                                                             GLS.white
                                                                             fontSize
      where
        inlineOffset = realToFrac index * ((fontSize * 0.55) + 3.75)
        offset       = (baseOffsetX + inlineOffset, offsetY)
    scorePictures =
        LI.imap (stringToPicture ((wellWidth / 2.0) + border) (wellHeight / 2.0))
                (score state & show)
            & GLS.pictures
    optionShowGridPictures0 =
        LI.imap
                (stringToPicture ((wellWidth / 2.0) + border) ((wellHeight / 2.0) - (border * 1)))
                "SHOW PIECE SHADOW"
            & GLS.pictures
    optionShowGridPictures1 =
        LI.imap
                (stringToPicture ((wellWidth / 2.0) + border) ((wellHeight / 2.0) - (border * 2)))
                "SHOW GRID"
            & GLS.pictures
    optionShowGridPictures2 =
        LI.imap
                (stringToPicture ((wellWidth / 2.0) + border) ((wellHeight / 2.0) - (border * 3)))
                "ALLOW INSTANT DROP"
            & GLS.pictures

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
