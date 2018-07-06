module Main (main) where

import qualified Graphics.Gloss as GLS

import           Lib

window :: Options -> GLS.Display
window options =
    case resolution options of
        FullScreen -> GLS.FullScreen
        WindowedMax -> GLS.InWindow "Hetris" (1920, 1080) (0, 0)
        WindowedExact (width, height) -> GLS.InWindow "Hetris" (width, height) (0, 0)

background :: GLS.Color
background = GLS.white

fps :: Int
fps = 60

main :: IO ()
main =
    {-GLS.play window background fps-}
    GLS.display (window defaultOptions) background (GLS.circle 100)
