module Main
    ( main
    )
where

import qualified System.Random                 as Random
import qualified Graphics.Gloss                as GLS

import           Drawing
import           Game
import           State
import           Tetris

window :: Options -> GLS.Display
window options = case display options of
    FullScreen                    -> GLS.FullScreen
    WindowedMax                   -> GLS.InWindow "Hetris" (1920, 1080) (0, 0)
    WindowedExact (width, height) -> GLS.InWindow "Hetris" (width, height) (0, 0)

background :: GLS.Color
background = GLS.black

main :: IO ()
main = do
    randomGenerator <- Random.newStdGen
    GLS.play (window defaultOptions)
             background
             fps
             (initialState randomGenerator)
             render
             handleEvent
             handleFrame
