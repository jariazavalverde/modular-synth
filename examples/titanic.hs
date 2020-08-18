import Data.WAVE(putWAVEFile)
import Data.Signal

titanic :: [Signal]
titanic = map ((decay 0.3) . (time 0.5) . ($ (sawWave 1 0)))
    [f,f,f,f,e,f,zero,f,e,f,zero,g,a,g]

main :: IO ()
main = putWAVEFile "titanic.wav" (toWave audRate $ mconcat titanic)