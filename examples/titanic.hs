import Data.WAVE(putWAVEFile)
import Data.Signal

titanic :: [Signal]
titanic = map ((0.5 `time`) . ($ (sinusoid 1 0))) [f,f,f,f,e,f,zero,f,e,f,zero,g,a,g]

main :: IO ()
main = putWAVEFile "titanic.wav" (toWave audRate $ mconcat titanic)