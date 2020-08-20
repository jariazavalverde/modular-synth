import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note

example :: Signal Double
example = gain 1 (mempty +>
          noteToSignal (triplet quarter f a g) 60 triangleWave) +>
          noteToSignal (duplet quarter a c) 60 sineWave

main :: IO ()
main = putWAVEFile "example.wav" (toWave audRate example)