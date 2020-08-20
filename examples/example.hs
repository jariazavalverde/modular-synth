import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note

example :: Signal Double
example = gain 1 (mempty +>
          noteToSignal (Triplet
              (quarter f)
              (quarter a)
              (quarter g)
          ) 60 triangleWave)

main :: IO ()
main = putWAVEFile "example.wav" (toWave audRate example)