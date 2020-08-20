import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note
import Data.Bar

example :: Signal Double
example = gain 2 (mempty +>
    barToSignal (twice [[
        triplet quarter f a g,
        duplet quarter a c
    ]]) 60 triangleWave)

main :: IO ()
main = putWAVEFile "example.wav" (toWave audRate example)