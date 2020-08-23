import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note
import Data.VCO
import Data.VCA

vco1 = VCO c 1
vca1 = VCA 1.0 (outSquareVCO unitVCO) (outSineVCO vco1)

main :: IO ()
main = putWAVEFile "example.wav" (signalToWave audioRate 10 (outSignalVCA vca1))