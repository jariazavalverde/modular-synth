import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note
import Data.VCO
import Data.VCA

vco = VCO c (outSineVCO unitVCO)
vca = VCA 1.0 (outSquareVCO unitVCO) (outSineVCO vco)

main :: IO ()
main = putWAVEFile "example.wav" (signalToWave audioRate 10 (outSignalVCA vca))