import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note
import Data.VCO

vco1 = VCO 1 1
vco2 = VCO c (outSineVCO vco1)

main :: IO ()
main = putWAVEFile "example.wav" (signalToWave audioRate 10 (outSineVCO vco2))