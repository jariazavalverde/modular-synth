import Data.WAVE(putWAVEFile, doubleToSample, WAVE(..), WAVEHeader(..))

type Rate = Int
type Amplitude = Double
type Frecuency = Double
type Phase = Double
type Time = Double

data Signal a = Signal { getRate :: Rate, getSamples :: [a] }

instance Show (Signal a) where
    show (Signal rate _) = "<signal at " ++ show rate ++ "Hz>"

instance Functor Signal where
    fmap f (Signal rate samples) = Signal rate (map f samples)

ctrRate, audRate :: Rate
ctrRate = 4410
audRate = 44100

sinusoid :: Rate -> Amplitude -> Phase -> Frecuency -> Signal Double
sinusoid rate a phi f = let w = 2*pi*f
                            rate' = fromIntegral rate
                        in Signal rate $ cycle (map (\t -> a * sin (w*(t/rate') + phi)) [0..fromIntegral rate])

joinSignals :: Rate -> [Signal a] -> Signal a
joinSignals rate xs = Signal rate (concat $ map getSamples xs)

empty, c, d, e, f, g, a, b :: (Frecuency -> Signal Double) -> Signal Double
empty signal = signal 0
c signal = signal 261.6
d signal = signal 293.7
e signal = signal 329.6
f signal = signal 349.2
g signal = signal 392.0
a signal = signal 440.0
b signal = signal 493.9

seconds :: Time -> Signal a -> Signal a
seconds t (Signal rate samples) = Signal rate $ take (round (t*fromIntegral rate)) samples

signalToWave :: Signal Double -> WAVE
signalToWave (Signal rate samples) = let header = WAVEHeader 1 rate 32 (Just $ length samples)
                                         body = map ((:[]).doubleToSample) samples
                                     in WAVE header body

titanic :: [Signal Double]
titanic = map (0.5 `seconds`) $ map ($ (sinusoid audRate 1 0)) [f,f,f,f,e,f,empty,f,e,f,empty,g,a,g]

main :: IO ()
main = putWAVEFile "sample.wav" (signalToWave $ joinSignals audRate titanic)