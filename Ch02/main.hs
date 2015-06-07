import Euterpea.Music.Note.Music
import Euterpea.IO.MIDI.ToMidi

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
        in dMinor :+: gMajor :+: cMajor

-- Create a major chord starting at pitch with the passed duration
majorChord :: Pitch -> Dur -> Music Pitch
majorChord p du = let root = Prim (Note du p)
                      -- Major third is up four semitons
                      maj3 =  Prim (Note du (trans 4 p))
                      -- Perfect fith is seven semitons
                      p5 =  Prim (Note du (trans 7 p))
                 in root :=: maj3 :=: p5

-- Create a minor chord starting at pitch with the passed duration
minorChord :: Pitch -> Dur -> Music Pitch
minorChord p du = let root = Prim (Note du p)
                      -- Minor third is up three semitones
                      min3 =  Prim (Note du (trans 3 p))
                      -- Perfect fith is seven semitones
                      p5 =  Prim (Note du (trans 7 p))
                 in root :=: min3 :=: p5

-- Exercise 2.1 The above example is fairly concrete, in that, for one, it
-- is rooted in C major, and furthermore it has a fixed tempo. Define
-- a function twoFiveOne :: Pitch -> Dur -> Music Pitch such that
-- twoFiveOne p d constructs a ii-V-I chord progression in the key whose
-- major scale begins on the pitch p (i.e. the first degree of the major
-- scale on which the progression is being constructed), where the
-- duration of the first two cho rds is each d , and the duration of the
-- last chord is 2 ∗ d
--
-- Notes:
-- The function `trans` takes an int `n` and a pitch and transposes the
-- pitch `n` semitones, i.e., 
-- trans :: Int -> Pitch -> Pitch
twoFiveOne :: Pitch -> Dur -> Music Pitch
-- Second degree of a major scale is up one whole step (2 semitones)
-- Fifth degree of a major scale is up: whole, whole, half, whole
-- or 2 + 2 + 1 + 2 = 7 semitones
twoFiveOne p du = let two = minorChord (trans 2 p) du
                      five = majorChord (trans 7 p) du
                      one = majorChord p du
                  in two :+: five :+: one

main :: IO () 
--main =  writeMidi "out.midi" Main.t251
main =  writeMidi "out.midi" $ (twoFiveOne (D, 4) wn)
