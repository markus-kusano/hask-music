import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.IO.MIDI.ToMidi (writeMidi)

-- In general, the cymbals are the most complicated part. On the fourth
-- bar, there is a hi-hat diddly extending into the fifth bar
--
-- This is the intro which starts with a crash (normally, it starts with an
-- open HH)
--
-- It is 4 bars.
cymIntro :: Music Pitch
cymIntro = cymCr1
            :+: cymNoOp
            :+: cymNoOp
            :+: cymOp5        -- Bar 4

-- This is the "middle" sections of the cymbals. It starts with an open HH
-- and ends with an open HH. 
--
-- 4 bars
cymIntroMiddle :: Music Pitch
cymIntroMiddle = cymOp1
            :+: cymNoOp
            :+: cymNoOp
            :+: cymOp5        -- Bar 4

-- There are 16 bars until the vocals come in
cymPreVocal :: Music Pitch
cymPreVocal = cymIntro :+: (timesM 3 cymIntroMiddle)

-- 16 bars of snare and bass before vocals
snbPreVocal :: Music Pitch
snbPreVocal = timesM 4 fourBar
        -- A four bar snare and bass phrase
  where fourBar = (timesM 3 snbIntroMiddle) :+: (snbIntroEnd)

-- 16 more bars for the first verse. 
cymFirstV :: Music Pitch
-- cymIntroMiddle is four bars
cymFirstV = timesM 4 cymIntroMiddle

-- 16 more bars for the first verse
--
-- On the 16th bar, beats 5 and 6 are snare with an open HH on beat 4
snbFirstV :: Music Pitch
snbFirstV = timesM 4 fourbar
    where fourbar = (timesM 3 snbFVMiddle) :+: snbFVEnd

-- Cymbal line for the main drum beat with no open hihats or crashes.
-- One bar
cymNoOp :: Music Pitch
cymNoOp = instrument Percussion $ timesM 6 (perc ClosedHiHat en)

-- Cymbal line for the main drum beat. 
-- Open hi-hat on beat one.
cymOp1 :: Music Pitch
cymOp1 = instrument Percussion $ 
                 (perc OpenHiHat en) 
                 :+: (timesM 5 (perc ClosedHiHat en))

-- Cymbal line for main drum beat.
-- Crash on beat one (this is the intro bar)
cymCr1 :: Music Pitch
cymCr1 = instrument Percussion $ 
                 (perc CrashCymbal1 en) 
                 :+: (timesM 5 (perc ClosedHiHat en))

-- Cymbal line for main drum beat
-- Open HH on beat 5. This is connected to mainDrumCymOp1
cymOp5 :: Music Pitch
cymOp5 = instrument Percussion $ 
                 timesM 4 (perc ClosedHiHat en) 
                 :+: (perc OpenHiHat en)
                 :+: (perc ClosedHiHat en)

-- Snare and bass for main drum beat intro (first 16 bars)
-- The bass hits on the down beat of 1 and on the off beats for the first
-- three beats
--
-- The snare hits on beat 4 with nothing going on on the off beats
--
-- One bar
snbIntroMiddle :: Music Pitch
snbIntroMiddle = instrument Percussion $ 
                 bassDrum sn :+: rest en :+: bassDrum sn
                 :+: rest sn :+: bassDrum sn
                 :+: snareDrum en :+: rest qn
  where bassDrum = perc BassDrum1
        snareDrum = perc AcousticSnare

-- On the fourth and fifth bar, the bass follows the open HH (beats five
-- and one)
snbIntroEnd :: Music Pitch
snbIntroEnd = instrument Percussion $ 
                 bassDrum sn :+: rest en :+: bassDrum sn
                 :+: rest sn :+: bassDrum sn
                 :+: snareDrum en :+: rest en :+: bassDrum en
  where bassDrum = perc BassDrum1
        snareDrum = perc AcousticSnare


-- Snare and bass for first verse (the 2nd 16 bars (after lyrics come in)).
-- The snare drum almost copies the bass except it hits only on the second
-- off beat (not the third)
-- 
-- This is one bar
snbFVMiddle :: Music Pitch
snbFVMiddle = instrument Percussion $ 
         -- Same first 3 beats as the intro (snb{Middle,End}Intro)
         bassDrum sn :+: rest en :+: bassDrum sn
         :+: rest sn :+: bassDrum sn
         -- Snare on the downbeat, second off beat
         :+: snareDrum sn :+: rest en :+: snareDrum sn
         :+: rest en 
  where bassDrum = perc BassDrum1
        snareDrum = perc AcousticSnare

-- The fourth bar of the first verse snare and bass.
-- (This is the bar with the open HH)
-- It is the same as in the intro: is one snare hit on the downbeat
snbFVEnd :: Music Pitch
snbFVEnd = snbIntroEnd

-- The last bar of the first verse ends with two snare hits
--snbFVFinal 

-- Here, I stich together all the sequences
song :: Music Pitch
song = 
      -- 16 bar intro before vocals
      cymPreVocal :=: snbPreVocal
      -- 16 bar first verse
      :+: cymFirstV :=: snbFirstV

-- Default tempo is 120. We would like ~75, so, 
-- 120 * x = 75 ==> (5/8). We are also in 6/8 so convert everything to
-- triplets (3/2)
main :: IO ()
main = writeMidi "out.midi" $ tempo (5/8) $ tempo (3/2) song
--main = play mainDrum01
