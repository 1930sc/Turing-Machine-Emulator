module MachineFunctions (runTM, Tape) where

              -- Turing Machine emulator. Made in Haskell --

import Data.List
import Types

{-   * To understand the Action type, see TMfiles module.
     * The Tape type is a variant of the one in here:
           - https://rosettacode.org/wiki/Universal_Turing_machine
     * I've come up with everything else:
           - https://github.com/Average-user/Turing-Machine-Emulator                                    -}

-- ========================================================================== --
                  -- Tape type, handling fucntions --
-- ========================================================================== --

{- Creates a Tape based on: The character chosen to represent a blank symbol, a
   String to represent the written part of the tape, and finally the amount of
   characters to print.                                                       -}
newTape :: Char -> String -> Int -> Tape
newTape blank = tapeF blank ""

{- It helps the previous function by creating the infinite lists              -}
tapeF :: Char -> [Char] -> [Char] -> Int -> Tape
tapeF blank lts rts n | null rts  = Tape blank left blanks n
                      | otherwise = Tape (head rts) left right n
                      where blanks = repeat blank
                            left   = reverse lts ++ blanks
                            right  = tail rts ++ blanks

{-| The action type defined on the UTMfiles module, defines three actions:
    'ToRight', 'ToLeft', and 'Stay'. This function implements them on the tape.      -}
moveTape :: Tape -> Char -> Action -> Tape
moveTape (Tape b (l:ls) (r:rs) n) x Stay    = Tape x (l:ls) (r:rs) n
moveTape (Tape b (l:ls) (r:rs) n) x ToLeft  = Tape l ls (x:r:rs) n
moveTape (Tape b (l:ls) (r:rs) n) x ToRight = Tape r (x:l:ls) rs n

-- ========================================================================== --
                -- Running a Turing Machine --
-- ========================================================================== --

{-| It takes a Tape, the current State and the Machine-rules, and returns
    the new Tape, the new State, and: False if the tape changed, or True
    if no rule was applied.
    t = current tape, s = current state, (r:rs) = rules                        -}
oneTapeChange :: Tape -> State -> Rules -> (Tape, State, Bool)
oneTapeChange t s []     = (t, s, True)
oneTapeChange t s (r:rs) = if st == s && c1 == currentChar t
                               then (moveTape t c2 ac, ns, False)
                               else oneTapeChange t s rs
    where st = stateOf r
          c1 = charOf r
          c2 = nextCharOf r
          ac = actionOf r
          ns = nextStateOf r

{-| It applies the oneTapeChange function until this one returns false on the
    'end' sector. It also keeps record of every different Tape, after every step,
    in a sequential order.
    t = current Tape, s = current state, rs = rules, ts = lsit of tapes, e = end
                                                                              -}
runTMH :: Tape -> State -> Rules -> [Tape] -> Bool -> [Tape]
runTMH t s rs ts e = if e then t:ts
                          else let (newT, newS, end) = oneTapeChange t s rs
                                 in runTMH newT newS rs (t:ts) end

{-| With the help of runTMH (runTM Helper), it returns a list with every
    change that the Tape has suffer along the process.
    sl = sides-length. -}
runTM :: String -> Machine -> Int -> [Tape]
runTM tape machine sl = runTMH (newTape b tape sl) initial rules [] False
  where initial = initialState machine
        rules   = rulesOf machine
        b       = blankOf machine
