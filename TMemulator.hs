
              -- Turing Machine emulator. Made in Haskell --
import Data.List
import System.IO
import Control.Monad
import Data.Time.Clock
import TMfiles

{-   * To understand the Action type, see TMfiles module.
     * The Tape type is a variant of the one in here:
           - https://rosettacode.org/wiki/Universal_Turing_machine
     * I've come up with everything else:
           - https://github.com/average-user                                    -}

-- ========================================================================== --
                -- Types definitions and Types synonyms --
-- ========================================================================== --
type State   = String
type Rule    = (State, Char, Char, Action, State)
type Rules   = [Rule]
type Machine = (Rules, State, Char)

data Tape    = Tape Char [Char] [Char] Int

{- ls = left side, rs = right side, x = 'head' and n = Chars to print.
   Because the Tape is infinite, it is necesary to tell the computer, how many
   characters it has to print. In a Tape like this: '...123[4]567...', where
   the dots are infinite to both sides, we tell the computer to print just the
   6 first characters of both sides. And this is done like this:

    *Main> let ls = "123" ++ (repeat '.')
    *Main> let rs = "456" ++ (repeat '.')
    *Main> (Tape '4' ls rs 6)
    ...321[4]456...                                                           -}

instance Show Tape where
  show (Tape x ls rs n) = left ++ "[" ++ [x] ++ "]" ++ right
                          where left  = reverse $ take n ls
                                right = take n rs


-- ========================================================================== --
                -- Main Function, Run Your Turing MACHINE !!! --
-- ========================================================================== --

{- It takes the path of a '.tm' file, a string representing the initial Tape,
   the length of the tape to be showed (see 'Show Tape' instance) and asks if
   it should be printed just the final tape, or the tape after every step     -}
main :: IO ()
main = do
  putStrLn "Turing Machine Path: "
  file <- getLine
  infile   <- openFile file ReadMode
  contents <- hGetContents infile
  putStrLn "\nInitial Tape: "
  tape <- getLine
  putStrLn "\nVisible length of the Tape: "
  len <- getLine >>= return . (\x -> div x 2) . read
  putStrLn "\nJust final tape[0], or every step[1]?: "
  out <- getLine
  let procedure = runTM tape (fileToMachine contents) len
    in if out == "1"
         then do mapM_ print $ reverse procedure
                 putStrLn $ (++) "\nN° of steps : " $ show (length procedure)

         else do start <- getCurrentTime >>= return . utctDayTime
                 print $ head procedure
                 end   <- getCurrentTime >>= return . utctDayTime
                 putStrLn $ (++) "\nN° of steps : " $ show (length procedure)
                 putStrLn $ "CPU Time    : " ++ show (end-start) ++ "\n"


-- ========================================================================== --
                  -- Tape type, handling fucntions --
-- ========================================================================== --

{- Creates a Tape based on the character chosen to represent a blank symbol, a
   String to represent the written part of the tape, and finally the amount of
   characters to print.                                                       -}
newTape :: Char -> String -> Int -> Tape
newTape blank tape n = tapeF blank "" tape n

{- It helps the previous function by creating the infinite lists              -}
tapeF :: Char -> [Char] -> [Char] -> Int -> Tape
tapeF blank lts rts n | null rts  = Tape blank left blanks n
                      | otherwise = Tape (head rts) left right n
                      where blanks = repeat blank
                            left   = reverse lts ++ blanks
                            right  = tail rts ++ blanks

{- The action type defined on the UTMfiles module, defines three actions:
   ToRight, ToLeft, and Stay. This function implements them on the tape.      -}
moveTape :: Tape -> Char -> Action -> Tape
moveTape (Tape b (l:ls) (r:rs) n) x Stay    = Tape x (l:ls) (r:rs) n
moveTape (Tape b (l:ls) (r:rs) n) x ToLeft  = Tape l ls (x:r:rs) n
moveTape (Tape b (l:ls) (r:rs) n) x ToRight = Tape r (x:l:ls) rs n

{- It returns the current char of a tape. Or in other words, the visile
   character for the 'head' of the machine.                                    -}
currentChar :: Tape -> Char
currentChar (Tape x l r n) = x


-- ========================================================================== --
                          -- Accessing tu Rules --
-- ========================================================================== --
ruleTuple :: State -> Char -> Char -> Action -> State -> Rule
ruleTuple a b c d e = (a, b, c, d, e)

stateOf :: Rule -> State
stateOf (x, _, _, _, _) = x

charOf :: Rule -> Char
charOf (_, x, _, _, _) = x

nextCharOf :: Rule -> Char
nextCharOf (_, _, x, _, _) = x

actionOf :: Rule -> Action
actionOf (_, _, _, x, _) = x

nextStateOf :: Rule -> State
nextStateOf (_, _, _, _, x) = x


-- ========================================================================== --
                         -- Accessing to Machines --
-- ========================================================================== --

{- A machine is composed by a set of rules, a blank symbol and a initial state.
   So, this functions are here to acces to every part of them                  -}
rulesOf :: Machine -> Rules
rulesOf (x,_,_) = x

initialState :: Machine -> State
initialState (_,x,_) = x

blankOf :: Machine -> Char
blankOf (_,_,x) = x

makeMachine :: Rules -> State -> Char -> Machine
makeMachine rules istate blank = (rules, istate, blank)


-- ========================================================================== --
                -- Running a Turing Machine --
-- ========================================================================== --

{- It takes a Tape, the current State and the Machine-rules, and returns
   the new Tape, the new State, and: False if the tape changed, and True
   if no rule was applied.
   t = current tape, s = current state, (r:rs) = rules                        -}
oneTapeChange :: Tape -> State -> Rules -> (Tape, State, Bool)
oneTapeChange t s []     = (t, s, True)
oneTapeChange t s (r:rs) = if st == s && c1 == currentChar t
                               then ((moveTape t c2 ac), ns, False)
                               else oneTapeChange t s rs
    where st = stateOf r
          c1 = charOf r
          c2 = nextCharOf r
          ac = actionOf r
          ns = nextStateOf r

{- It applies the oneTapeChange function until this one returns false on the
   'end' sector. It also keep record of every different Tape, after every step,
   in a sequential order.
   t = current Tape, s = current state, rs = rules, ts = lsit of tapes, e = end
                                                                              -}
runTMH :: Tape -> State -> Rules -> [Tape] -> Bool -> [Tape]
runTMH t s rs ts e = if e then (t:ts)
                        else let (newT, newS, end) = oneTapeChange t s rs
                               in runTMH newT newS rs (t:ts) end

{- With the help of runTMH (runTM Helper), it returns a list with every
   change that the Tape has suffer a long the process.
   sl = sides-length. -}
runTM :: String -> Machine -> Int -> [Tape]
runTM tape machine sl = runTMH (newTape b tape sl) initial rules [] False
  where initial = initialState machine
        rules   = rulesOf machine
        b       = blankOf machine
