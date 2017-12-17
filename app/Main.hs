module Main where

import Data.Time.Clock
import FileReading        (fileToMachine)
import MachineFunctions   (runTM, Tape)
import System.Environment
import System.IO

{- It takes the path of a '.tm' file, a string representing the initial Tape,
   the length of the tape to be showed (see 'Show Tape' instance) and asks if
   it should be printed just the final tape, or the tape after every step     -}
main :: IO ()
main = do
  args <- getArgs
  if any (==args) [["-help"], ["-h"], ["help"], ["h"], []]

    then helpText

    else
      do infile   <- openFile (head args) ReadMode
         contents <- hGetContents infile
         let tape  = args!!1
             len   = read (args!!2)
             tapes = runTM tape (fileToMachine contents) len
           in if last args `elem` ["true", "t", "1"]
                then
                  do putStrLn "\nTapes :"
                     mapM_ print $ reverse tapes
                     putStrLn $ (++) "\nN° of steps : " $ show $ length tapes
                     
                else
                  do start <- utctDayTime <$> getCurrentTime
                     putStrLn "\nFinal tape  :"
                     print $ head tapes
                     end   <- utctDayTime <$> getCurrentTime
                     putStrLn $ (++) "\nN° of steps : " $ show $ length tapes
                     putStrLn $ "CPU Time    : " ++ show (end-start) ++ "\n"

helpText = putStrLn $ concat
  [ "\nThis program require some arguments in this form: \n"
  , "turing-machine-emulator- exe <PATH> <TAPE> <NUMBER> <BOOL>\n"
  , "Where :\n"
  , "  PATH   : The .tm file path.\n"
  , "  TAPE   : The initial tape to run.\n"
  , "  NUMBER : The length of the infinite tape to be shown to both siedes\n"
  , "  BOOl   : Can be true or empty, and in the first case, instead of \n"
  , "            printing the final tape, will print every tape after every\n"
  , "            change made\n"
  , "Some examples :\n"
  , "  turing-machine-emulator-exe Machines/palindrome.tm 10101 7 t\n"
  , "  turing-machine-emulator-exe Machines/palindrome.tm 1000 10 \n"
  ]
