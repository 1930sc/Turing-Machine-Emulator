module Main where

import MachineFunctions (runTM, Tape)
import FileReading (fileToMachine)
import System.IO
import Data.Time.Clock

{- It takes the path of a '.tm' file, a string representing the initial Tape,
   the length of the tape to be showed (see 'Show Tape' instance) and asks if
   it should be printed just the final tape, or the tape after every step     -}
main :: IO ()
main = do
  putStrLn "Turing Machine Path: "
  file     <- getLine
  infile   <- openFile file ReadMode
  contents <- hGetContents infile
  putStrLn "\nInitial Tape: "
  tape <- getLine
  putStrLn "\nVisible length of the Tape: "
  len <- ((`div` 2) . read) <$> getLine
  putStrLn "\nJust final tape[0], or every step[1]?: "
  out <- getLine
  let procedure = runTM tape (fileToMachine contents) len
    in if out == "1"
         then do mapM_ print $ reverse procedure
                 putStrLn $ (++) "\nN° of steps : " $ show (length procedure)

         else do start <- utctDayTime <$> getCurrentTime
                 print $ head procedure
                 end   <- utctDayTime <$> getCurrentTime
                 putStrLn $ (++) "\nN° of steps : " $ show (length procedure)
                 putStrLn $ "CPU Time    : " ++ show (end-start) ++ "\n"
