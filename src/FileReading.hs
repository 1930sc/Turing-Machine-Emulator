module FileReading (fileToMachine, Action(ToRight, ToLeft, Stay)) where

import Data.List.Split (splitOneOf)
import Types

-- ========================================================================== --
   -- Transforms a file into a useful data. A Machine type, to be specific --
-- ========================================================================== --
fileI :: String -> String
fileI xs = [ x | x <- dropWhile (/='=') finded, x /= ' ', x /= '=']
  where
    finded = (tail . takeWhile (/='\n') . dropWhile (/='I')) xs

fileB :: String -> Char
fileB xs = head [x | x <- dropWhile (/='=') finded, x /= ' ', x /= '=']
  where
    finded = (tail . takeWhile (/='\n') . dropWhile (/='B')) xs

noComments :: String -> String
noComments = fn False
  where
    fn _ []                              = []
    fn _ [x]                             = [x]
    fn b (x:y:xs) | b && x == '\n'       = x : fn False (y:xs)
                  | b                    = fn True (y:xs)
                  | x == ';' && y == ';' = fn True xs
                  | otherwise            = x : fn b (y:xs)

cleanRules :: String -> [String]
cleanRules xs = splitOneOf "()" f
  where
    f = tail [x | x <- dropWhile (/='=') finded, x /= '\n']
    finded = (tail . dropWhile (/='R')) xs

listRules :: String -> [[String]]
listRules = rightSizes . filtEmpty . map (splitOneOf " ") . cleanRules
  where
    filtEmpty  = map (filter (/=""))
    rightSizes = filter (\l -> length l == 5)

fileRules :: String -> Rules
fileRules = map listToRule . listRules

listToRule :: [String] -> Rule
listToRule (a:b:c:d:e:xs) = Rule (State a) (head b) (head c) ac (State e)
  where
    ac | d == "->" = ToRight
       | d == "<-" = ToLeft
       | otherwise = Stay

fileToMachine :: String -> Machine
fileToMachine xs = Machine rules (State istate) blank
  where
    nxs    = noComments xs
    istate = fileI nxs
    blank  = fileB nxs
    rules  = fileRules nxs
