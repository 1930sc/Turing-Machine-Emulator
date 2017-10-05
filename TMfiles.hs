module TMfiles
 ( fileToMachine
 , Action(ToRight, ToLeft, Stay))
 where

import Data.List.Split

data Action  = ToLeft | ToRight | Stay deriving (Eq, Show)

type State   = String
type Rule    = (State, Char, Char, Action, State)
type Rules   = [Rule]
type Machine = (Rules, State, Char)


-- ========================================================================== --
   -- Transforms a file into a useful data. A Machine type, to be specific --
-- ========================================================================== --
fileI :: String -> String
fileI xs = [ x | x <- dropWhile (/='=') finded, x /= ' ', x /= '=']
  where finded = (tail . takeWhile (/='\n') . dropWhile (/='I')) xs

fileB :: String -> Char
fileB xs = head [x | x <- dropWhile (/='=') finded, x /= ' ', x /= '=']
  where finded = (tail . takeWhile (/='\n') . dropWhile (/='B')) xs

noComments :: String -> String
noComments xs = fn xs False
  where fn []     _ = []
        fn [x]    _ = [x]
        fn (x:y:xs) b | b && x == '\n'       = x : fn (y:xs) False
                      | b                    = fn (y:xs) True
                      | x == ';' && y == ';' = fn xs True
                      | otherwise            = x : fn (y:xs) b

cleanRules :: String -> [String]
cleanRules xs = splitOneOf "()" f
  where f = tail [x | x <- dropWhile (/='=') finded, x /= '\n']
        finded = (tail . dropWhile (/='R')) xs

listRules :: String -> [[String]]
listRules = rightSizes . filtEmpty . map (splitOneOf " ") . cleanRules
    where filtEmpty  = map (filter (/=""))
          rightSizes = filter (\l -> length l == 5)

fileRules :: String -> Rules
fileRules = map listToRule . listRules

listToRule :: [String] -> Rule
listToRule (a:b:c:d:e:xs) = ruleTuple a (head b) (head c) ac e
  where ac | d == "->" = ToRight
           | d == "<-" = ToLeft
           | otherwise = Stay

makeMachine :: Rules -> State -> Char -> Machine
makeMachine rules istate blank = (rules, istate, blank)

ruleTuple :: State -> Char -> Char -> Action -> State -> Rule
ruleTuple a b c d e = (a, b, c, d, e)

fileToMachine :: String -> Machine
fileToMachine xs = makeMachine rules istate blank
  where nxs    = noComments xs
        istate = fileI nxs
        blank  = fileB nxs
        rules  = fileRules nxs
