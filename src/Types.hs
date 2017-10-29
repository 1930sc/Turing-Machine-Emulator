module Types where

data Action  = ToLeft | ToRight | Stay deriving (Eq, Show)

-- ========================================================================== --
                -- Types definitions and Types synonyms --
-- ========================================================================== --
newtype State = State { unState :: String }
  deriving (Eq)

type Rules = [Rule]

-- | A machine is composed by a set of rules, a blank symbol and a initial state.
data Machine = Machine 
             { rulesOf :: Rules
             , initialState :: State
             , blankOf :: Char 
             }

makeMachine :: Rules -> State -> Char -> Machine
makeMachine = Machine

data Rule    = Rule 
             { stateOf :: State
             , charOf :: Char
             , nextCharOf :: Char
             , actionOf :: Action
             , nextStateOf :: State
             }

ruleTuple :: State -> Char -> Char -> Action -> State -> Rule
ruleTuple = Rule

data Tape    = Tape 
             { currentChar :: Char 
             -- ^ It returns the current char of a tape. Or in other words, the symbols that
             -- it's currently under the head.                                             
             , tapeLeft :: [Char]
             , tapeRight :: [Char]
             , displayChars :: Int
             }

{-| ls = left side, rs = right side, x = 'head' and n = Chars to print.
    Because the Tape is infinite, is necesary to tell the computer, how many
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
