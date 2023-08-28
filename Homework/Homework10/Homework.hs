{-
-- Question 1 --
Continuing with the logistics software of the lesson:
 1. After using the `Container` type class for a while, you realize that it might need a few adjustments:
  	- First, write down the `Container` type class and its instances, same as we did in the lesson
  	  (try to do it without looking and check at the end or if you get stuck).
  	- Then, add a function called `unwrap` that gives you back the value inside a container.
 2. Create an instance for the `MailedBox` data type.
 	- The MailedBox data type represents a box sent through the mail.
 	- The parameter `t` is a tag with a person's identifier
 	- The parameter `d` is the person's details (address,etc).
 	- The parameter `a` is the content of the MailedBox
-}

class Container c where
  isEmpty :: c a -> Bool
  contains :: Eq a => c a -> a -> Bool
  replace :: c a -> b -> c b
  unwrap :: c a -> a

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a deriving (Show)

instance Container (MailedBox t d) where
  isEmpty (EmptyMailBox {}) = True
  isEmpty (MailBoxTo {}) = False
  contains (EmptyMailBox {}) _ = False
  contains (MailBoxTo _ _ a) a' = a == a'
  replace (EmptyMailBox t d) = MailBoxTo t d
  replace (MailBoxTo t d _) = MailBoxTo t d
  unwrap (MailBoxTo _ _ a) = a
  unwrap (EmptyMailBox _ _) = error "cannot unwrap empty mail box"

-- Question 2 --
-- Create instances for Show, Eq, and Ord for these three data types (use
-- automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief deriving (Show, Eq, Ord)

data Experience = Programming | Managing | Leading deriving (Show, Eq, Ord)

type Address = String

data Salary = USD Double | EUR Double deriving (Show)

instance Eq Salary where
  (==) (USD usd) (USD usd') = usd == usd'
  (==) (EUR eur) (EUR eur') = eur == eur'
  (==) _ _ = False

instance Ord Salary where
  compare (USD usd) (USD usd')
    | usd == usd' = EQ
    | usd - usd' < 0 = LT
    | otherwise = GT
  compare (EUR eur) (EUR eur')
    | eur == eur' = EQ
    | eur - eur' < 0 = LT
    | otherwise = GT
  compare _ _ = error "cannot compute i need a exchange rate"

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Show, Eq, Ord)

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }
  deriving (Show, Eq)

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS
-- Uncomment the next code and make it work (Google what you don't know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show, Num)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0
