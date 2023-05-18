module Sandbox where

import A1
import A2
-- import A3
-- import A3

import Data.List
import Text.ParserCombinators.ReadP (char)
import Text.Read (Lexeme (String), get)

rev :: String -> String
rev [] = []
rev (x : xs) = rev xs ++ [x]

type Name = String

type Id = Int

type Power = String

data Pokemon = MkPokemon
  { getName :: Name,
    getId :: Id,
    getPowers :: [Power]
  }

-- instance Show Pokemon where
--     show p = unwords [ getName p ++ "\n"
--                      ," NO. " ++ show (getId p) ++ "\n"
--                      ]

instance Show Pokemon where
  show p = getName p ++ "\n " ++ concat (intersperse "\n" $ getPowers p)

-- instance Eq Pokemon where
--     (==) p = getId < getId

-- instance Ord Pokemon where
--     compare :: Pokemon -> Pokemon -> Bool
--     compare            

pokemon = [" ", "bulbasaur", "squirtle", "charmander", " "]

pList =
  [ bulbasaur,
    ivysaur,
    venusaur,
    squirtle,
    charmander,
    charizard,
    pikachu
  ]

baobamon :: Pokemon
baobamon =
  MkPokemon
    { getName = "Babobamon",
      getId = 69,
      getPowers = ["Cook", "Grow Plants"]
    }

bmon :: Pokemon
bmon =
  MkPokemon
    { getName = "BMon",
      getId = 42,
      getPowers = ["","Cook", "Grow Plants",  "Clean", "Install", "Fix", "Design"]
    }



bulbasaur :: Pokemon
bulbasaur = MkPokemon "Bulbasaur" 1 ["Grass", "Poison"]

ivysaur :: Pokemon
ivysaur = MkPokemon "Ivysaur" 1 ["Grass", "Poison"]

venusaur :: Pokemon
venusaur = MkPokemon "Venusaur" 1 ["Grass", "Poison"]

squirtle :: Pokemon
squirtle = MkPokemon "Squirtle" 7 ["Water"]

charmander :: Pokemon
charmander = MkPokemon "Charmander" 4 ["Fire"]

charizard :: Pokemon
charizard = MkPokemon "Charizard" 4 ["Fire", "Flying"]

pikachu :: Pokemon
pikachu = MkPokemon "Pikachu" 4 ["Fire"]

-- getPowers :: [Pokemon] -> [Power]
-- getPowers p = foldr (\_ _ p -> p:[]) p

getNames :: [Pokemon] -> [Name]
getNames [] = []
getNames ((MkPokemon n _ _) : ps) = n : getNames ps

-- getPowers :: [Pokemon] -> [Power]
-- getPowers [] = []
-- getPowers ((MkPokemon _ _ p) : ps) =  concat p : getPowers ps

-- getId :: [Pokemon] -> [Int]
-- getId [] = []
-- getId ((MkPokemon _ i _) : xs) = i : getId xs

vowel :: Char -> Bool
vowel c = c `elem` "AEIOUaeiou"

onlyVowels :: String -> String
onlyVowels [] = []
onlyVowels (x : xs) = if vowel x then x : onlyVowels xs else onlyVowels xs

stringList :: [String]
stringList = ["cop", "Call", "plant", "People", "woman", "Want", "apple", "Alcohol"]

b = 0

m = foldr (\a b -> if vowel a then b + 1 else b) b "fsdazvxcoiweir"

length7 :: String -> Bool
length7 s = length s == 7

includes :: Eq a => a -> [a] -> Bool
includes a [] = False
includes a (x : xs)
  | a == x = True
  | otherwise = includes a xs

-- pangram :: String -> Bool
-- pangram [] = False
-- pangram (x:xs) = if x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] then True else pangram xs

pangram :: String -> Bool
pangram [] = False
pangram s = all (`elem` s) ['a' .. 'z']

ints :: [[Int]]
ints = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

-- elem' :: Eq a => a -> [a] -> Bool
-- elem' a [] = False
-- elem a (x)

-- elem' a xs = foldr (\x b -> if a == x then True else b) False xs
-- [(a,Int)]
myL :: [String] -> Int
myL xs = go xs
  where
    go :: [String] -> Int
    go [] = 0
    go (x : xs) = 1 + go xs

myR :: String -> String
myR s = go [] s
  where
    go :: String -> String -> String
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

getInt :: IO Int
getInt = getLine >>= \s -> return (read s :: Int)

-- main =
--  getLine >>= \a ->
--  getLine >>= \b ->
--  getLine >>= \c ->
--  putStrLn (a ++ b ++ c)
-- putStrLn (show (a ++ b ++ c))

-- main =
--  getLine >>= \a ->
--  getLine >>= \b ->
--  getLine >>= \c ->

--  putStrLn (a ++ b ++ c)

printElements :: [String] -> IO ()
printElements [] = return ()
printElements (x : xs) = do
  putStrLn x
  printElements xs

-- getPowers' = nub . concatMap stringList

addOneList' lst = map addOne' lst
  where
    addOne' x = x + 1

-- addOneList lst = map (\x -> x + 1) lst