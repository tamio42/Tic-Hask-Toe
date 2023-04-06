module Sandbox where


rev :: String -> String
rev []     = []
rev (x:xs) = rev xs ++ [x]

type Name = String
type Id = Int
type Power = String
data Pokemon = MkPokemon Name Id [Power]
pokemon = [ " ","bulbasaur", "squirtle", "charmander"," "]

bulbasaur :: Pokemon
bulbasaur = MkPokemon "Bulbasaur" 1 ["Grass", "Poison"]

squirtle :: Pokemon
squirtle = MkPokemon "Squirtle" 7 ["Water"]

charmander :: Pokemon
charmander = MkPokemon "Charmander" 4 ["Fire"]
 

getNames :: [Pokemon] -> [Name]
getNames [] = []
getNames ((MkPokemon n _ _) : ps) = n : getNames ps

getId :: [Pokemon] -> [Int]
getId [] = []
getId ((MkPokemon _ i _) : xs) = i : getId xs

vowel :: Char -> Bool
vowel c = c `elem` "AEIOUaeiou"

onlyVowels :: String -> String
onlyVowels []     = []
onlyVowels (x:xs) = if vowel x then x : onlyVowels xs else onlyVowels xs

stringList :: [String]
stringList = ["cop", "Call", "plant", "People", "woman", "Want", "apple", "Alcohol"]
b = 0
m = foldr (\a b -> if vowel a then b + 1 else b) b "fsdazvxcoiweir"

length7 :: String -> Bool
length7 s = length s == 7

-- factors :: Int -> [Int]
-- factors n = loop [1..n]
--   loop []         = []
--   loop (x:xs)
--    | mod n x == 0 = x : loop xs
--    | otherwise    = loop xs

includes :: Eq a => a -> [a] -> Bool
includes a []     = False
includes a (x:xs)
 | a == x   = True
 | otherwise = includes a xs

-- pangram :: String -> Bool
-- pangram [] = False
-- pangram (x:xs) = if x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] then True else pangram xs

pangram :: String -> Bool
pangram [] = False
pangram s = all (`elem` s) ['a'..'z']