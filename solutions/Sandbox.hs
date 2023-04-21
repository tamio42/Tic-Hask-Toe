module Sandbox where
import A1
import A2
-- import A3
import Text.Read (Lexeme(String))
import Data.List


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

ints :: [[Int]]
ints = [[1,2,3],[4,5,6],[7,8,9]]

-- elem' :: Eq a => a -> [a] -> Bool
-- elem' a [] = False
-- elem a (x)

-- elem' a xs = foldr (\x b -> if a == x then True else b) False xs
-- [(a,Int)]
myL :: [String] -> Int
myL xs = go xs where
 go :: [String] -> Int
 go [] = 0
 go (x:xs) = 1 + go xs

myR :: String -> String
myR s = go [] s where
 go :: String -> String -> String
 go acc [] = acc
 go acc (x:xs) = go (x : acc) xs


getInt :: IO Int
getInt = getLine >>= \s -> return ((read s) :: Int)

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
printElements (x:xs) = do putStrLn x
                          printElements xs
                          
names :: [String]
names = ["a","b","c"]

main :: IO [()]
main = sequence (map putStrLn names)