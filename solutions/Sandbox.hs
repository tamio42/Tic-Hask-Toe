module Sandbox where


rev :: String -> String
rev []     = []
rev (x:xs) = rev xs ++ [x]

type Name = String
type Id = Int
type Power = String
data Pokemon = MkPokemon Name Id [Power]
pokemon = [bulbasaur, squirtle, charmander]

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