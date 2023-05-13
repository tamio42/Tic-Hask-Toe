-- import Data.Char
import Data.List 
import System.IO
import Data.Text (replace)

type Secret = String
type Word   = String
type Guess  = Char
type Letter = Char
type Lives  = Int

getSecret :: IO String
getSecret = do
    hSetEcho stdin False -- turn off input visibility
    putStrLn "Enter the secret"
    s <- getLine
    hSetEcho stdin True -- turn on input visibility
    case length s > 4 && all isAlpha s of
        True -> return (fmap toUpper s)
        False -> do
            putStrLn "Invalid input! Please enter again"
            getSecret

getGuess :: IO Char
getGuess = do
    s <- getLine
    case null (tail s) of
        True -> return (toUpper $ head s)
        False -> do
            putStrLn "Invalid input! Please enter again"
            getGuess

data Game = Game {
    --  gSecret  :: String
     gWord    :: [(Char,Char)]
    ,gLives   :: Int
    ,gLetters :: String
}

replace2 :: Char -> String -> String
replace2 _ [] = []
replace2 c (x:xs) = if x == c then '_' : replace2 c xs else x : replace2 c xs

mkGame :: Secret -> Game
mkGame secret = Game (zip secret (fmap (\_ -> '_') secret )) 7 ['A'..'Z']

playGuess :: Guess -> Game -> Game
playGuess guess game =   
    case (elem guess (gLetters game)) of
        False -> game
        True  -> 
            let word    = gWord game
                lives   = gLives game
                letters = gLetters game
                secret  = fmap fst word
                charI   = elemIndex guess letters
            in case elem guess secret of
                True -> 
                    let word'    = fmap (\(s, g) -> if guess == s 
                                                    then (s,s)
                                                    else (s,g)) word
                        -- letters' = delete guess letters
                        -- letters' = take charIndex letters ++ "_" ++ (drop (4+1) letters)
                        -- letters' = replace guess '_' letters
                        letters' = replace2 guess letters
                    in game { gWord = word', gLetters = letters'}
                False -> 
                    let lives'   = lives - 1 
                        letters' = replace2 guess letters 
                    in game { gLives = lives', gLetters = letters'}

addSpaces :: String -> String
addSpaces str = intercalate " " (map (:[]) str)

playGame :: Game -> IO ()
playGame game = do
    putStrLn "\n"
    -- putStrLn (fmap fst (gWord game))
    -- putStrLn (fmap snd (gWord game)) 
    putStrLn $ addSpaces $ fmap snd (gWord game)
    putStrLn "\n"
    putStrLn "*****************************\n"
    putStrLn "Your guess..."
    guess <- getGuess
    let game'    = playGuess guess game
        lives'   = gLives game'
        letters' = gLetters game'
        secret' =  fmap fst (gWord game)
    putStrLn $ "\nLives left: " ++ (show lives' ++ "\n")
    putStrLn $ "Leftover letters: " ++ (letters')
    case isWin game' of
        True -> putStrLn $ "You won! Good joooorb! \n"
                         ++ "The word is: " ++ secret' ++ "\n"
        False -> case isLoss game' of
            True ->  putStrLn "You lost, you loser!"
            False -> playGame game'
    where
    isWin game  = all (\(_,g) -> g /= '_') (gWord game)
    isLoss game = gLives game < 0

main = do
    secret <- getSecret
    let game = mkGame secret
    playGame game