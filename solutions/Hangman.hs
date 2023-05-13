import Data.Char
import Data.List

type Secret = String
type Word   = String
type Guess  = Char
type Letter = Char
type Lives  = Int

getSecret :: IO String
getSecret = do
    putStrLn "Enter the secret"
    s <- getLine
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
            in case elem guess secret of
                True -> 
                    let word'    = fmap (\(s, g) -> if guess == s 
                                                    then (s,s)
                                                    else (s,g)) word
                        letters' = delete guess letters
                    in game { gWord = word', gLetters = letters'}
                False -> 
                    let lives'   = lives - 1 
                        letters' = delete guess letters 
                    in game { gLives = lives', gLetters = letters'}


playGame :: Game -> IO ()
playGame game = do
    putStrLn "\n"
    -- putStrLn "Lives left: "
    putStrLn (fmap fst (gWord game))
    putStrLn (fmap snd (gWord game))
    -- putStrLn "Leftover letters: "
    putStrLn "\n"
    guess <- getGuess
    let game' = playGuess guess game
    case isWin game' of
        True -> putStrLn $ "You won! Good joooorb!"
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