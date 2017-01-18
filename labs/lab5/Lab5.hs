import RandState
import State
import System.Environment
import System.IO
import System.Random
import Data.List
import Control.Monad

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]

instance Show PlayingCard where
  show (PlayingCard value suit) =
    valueStr value ++ suitStr suit
      where
        suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
        suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
        suitStr Spades   = "♠"
        suitStr Clubs    = "♣"
        -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
        -- suitStr Diamonds = "D"
        -- suitStr Spades   = "S"
        -- suitStr Clubs    = "C"
        valueStr King           = "K"
        valueStr Queen          = "Q"
        valueStr Jack           = "J"
        valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
  [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
      allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
      allSuits = [Hearts, Diamonds, Spades, Clubs]

randR :: Random a => (a, a) -> RandState a
randR bound = do
  gen <- get
  let (x, gen') = randomR bound gen
  put gen'
  return x

rollTwoDice :: RandState Int
rollTwoDice = do
  a <- randR (1, 6)
  b <- randR (1, 6)
  return (a + b)

remove :: [a] -> Int -> [a]
remove ls i = front ++ tail back where
  (front, back) = splitAt i ls 

removeCard :: [PlayingCard] -> RandState (PlayingCard, [PlayingCard])
removeCard cards = do 
  rm <- randR (0, (length cards) - 1)
  return (cards !! rm, cards `remove` rm)

--shuffleDeck :: [PlayingCard] -> RandState [PlayingCard]
shuffleDeck = shuffleDeck_ [] where
  shuffleDeck_ shuffled deck = do
    (new, todo) <- removeCard deck
    case todo of
      [] -> return (new : shuffled)
      _  -> shuffleDeck_ (new : shuffled) todo

-- -- UNCOMMENT THE FOLLOWING BEFORE SUBMISSION --
shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = do
  let results = runRandom (replicateM nTimes (shuffleDeck fullCardDeck)) gen
  putStrLn $ intercalate ['\n'] $ map show results
  

rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen = do
  let results = runRandom (replicateM nTimes rollTwoDice) gen
  putStrLn $ intercalate ['\n'] $  map show results


-- BESIDES UNCOMMENTING, DO NOT MODIFY BELOW THIS LINE --

usage :: String
usage =
  "Lab 5: Randomizer\n" ++
  "\n" ++
  "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
  "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
  "\n"

main :: IO ()
main = do
  gen  <- newStdGen
  args <- getArgs
  case args of
    ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
    ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
    _                       -> putStrLn usage
