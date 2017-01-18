import RandState
import State
import System.Environment
import System.IO
import System.Random
import Data.List
import Control.Monad

randR :: Random a => (a, a) -> RandState a
randR bound = do
  gen <- get
  let (x, gen') = randomR bound gen
  put gen'
  return x

euclideanDistOrigin :: (Double, Double) -> Double
euclideanDistOrigin (x, y) = sqrt (x^2 + y^2)

piTrial :: RandState Bool
piTrial = do
  x <- randR (-1, 1)
  y <- randR (-1, 1)
  return (euclideanDistOrigin (x, y) < 1.0)

approxPi :: Int -> StdGen-> Double
approxPi n gen = 
  let results = runRandom (replicateM n piTrial) gen
      sum     = foldr1 (+) $ map (\b -> if b then 1 else 0) results
  in 4 * (fromIntegral sum) / (fromIntegral n)
  
main :: IO ()
main = do
  gen  <- newStdGen
  args <- getArgs
  case args of
    [n] -> putStrLn . show $ approxPi (read n) gen
    _   -> error "just input a number"
