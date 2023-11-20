module Starman where

import System.Random
import System.Random.Stateful (UniformRange(uniformRM))

check :: String -> String -> Char -> (Bool, String)
check word display c =
  let correct = c `elem` word
      display' = [if x == c then c else y | (x, y) <- zip word display]
   in (correct, display')

turn :: String -> String -> Int -> IO ()
turn word display n = do
  if n == 0
    then putStrLn $ "You lose... The word was: " ++ word
    else
      if word == display
        then putStrLn $ "You win! The word was: " ++ word
        else mkGuess word display n
  where
    mkGuess :: String -> String -> Int -> IO ()
    mkGuess word display n = do
      putStrLn (display ++ " " ++ replicate n '*')
      putStr " Enter your guess: "
      guess <- getLine
      let (correct, display') = check word display $ head guess
      let n' = if correct then n else n - 1
      turn word display' n'

getWords :: IO [String]
getWords = words <$> readFile "words.txt"

generateRandom :: (Random a) => IO a
generateRandom = fst . random <$> getStdGen

getRandomWord :: IO String
getRandomWord = do
  words <- getWords
  index <- randomRIO (0, length words) :: IO Int
  return $ words !! index

starman :: IO ()
starman = do
  word <- getRandomWord
  turn word ['-' | x <- word] 7
