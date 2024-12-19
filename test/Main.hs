{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Advent
    ( runAoC,
      mkDay_,
      AoC(AoCSubmit, AoCInput),
      AoCOpts(AoCOpts),
      AoCUserAgent(AoCUserAgent),
      Part(Part2, Part1),
      SubmitRes(SubCorrect, SubUnknown, SubInvalid) )
import Control.Monad (unless,)
import Data.Maybe
import Data.Either (fromRight)
import Data.Text (unpack)
import Day01 qualified
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

solutions :: [(Int, String -> String, String -> String)]
solutions =
  [ (1, Day01.part1, Day01.part2)
  ]

user_agent :: AoCUserAgent
user_agent = AoCUserAgent "https://github.com/nuudlman/aoc2024" "isaac.nudelman@utexas.edu"

mk_opts :: String -> AoCOpts
mk_opts token = AoCOpts token 2024 user_agent (Just ".cache") False 3000000

main :: IO ()
main = do
  opts <- mk_opts <$> maybe (error "AOC_TOKEN not set") id <$> lookupEnv "AOC_TOKEN"
  results <- mapM (submit_day opts) solutions
  unless (all snd results) $ do
    putStrLn $ "Failed days: " ++ show (map fst $ filter (not . snd) results)
    exitFailure

submit_day :: AoCOpts -> (Int, String -> String, String -> String) -> IO (Int, Bool)
submit_day opts (day, part1, part2) = do
  let day' = toInteger day
      fetch_input = runAoC opts $ AoCInput (mkDay_ day')
      submit_answer part answer = runAoC opts $ AoCSubmit (mkDay_ day') part answer

  input <- unpack . (fromRight "") <$> fetch_input
  (_, res1) <- fromRight ("", SubInvalid) <$> submit_answer Part1 (part1 input)
  (_, res2) <- fromRight ("", SubInvalid) <$> submit_answer Part2 (part2 input)

  pure $ (day,) $ case (res1, res2) of
    (SubCorrect _, SubCorrect _) -> True
    _ -> False
