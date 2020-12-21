module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

main :: IO ()
main = do
  putStrLn "--Day 01--"
  expenseReport
  expenseReport'
  putStrLn "--Day 02--"
  checkPasswords
  checkPasswords'
  putStrLn "--Day 03--"
  countTrees
  countTrees'
  putStrLn "--Day 04--"
  countPassports
  putStrLn "--Day 05--"
  analyzeIDs
  putStrLn "--Day 06--"
  sumAnswers
