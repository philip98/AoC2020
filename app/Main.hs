module Main where

import Day01
import Day02

main :: IO ()
main = do
  putStrLn "--Day 01--"
  expenseReport
  expenseReport'
  putStrLn "--Day 02--"
  checkPasswords
  checkPasswords'
