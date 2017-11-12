module Main where

import Pingo

main :: IO ()
main = do
  input <- getContents
  case parse input of
    Left err -> print err
    Right as -> mapM_ output as
