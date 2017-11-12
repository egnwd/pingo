module Main where

import Control.Applicative
import Options

import Pingo

data MainOptions = MainOptions
  { optColor :: Color
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> defineOption (optionType_enum "color") (\o -> o
      { optionLongFlags = ["color"]
      , optionDefault = Auto
      })

main :: IO ()
main = runCommand $
  (\opts args ->
    do
    input <- getContents
    case parse input of
      Left err -> print err
      Right as -> mapM_ (output $ optColor opts) $ zip [1..] as
  )