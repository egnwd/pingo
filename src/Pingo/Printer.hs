module Pingo.Printer (output, printer, Color (..), Colorable (..)) where

import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)

import Pingo.AST
import Pingo.Printer.Color

printer :: (Colorable a, Show a) => Color -> IO (a -> String)
printer c = do
  istty <- queryTerminal stdOutput
  return $ if c == Always || (istty && c /= Never)
     then color
     else show

-- | The 'output' function takes an ID-Answer Set tuple
-- and prints it according to the 'Color' option
output :: Color -> AnswerSet -> IO ()
output c as = do
  p <- printer c
  putStrLn $ p as
