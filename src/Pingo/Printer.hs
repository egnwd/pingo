{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Pingo.Printer (output) where

import System.Console.ANSI
import Data.List

import Pingo.AST

class Colorable a where
  color :: a -> String

instance Colorable Ident where
  color i = setSGRCode [SetColor Foreground Vivid Blue] ++ i ++ (setSGRCode [Reset])

instance Colorable Number where
  color n = setSGRCode [SetColor Foreground Dull Yellow] ++ (show n) ++ (setSGRCode [Reset])

instance Colorable Argument where
  color (Lit atom) = color atom
  color (Num n) = color n
  color (Sep s) = s

instance Colorable Atom where
  color (Atom name []) = color name
  color (Atom name args) =
    (setSGRCode [SetColor Foreground Vivid Green]) ++ name ++ (setSGRCode [Reset])
    ++ "(" ++ (concatMap color $ intersperse (Sep ", ") args) ++ ")"

-- Printers
prettyLn :: (Colorable a, Show a) => a -> IO ()
prettyLn = (>> putStrLn "") . (putStr "\t" >>) . pretty

pretty :: (Colorable a, Show a) => a -> IO ()
pretty = putStr . color

output (n, a) = (putStrLn $ "Answer " ++ (show n) ++ ":") >> mapM_ prettyLn a

