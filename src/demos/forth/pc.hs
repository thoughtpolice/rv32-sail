{-# OPTIONS_GHC -Wall #-}

module Main
  ( main -- :: IO ()
  ) where

import qualified System.Environment as Env

compile :: [String] -> String
compile _ = mempty

main :: IO ()
main = do
  kern <- readFile =<< (fmap head Env.getArgs)
  putStrLn kern
  interact (compile . lines)
