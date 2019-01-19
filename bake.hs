#! /usr/bin/env nix-shell
#! nix-shell --pure shell.nix -i runghc-bake

--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

module Main
  ( main -- :: IO ()
  ) where

import qualified Bake

main :: IO ()
main = Bake.main
