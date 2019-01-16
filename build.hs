#! /usr/bin/env nix-shell
#! nix-shell --pure shell.nix -i runghc2

--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Main
  ( main -- :: IO ()
  ) where

--------------------------------------------------------------------------------
-- Imports

-- base
import           Data.Maybe    ( fromMaybe )

-- shake
import           Development.Shake
import           Development.Shake.FilePath

-- local imports
import           Options ( myShakeOptions, myFlags )
import           Sail    ( SailBackend(..), sailSimRules )

--------------------------------------------------------------------------------
-- Basics

-- | Append the build directory to a path; this is just a shorthand so the
-- build directory is not hardcoded everywhere.
bdir :: FilePath -> FilePath
bdir = ("_make" </>)

--------------------------------------------------------------------------------
-- Main Shake Ruleset

-- | Get the set of Sail source files for the ISA specification.
specSources
  :: Monad m
  => Maybe FilePath
  -- ^ Which file to use for the main entry point. If @'Nothing'@ is specified,
  -- then the default entry point, which uses an ELF-based loader, will be
  -- selected.
  -> m [FilePath]
  -- ^ Resulting list of Sail source files.
specSources mainFile = pure $ map (\x -> "spec" </> x <.> "sail")
  [ "setup", "basics", "scattered", fromMaybe "elfmain" mainFile ]

-- | Top-level set of all rules for the build system.
mainRules :: Rules ()
mainRules = do
  -- main emulator: 'cruise' C backend, with default ELF loader
  specSources Nothing >>= sailSimRules SailBackendC (bdir "cruise")

  -- TODO FIXME: build firmware source code

--------------------------------------------------------------------------------
-- Driver

-- | Main entry point
main :: IO ()
main = shakeArgsWith myShakeOptions myFlags $ \_ targets -> pure $ Just $ do
  want (if not (null targets) then targets else [ "all" ])

  -- phony rules which are shorthands
  "all" ~> need [ bdir "cruise" ]
  "clean" ~> removeFilesAfter (bdir mempty) ["//*"]

  mainRules
