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
bdir = ("build" </>)

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
specSources mainFile = pure $ map (\x -> "src/spec" </> x <.> "sail")
  [ "setup", "basics", "scattered", fromMaybe "elfmain" mainFile ]

-- | Top-level set of all rules for building the emulator.
emulatorRules :: Rules ()
emulatorRules = do
  -- main emulator: 'cruise' C backend, with default ELF loader
  specSources Nothing >>= sailSimRules SailBackendC (bdir "cruise")

--------------------------------------------------------------------------------
-- Tests

getInstrTestFiles :: Action [FilePath]
getInstrTestFiles = getDirectoryFiles "src" [ "/t/tests/*.S" ]

getFirmwareTestFiles :: Action [FilePath]
getFirmwareTestFiles = getDirectoryFiles "src"
  [ "/t/firmware/*.c", "/t/firmware/*.S" ]

testRules :: Rules ()
testRules = do

  let takeBaseName1 = dropExtensions . takeBaseName

  -- assembles the files in ./src/t/tests/
  let tasm out name = do
        let src = "src/t/tests" </> name <.> "S"
        need [ src ]
        cmd "riscv32-unknown-elf-gcc -march=rv32im"
          [ "-c", "-o", out ]
          ("-DTEST_FUNC_NAME=" <> name)
          ("-DTEST_FUNC_TXT=" <> show name)
          ("-DTEST_FUNC_RET=" <> name <> "_ret")
          src

  -- links rv32 objects together
  let rvld out objs = do
        let ldsect = "src/t/firmware/sections.lds"
            ldmap  = "src/t/firmware/firmware.map"
            flags  = [ "-Wl,-Bstatic,-T,", ldsect, ",-Map,", ldmap, ",--strip-debug" ]

        need (ldsect:objs)
        cmd "riscv32-unknown-elf-gcc"
          [ "-ffreestanding", "-nostdlib" ]
          [ "-o", out ]
          (concat flags)
          objs
          [ "-lgcc" ]

  -- compiles C firmware test files
  let rvcc out name = do
        let src = "src/t/firmware" </> name <.> "c"

        need [ src ]
        cmd "riscv32-unknown-elf-gcc -march=rv32ic"
          [ "-Os", "-pedantic", "-std=c99" ]
          [ "-Werror", "-Wall", "-Wextra", "-Wshadow", "-Wundef"
          , "-Wpointer-arith", "-Wcast-qual", "-Wcast-align", "-Wwrite-strings"
          , "-Wredundant-decls", "-Wstrict-prototypes", "-Wmissing-prototypes"
          ]
          [ "-ffreestanding", "-nostdlib" ]
          [ "-c", "-o", out, src ]

  -- assembles firmware test files
  let rvasm out name = do
        let src = "src/t/firmware" </> name <.> "S"
        need [ src ]
        cmd "riscv32-unknown-elf-gcc -march=rv32imc"
          [ "-c", "-o", out, src ]

  bdir "selftest.elf" %> \out -> do
    asm_srcs <- getInstrTestFiles
    fw_srcs <- getFirmwareTestFiles

    let asm_objs = [ bdir v -<.> "asm.o" | v <- asm_srcs ]
        fw_objs  = [ bdir v -<.> "asm.o" | v <- fw_srcs, takeExtensions v == ".S" ]
                <> [ bdir v -<.> "c.o" | v <- fw_srcs, takeExtensions v == ".c" ]

    rvld out (fw_objs <> asm_objs)

  -- Test rules
  bdir "t/tests/*.asm.o" %> \out -> tasm out (takeBaseName1 out)
  bdir "t/firmware/*.asm.o" %> \out -> rvasm out (takeBaseName1 out)
  bdir "t/firmware/*.c.o"   %> \out -> rvcc  out (takeBaseName1 out)

--------------------------------------------------------------------------------
-- Driver

-- | Main entry point
main :: IO ()
main = shakeArgsWith myShakeOptions myFlags $ \_ targets -> pure $ Just $ do
  want (if not (null targets) then targets else [ "all" ])

  -- phony rules which are shorthands
  "all" ~> need [ bdir "cruise", bdir "selftest.elf" ]
  "clean" ~> removeFilesAfter (bdir mempty) ["//*"]

  -- all main, top-level rule sets
  emulatorRules
  testRules
