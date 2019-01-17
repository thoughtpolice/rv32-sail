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
import           Sail    ( SailBackend(..), sailSimRules, sailGenHexBits )

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
specSources mainFile = pure $
    -- prelimary setup. these modules are fairly reusable
  [ src "setup", gen "hexbits" ] <>
    -- riscv specific modules
  map src [ "basics", "rv32", "scattered" ] <>
    -- chosen loader entry point
  map src [ fromMaybe "elfmain" mainFile ]
  where

    -- source files which are hand-written (live under ./src)
    src x = "src/spec" </> x <.> "sail"
    -- source files which are generated (live under ./build)
    gen x = bdir x <.> "sail"

-- | Top-level set of all rules for building the emulator.
emulatorRules :: Rules ()
emulatorRules = do
  -- main emulator: 'cruise' C backend, with default ELF loader
  sources <- specSources Nothing
  sailSimRules SailBackendC     (bdir "cruise.opt") sources
  sailSimRules SailBackendOCaml (bdir "cruise")     sources

  -- generate hexbits.sail
  bdir "hexbits.sail" %> sailGenHexBits

--------------------------------------------------------------------------------
-- Tests

getInstrTestFiles :: Action [FilePath]
getInstrTestFiles = getDirectoryFiles "src"
  [ "/t/tests/*.S"
  ]

getFirmwareTestFiles :: Action [FilePath]
getFirmwareTestFiles = getDirectoryFiles "src"
  [ "/t/firmware/*.c"
  , "/t/firmware/*.S"
  ]

testRules :: Rules ()
testRules = do
  -- takeBaseName doesn't drop *all* extensions, e.g. foo.asm.o only
  -- becomes foo.asm
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
            ldmap  = bdir (takeBaseName out <.> "map")
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

    unit $ rvld out (fw_objs <> asm_objs)
    cmd "chmod -x" out

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
  let top = map bdir [ "cruise", "cruise.opt", "selftest.elf" ]
  "all" ~> need top
  "clean" ~> removeFilesAfter (bdir mempty) ["//*"]

  -- all main, top-level rule sets
  emulatorRules
  testRules
