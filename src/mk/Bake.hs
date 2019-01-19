{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Bake
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
import           CC
import           Decoder ( decoderFrontend )
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
specSources mainFile
  = pure $ preamble <> setup <> decoder <> execute <> elfMain
  where
    preamble = [ src "preamble", gen "hexbits" ]
    setup    = map src [ "basics", "config" ]
    decoder  = [ src "decode/prologue" ]
            <> map gen [ "decode/base" ]
            <> [ src "decode/epilogue" ]
    execute  = [ src "execute/prologue" ]
            <> [ src "execute/epilogue" ]
    elfMain  = [ src (fromMaybe "elfmain" mainFile) ]

    -- source files which are hand-written (live under ./src)
    src x = "src/spec" </> x <.> "sail"
    -- source files which are generated (live under ./build)
    gen x = bdir ("src/spec" </> x) <.> "sail"

-- | Top-level set of all rules for building the emulator.
emulatorRules :: Rules ()
emulatorRules = do
  -- main emulator: 'cruise' C backend, with default ELF loader
  sources <- specSources Nothing
  sailSimRules SailBackendC     (bdir "cruise.opt") sources
  sailSimRules SailBackendOCaml (bdir "cruise")     sources

  -- generate hexbits.sail
  bdir "src/spec/hexbits.sail" %> sailGenHexBits

  -- generate decoder
  bdir "src/spec/decode/base.sail" %> \out -> do
    need [ "src/mk/Decoder.hs" ]
    putNormal ("# writeFile' (for " <> out <> ")")
    writeFile' out decoderFrontend

--------------------------------------------------------------------------------

rvcc :: String -> FilePath -> Action ()
rvcc arch out = cc src out
  where
    src = dropDirectory1 (dropExtension out)
    cc  = cc' defaultCcParams
      { ccChoice = GCC, ccPrefix = HostPrefix "riscv32-unknown-elf-"
      , ccMarch  = Just arch, ccLang = C11, ccOpt = Size
      , ccWarnings = [ "error", "all", "extra", "shadow", "undef"
                     , "pointer-arith", "cast-qual", "cast-align"
                     , "write-strings", "redundant-decls", "strict-prototypes"
                     , "missing-prototypes"
                     ]
      , ccFreestanding = True
      }

rvld :: String -> FilePath -> [FilePath] -> Action ()
rvld arch out srcs = need [ lds ] >> libfirmObjs >>= \f -> ld (srcs ++ f) out
  where
    ( lds, ldm ) = ( "src/libfirm/sections.lds", out -<.> "map" )
    ld = ld' defaultCcParams
      { ccChoice = GCC, ccPrefix = HostPrefix "riscv32-unknown-elf-"
      , ccMarch  = Just arch
      , ccFreestanding = True
      , ccLdFlags = [ "-Bstatic", "-T", lds, "-Map", ldm, "--strip-debug" ]
      }

--------------------------------------------------------------------------------
-- Demos

ccObjsFromSources :: FilePath -> Action [FilePath]
ccObjsFromSources path = sourcesToObjects <$> getDirectoryFiles "" paths
  where paths = [ path </> "*.S", path </> "*.c" ]
        sourceToObject x = bdir (x <.> "o")
        sourcesToObjects = map sourceToObject

libfirmObjs :: Action [FilePath]
libfirmObjs = ccObjsFromSources "src/libfirm"

demoRules :: Rules ()
demoRules = do
  let dirToElf x = bdir "demos" </> x <.> "elf"
      getDemos   = getDirectoryDirs "src/demos"

  bdir "src/libfirm/*.o" %> rvcc "rv32i"
  bdir "src/demos//*.o"  %> rvcc "rv32i"

  bdir "demos/*.elf" %> \out -> do
    let dir = "src" </> dropDirectory1 (dropExtension out)
    ccObjsFromSources dir >>= rvld "rv32i" out

  -- phony top-level rule
  "demos" ~> do need =<< (map dirToElf <$> getDemos)

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

_testRules :: Rules ()
_testRules = do
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
  let ld out objs = do
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
  let cc out name = do
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

    unit $ ld out (fw_objs <> asm_objs)
    cmd "chmod -x" out

  -- Test rules
  bdir "t/tests/*.asm.o" %> \out -> tasm out (takeBaseName1 out)
  bdir "t/firmware/*.asm.o" %> \out -> rvasm out (takeBaseName1 out)
  bdir "t/firmware/*.c.o"   %> \out -> cc  out (takeBaseName1 out)

--------------------------------------------------------------------------------
-- Driver

-- | Main entry point
main :: IO ()
main = shakeArgsWith myShakeOptions myFlags $ \_ targets -> pure $ Just $ do
  -- establish the top-level 'all' rule as the default
  want (if not (null targets) then targets else [ "all" ])

  -- major rule sets
  emulatorRules
  demoRules
--_testRules

  -- phonies
  "clean" ~> removeFilesAfter (bdir mempty) ["//*"]
  "all" ~> need [ bdir "cruise", bdir "cruise.opt", "demos" ]
