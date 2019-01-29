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
import           Control.Monad ( liftM2 )
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
    preamble = map (srcDir "preamble") [ "import", "ffi", "operators", "asm", "util" ]
            <> [ gen "hexbits" ]
    setup    = map src [ "basics", "config" ]
    decoder  = [ src "decode/prologue" ]
            <> map gen [ "decode/base" ]
            <> [ src "decode/epilogue" ]
    execute  = map (srcDir "execute") [ "prologue", "base", "ext-m", "epilogue" ]
    elfMain  = [ src "ecall", src (fromMaybe "elfmain" mainFile) ]

    -- source files which are hand-written (live under ./src)
    src      x = "src/spec" </> x <.> "sail"
    srcDir p x = "src/spec" </> p </> x <.> "sail"

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

rvcc :: String -> [(String, String)] -> FilePath -> Action ()
rvcc arch defns out = cc src out
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
      , ccDefines = defns
      , ccFreestanding = True
      }

rvld :: String -> FilePath -> [FilePath] -> Action ()
rvld arch out srcs = need [ lds ] >> bootObjs >>= \f -> ld (srcs ++ f) out
  where
    ( lds, ldm ) = ( "src/boot/sections.lds", out -<.> "map" )
    ld = ld' defaultCcParams
      { ccChoice = GCC, ccPrefix = HostPrefix "riscv32-unknown-elf-"
      , ccMarch  = Just arch
      , ccFreestanding = True, ccLibs = [ "gcc" ]
      , ccLdFlags = [ "-Bstatic", "-T", lds, "-Map", ldm, "--strip-debug" ]
      }

--------------------------------------------------------------------------------
-- Demos

ccObjsFromSources :: FilePath -> Action [FilePath]
ccObjsFromSources path = sourcesToObjects <$> getDirectoryFiles "" paths
  where paths = [ path </> "*.S", path </> "*.c" ]
        sourceToObject x = bdir (x <.> "o")
        sourcesToObjects = map sourceToObject

bootObjs :: Action [FilePath]
bootObjs = liftM2 (++)
  (ccObjsFromSources "src/libfirm")
  (ccObjsFromSources "src/boot")

bootRules :: Rules ()
bootRules = do
  bdir "src/libfirm/*.o" %> rvcc "rv32i" []
  bdir "src/boot/*.o"    %> rvcc "rv32i" []

demoRules :: Rules ()
demoRules = do
  let dirToElf x = bdir "demos" </> x <.> "elf"
      getDemos   = getDirectoryDirs "src/demos"

  bdir "src/demos//*.o" %> rvcc "rv32i" []
  bdir "demos/*.elf" %> \out -> do
    let dir = "src" </> dropDirectory1 (dropExtension out)
    ccObjsFromSources dir >>= rvld "rv32i" out

  -- phony top-level rule
  "demos" ~> do need =<< (map dirToElf <$> getDemos)

--------------------------------------------------------------------------------
-- Test rules

testRules :: Rules ()
testRules = do
  let dirToElf x = bdir "t" </> x <.> "elf"

  bdir "src/t/firmware/*.o" %> rvcc "rv32i" []
  bdir "src/t/tests/*.o" %> \out -> do
    let tname = takeFileName (dropExtensions out)
        defns = [ ("TEST_FUNC_NAME", tname)
                , ("TEST_FUNC_TXT", show tname)
                , ("TEST_FUNC_RET", tname <> "_ret")
                ]
    rvcc "rv32im" defns out

  bdir "t/smoke.elf" %> \out -> do
    cObjs <- ccObjsFromSources "src/t/firmware"
    tObjs <- ccObjsFromSources "src/t/tests"
    rvld "rv32i" out (cObjs ++ tObjs)

  "tests" ~> do need =<< (map dirToElf <$> pure [ "smoke" ])

--------------------------------------------------------------------------------
-- Driver

-- | Main entry point
main :: IO ()
main = shakeArgsWith myShakeOptions myFlags $ \_ targets -> pure $ Just $ do
  -- establish the top-level 'all' rule as the default
  want (if not (null targets) then targets else [ "all" ])

  -- major rule sets
  emulatorRules
  bootRules
  demoRules
  testRules

  -- phonies
  "clean" ~> removeFilesAfter (bdir mempty) ["//*"]
  "all" ~> need [ bdir "cruise", bdir "cruise.opt", "demos", "tests" ]
