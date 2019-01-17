{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Sail where

-- base
import           Data.Maybe    ( fromMaybe )
import           Control.Monad ( void )

-- shake
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Classes

-- local imports
import           CC   ( gcc )

--------------------------------------------------------------------------------
-- Types

-- | TODO FIXME
data SailBackend
  = SailBackendC
  | SailBackendOCaml
  deriving Show

--------------------------------------------------------------------------------
-- Executables

-- | Invoke the Sail compiler to generate C code for a sequential emulator.
sailc
  :: FilePath
  -- ^ Output path.
  -> [FilePath]
  -- ^ Input .sail files. Ordering matters.
  -> Action ()
sailc out srcs = cmd [ FileStdout out ] "sail -no_warn -O -c" srcs

sailcaml
  :: FilePath
  -> [FilePath]
  -> Action ()
sailcaml out srcs = cmd
  [ EchoStdout False
  , Cwd (takeDirectory1 out)
  ]
  "sail -no_warn -ocaml"
    [ "-o", dropDirectory1 out ]
    [ "-ocaml_build_dir", "ocaml" ]
    (map (".." </>) srcs)

--------------------------------------------------------------------------------
-- Sail utilities

-- | Generate some tedious code for using Sail 'mapping clause assembler'
-- directives. Hopefully this will be removed one day.
sailGenHexBits
  :: FilePath
  -> Action ()
sailGenHexBits out = do
  let src = "src/etc/gen_hexbits.py"
  need [ src ] >> cmd [ FileStdout out ] "python3" src

-- | Get the Sail library directory from the @$SAIL_DIR@ environment variable.
-- If this variable is not set, the build will fail.
sailDir :: Action FilePath
sailDir = getEnv "SAIL_DIR" >>= \case
  Nothing -> fail "SAIL_DIR unset; cannot locate RTS files, etc!"
  Just sd -> pure sd

-- | Get the set of source files for the C Runtime System from the sail
-- directory, by way of @'sailDir'@.
sailRts :: Action [FilePath]
sailRts = do
  sd <- sailDir
  pure . map (sd </>) =<< getDirectoryFiles sd [ "/lib/*.c" ]

-- | Generate a set of @'Rules'@ that will compile a set of Sail source code
-- files for a specification into a sequential simulator, by way of the C
-- backend
sailSimRules
  :: SailBackend
  -> FilePath
  -> [FilePath]
  -> Rules ()

-- C backend
sailSimRules SailBackendC bin srcs = do
  let csrc  = bin <.> "c"

  -- compile C -> binary, with Sail runtime
  bin %> \out -> do
    -- the C code must be combined with the RTS code
    allsrc <- fmap (csrc:) sailRts

    -- generated C code must have the right include path
    sdir <- sailDir
    let cflags = [ "-O2", "-I", sdir </> "lib" ]

    -- require & build
    need allsrc >> gcc out cflags allsrc [ "z", "gmp" ]

  -- compile Sail -> C code
  csrc %> \out -> need srcs >> sailc out srcs

-- TODO FIXME: OCaml backend
sailSimRules SailBackendOCaml bin srcs = do
  bin %> \out -> need srcs >> sailcaml out srcs
