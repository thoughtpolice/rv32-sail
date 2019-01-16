{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module CC where

-- base
import           Data.Maybe    ( fromMaybe )
import           Control.Monad ( void )

-- shake
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Classes

--------------------------------------------------------------------------------
-- Host GCC Wrapper

-- | Invoke the host-native GCC compiler. Mostly used for compiling the
-- sequential emulator.
gcc
  :: FilePath
  -> [String]
  -> [FilePath]
  -> [String]
  -> Action ()
gcc out cflags srcs ldlibs = cmd "gcc" output cflags srcs ldflags where
  ldflags = map ("-l" ++) ldlibs
  output  = [ "-o", out ]
