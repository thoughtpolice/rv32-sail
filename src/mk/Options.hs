{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Options where

-- base
import           Data.Maybe            ( fromMaybe )
import           Control.Monad         ( void )
import           System.Console.GetOpt ( OptDescr(..) )

-- shake
import           Development.Shake
import           Development.Shake.FilePath
import           Development.Shake.Classes

--------------------------------------------------------------------------------
-- Types

data Flags = Unit () deriving Eq

--------------------------------------------------------------------------------
-- Command line Flags

myFlags :: [ OptDescr (Either String Flags) ]
myFlags = []

--------------------------------------------------------------------------------
-- Shake Options

-- | Shake build options.
myShakeOptions :: ShakeOptions
myShakeOptions = shakeOptions
  { shakeFiles    = ".shake"
  , shakeProgress = progress
  , shakeVersion  = "1"
  , shakeTimings  = False
  , shakeColor    = True
  }
  where
    -- | A progress bar that polls once every second, as opposed to once every
    -- five seconds.
    progress p = do
      program <- progressProgram
      progressDisplay 1 (\s -> progressTitlebar s >> program s) p
