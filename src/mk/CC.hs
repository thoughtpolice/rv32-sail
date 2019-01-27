{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-}

module CC where

-- base
import           Data.List     ( intercalate )

-- shake
import           Development.Shake

--------------------------------------------------------------------------------

data CcMode     = Link | Compile                        deriving Eq
data CcChoice   = Default | GCC | Clang                 deriving Eq
data CcPrefix   = NoPrefix | HostPrefix String          deriving Eq
data CcStd      = C99 | C11 | Gnu99 | Gnu11    deriving Eq
data CcOptLevel = Speed Int | Debug | Size              deriving Eq

data CcParams
  = CcParams
    { ccChoice   :: CcChoice
    , ccPrefix   :: CcPrefix
    , ccLang     :: CcStd
    , ccOpt      :: CcOptLevel
    , ccMode     :: CcMode
    , ccWarnings :: [String]
    , ccIncPaths :: [FilePath]
    , ccLibPaths :: [FilePath]
    , ccLibs     :: [String]
    , ccMarch    :: Maybe String
    , ccFreestanding :: Bool
    , ccLdFlags  :: [String]
    }

defaultCcParams :: CcParams
defaultCcParams = CcParams
  { ccChoice = Default, ccPrefix = NoPrefix, ccMarch = Nothing
  , ccFreestanding = False
  , ccOpt = Speed 1, ccLang = C11
  , ccWarnings = [], ccIncPaths = [], ccLibPaths = []
  , ccLibs = [], ccLdFlags = [], ccMode = Compile
  }

cc1
  :: CcParams
  -> [FilePath]
  -> FilePath
  -> Action ()
cc1 CcParams{..} sources out = do
  need sources
  opt <- optLevel
  cmd cc lang march freestand opt warnings incPaths libPaths
    mode [ "-o", out ] sources libs ldflags

  where
    cc | HostPrefix prefix <- ccPrefix = prefix <> ccName
       | otherwise = ccName

    -- empty strings cause '' to be output which causes gcc
    -- to get mad, so use an empty list instead
    mode | Link    <- ccMode = [ ]
         | Compile <- ccMode = [ "-c" ]

    ccName | Default <- ccChoice = "cc"
           | Clang   <- ccChoice = "clang"
           | GCC     <- ccChoice = "gcc"

    warnings = map ("-W" <>) ccWarnings
    incPaths = map ("-I" <>) ccIncPaths
    libPaths = map ("-L" <>) ccLibPaths
    libs     = map ("-l" <>) ccLibs
    march    = maybe [ ] (\x -> [ "-march=" <> x ]) ccMarch

    ldflags | null ccLdFlags = [  ]
            | otherwise      = [ "-Wl," <> intercalate "," ccLdFlags ]

    freestand | ccFreestanding = [ "-ffreestanding", "-nostdlib" ]
              | otherwise = [ ]

    lang = case ccLang of
      _ | ccMode == Link -> [ ]
      C99   -> [ "-std=c99" ]
      C11   -> [ "-std=c11" ]
      Gnu99 -> [ "-std=gnu99" ]
      Gnu11 -> [ "-std=gnu11" ]


    optLevel = case ccOpt of
      _ | ccMode == Link -> pure [ ]

      Speed o | o >= 0, o <= 4 -> pure [ "-O" <> show o ]
              | otherwise      -> optLevelFail o
      Debug   -> pure [ "-Og" ]
      Size    -> pure [ "-Os" ]
    optLevelFail o = fail $ "Invalid C compiler optimization level: '" <> show o <> "'"

cc' :: CcParams -> FilePath -> FilePath -> Action ()
cc' params src out = cc1 opts [ src ] out
  where opts = params
          { ccMode     = Compile
          , ccLibs     = []
          , ccLibPaths = []
          , ccLdFlags  = []
          }

ld' :: CcParams -> [FilePath] -> FilePath -> Action ()
ld' params srcs out = cc1 opts srcs out
  where opts = params
          { ccMode     = Link
          , ccWarnings = []
          , ccIncPaths = []
          }

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
