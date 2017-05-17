{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

module BH.Backups.Install
  where

import System.Console.GetOpt

import Development.Shake
import Development.Shake.FilePath
import System.Directory.Extra

import Sgf.Development.Shake.RsyncFilter
import Sgf.Development.Shake.Shell
import Sgf.Development.Shake.Target


-- * Constants.
--
-- $constants

-- | Source directory.
srcdir :: FilePath
srcdir              = "src"

-- | Rsync filter source directory.
rsyncsrcdir :: FilePath
rsyncsrcdir         = "rsnapshot.d"


-- * Options.
--
-- $options

data Options =
    Options
        { -- | Prefix path used to construct all others pathes.
          _prefix        :: FilePath
        -- | Prefix path used to construct '_bindir'. If it's empty, '_prefix'
        -- will be used.
        , _execPrefix :: FilePath
        -- | Directory for installing binary files.
        , _bindir        :: FilePath
        -- | Directory for installing config files.
        , _sysconfdir    :: FilePath
        }
  deriving (Show)

-- | Default 'Options'.
defOptions :: Options
defOptions          =
    Options
        { _prefix       = "t"
        , _execPrefix   = ""
        , _bindir       = "bin"
        , _sysconfdir   = "etc" </> "rsnapshot.d"
        }

-- | Construct full config installation path by applying '_sysconfdir' to
-- '_prefix'.
sysconfdir :: Options -> FilePath
sysconfdir Options {_prefix = p, _sysconfdir = s}
                    = p </> s

-- | Construct full binary installation path by applying '_bindir' to
-- '_execPrefix', if not empty, or '_prefix' otherwise.
bindir :: Options -> FilePath
bindir Options {_prefix = p, _execPrefix = ep, _bindir = b}
  | null ep         = p  </> b
  | otherwise       = ep </> b

-- | 'OptDescr' for 'shakeArgsWith' (and 'getOpt').
opts :: [OptDescr (Either String (Options -> Options))]
opts                =
    [ Option [] ["prefix"]
        (ReqArg (\x -> Right (\op -> op{_prefix = x})) "PATH")
        ("Prefix path used to construct all other pathes."
            ++ " Default: " ++ _prefix defOptions)
    , Option [] ["exec-prefix"]
        (ReqArg (\x -> Right (\op -> op{_execPrefix = x})) "PATH")
        ("Prefix path used to construct 'bindir` path."
            ++ " If not specified, 'prefix' will be used."
            ++ " Default: " ++ _execPrefix defOptions)
    , Option [] ["bindir"]
        (ReqArg (\x -> Right (\op -> op{_bindir = x})) "PATH")
        ("Trailing part of binary files installation path."
            ++ " '--prefix' or '--exec-prefix' are prepended to this path."
            ++ " Default: " ++ bindir defOptions)
    , Option [] ["sysconfdir"]
        (ReqArg (\x -> Right (\op -> op{_sysconfdir = x})) "PATH")
        ("Trailing part of config files installation path."
            ++ " '--prefix' is prepended to this path."
            ++ " Default: " ++ sysconfdir defOptions)
    ]


-- * Targets.
--
-- $targets.
--
-- Implementation of strictly-typed targets. /Not used/ now in favour of
-- shake's 'phony' targets.

-- | Constructors to be promoted to type-level for special install targets.
data SpecialTarget  = AllT      -- ^ Install all.
                    | RsyncT    -- ^ Install only rsync filters.
                    | MysqlT    -- ^ Install only mysql backup.
type AllT           = 'AllT
type RsyncT         = 'RsyncT
type MysqlT         = 'MysqlT

-- | Install rsync filters ('Rsync') and mysql backup script ('Mysql').
instance BuildTarget 'AllT where
    data Target 'AllT       = All Options
    wantTarget (All op)     = do
        wantTarget (Rsync op)
        wantTarget (Mysql op)
-- | Install all top-level files in 'rsyncsrcdir' (excluding subdirectories).
instance BuildTarget 'RsyncT where
    data Target 'RsyncT = Rsync Options
    wantTarget (Rsync op)   = do
        xs <- liftIO $ listFiles rsyncsrcdir
        let ys  = map (sysconfdir op </>)
                    . filter ((== ".rsync-filter") . takeExtension)
                    . map takeFileName
                    $ xs
        wantTarget (Files ys)
-- | Install mysql backup script.
instance BuildTarget 'MysqlT where
    data Target 'MysqlT     = Mysql Options
    wantTarget (Mysql op)   = wantTarget $
                                Files [bindir op </> "mysql" <.> "sh"]

-- | Parse remaining command-line arguments as build targets.
defTargets :: Options -> [String] -> Rules ()
defTargets op []    = wantTarget (All op)
defTargets op [x]
  | x == "all"      = wantTarget (All op)
  | x == "rsync"    = wantTarget (Rsync op)
  | x == "mysql"    = wantTarget (Mysql op)
  | otherwise       = wantTarget (Files [x])
defTargets _  xs    = wantTarget (Files xs)


-- * Main.
--
-- $main

-- | Main install rules.
install :: IO ()
install  = shakeArgsWith shakeOptions opts $ \fopts args -> return $ Just $ do
            let op = foldr (.) id fopts defOptions
            --defTargets op args
            if null args
              then want ["all"]
              else want args

            "all"   ~> need ["rsync", "mysql"]

            "rsync" ~> do
                xs <- liftIO $ listFiles rsyncsrcdir
                let ys  = map (sysconfdir op </>)
                            . filter ((== ".rsync-filter") . takeExtension)
                            . map takeFileName
                            $ xs
                need ys

            "mysql" ~> need [bindir op </> "mysql" <.> "sh"]

            shellScript "sh" (bindir op) srcdir
            rsyncFilter "rsync-filter" (sysconfdir op) rsyncsrcdir

