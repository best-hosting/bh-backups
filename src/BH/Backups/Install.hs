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
        { -- | Prefix path used to construct all others (except 'sysconfdir').
        prefix        :: FilePath
        -- | Directory for installing binary files.
        , bindir        :: FilePath
        -- | Directory for installing config files.
        , sysconfdir    :: FilePath
        }
  deriving (Show)

-- | Default 'Options'.
defOptions :: Options
defOptions          =
    Options
        { prefix        = "t"
        , bindir        = prefix defOptions </> "bin"
        , sysconfdir    = prefix defOptions </> "etc" </> "rsnapshot.d"
        }

-- | 'OptDescr' for 'shakeArgsWith' (and 'getOpt').
opts :: [OptDescr (Either String (Options -> Options))]
opts                =
    [ Option [] ["prefix"]
        (ReqArg (\x -> Right (\op -> op{prefix = x})) "PATH")
        ("Prefix path used to construct all others (except 'sysconfdir')."
            ++ "Default: " ++ prefix defOptions)
    , Option [] ["bindir"]
        (ReqArg (\x -> Right (\op -> op{bindir = x})) "PATH")
        ("Directory for installing binary files."
            ++ "Default: " ++ bindir defOptions)
    , Option [] ["sysconfdir"]
        (ReqArg (\x -> Right (\op -> op{sysconfdir = x})) "PATH")
        ("Directory for installing config files."
            ++ "Default: " ++ sysconfdir defOptions)
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
            defTargets op args

            shellScript "sh" (bindir op) srcdir
            rsyncFilter "rsync-filter" (sysconfdir op) rsyncsrcdir

