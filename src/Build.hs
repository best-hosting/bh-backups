{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

import System.Console.GetOpt

import Development.Shake
import Development.Shake.FilePath
import System.Directory.Extra

import Sgf.Development.Shake.RsyncFilter
import Sgf.Development.Shake.Shell
import Sgf.Development.Shake.Target


-- $constants

-- | Source directory.
srcdir :: FilePath
srcdir              = "src"

-- | Rsync filter source directory.
rsyncsrcdir :: FilePath
rsyncsrcdir         = "rsnapshot.d"


-- $options
data Options        = Options
                        { prefix        :: FilePath
                        , bindir        :: FilePath
                        , sysconfdir    :: FilePath
                        }
  deriving (Show)
defOptions :: Options
defOptions          =
    Options
        { prefix        = "t"
        , bindir        = prefix defOptions </> "bin"
        , sysconfdir    = prefix defOptions </> "etc" </> "rsnapshot.d"
        }

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


-- $targets.

-- | Several special targets.
data SpecialTarget  = AllT      -- ^ Install all.
                    | RsyncT    -- ^ Install only rsync filters.
                    | MysqlT    -- ^ Install only mysql backup script.
-- | Now 'All' target install rsync filters ('Rsync') and mysql backup script
-- ('Mysql').
instance BuildTarget 'AllT where
    data Target 'AllT       = All Options
    wantTarget (All op)     = do
        wantTarget (Rsync op)
        wantTarget (Mysql op)
-- | Treat all files in 'rsyncsrcdir' (excluding subdirectories) as rsync
-- filters to be installed.
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


-- $main

main :: IO ()
main    = shakeArgsWith shakeOptions opts $ \fopts args -> return $ Just $ do
            let xopts = foldr (.) id fopts defOptions
            defTargets xopts args

            shellScript "sh" (bindir xopts) srcdir
            rsyncFilter "rsync-filter" (sysconfdir xopts) rsyncsrcdir

