
import System.Console.GetOpt

import Development.Shake
import Development.Shake.FilePath
import System.Directory.Extra

import Sgf.Development.Shake.RsyncFilter
import Sgf.Development.Shake.Shell

-- $constants

-- | Source directory.
srcdir :: FilePath
srcdir              = "src"

-- | Rsync filter source directory.
rsyncrscdir :: FilePath
rsyncrscdir         = "rsnapshot.d"

-- $options
data Options        = Options
                        { prefix        :: FilePath
                        , bindir        :: FilePath
                        , sysconfdir    :: FilePath
                        }
  deriving (Show)
defOptions :: Options
defOptions          = Options
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

-- $main

defTargets :: Options -> [String] -> Rules ()
defTargets xopts [] = defTargets xopts ["all"]
defTargets xopts [x]
  -- Assume existence of other targets. Not checked!
  | x == "all"      = mapM_ (defTargets xopts . (: [])) ["mysql", "rsync"]
  | x == "mysql"    = want [bindir xopts </> "mysql" <.> "sh"]
  | x == "rsync"    = do
        xs <- liftIO $ listFiles rsyncrscdir
        let ys  = map (sysconfdir xopts </>)
                    . filter ((== ".rsync-filter") . takeExtension)
                    . map takeFileName
                    $ xs
        want ys
  | otherwise       = want [x]
defTargets _ xs     = want xs

main :: IO ()
main    = shakeArgsWith shakeOptions opts $ \fopts args -> return $ Just $ do
            let xopts = foldr (.) id fopts defOptions
            defTargets xopts args

            shellScript "sh" (bindir xopts) srcdir
            rsyncFilter "rsync-filter" (sysconfdir xopts) rsyncrscdir

