
-- |
-- Module:      Sgf.Development.Shake.RsyncFilter
-- Description: Install rsync filters.
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Development.Shake.RsyncFilter
    ( usedIncludes
    , rsyncFilter
    )
  where

import Control.Monad.Writer
import Development.Shake
import Development.Shake.FilePath

import Sgf.Control.Lens
import qualified Sgf.Text.RsyncFilter as RF
import Sgf.Development.Shake.FilePath


-- | Rewrite path in rsync filter's include @line@ from source path @srcdir@
-- to install path @prefix@
--
-- >    usedIncludes srcdir prefix line
--
-- and save (rewritten) rsync include path in 'Writer' monad. Other lines
-- return as is.
usedIncludes :: FilePath        -- ^ Source path.
                -> FilePath     -- ^ Install path.
                -> String       -- ^ Line from rsync filter file.
                -> Writer [FilePath] String
usedIncludes srcdir prefix  =
    modifyAA (RF.rsyncAnyL . RF.rsyncIncludeL) $ \x -> do
      let x' = replacePrefix srcdir prefix x
      tell [x']
      return x'

-- | Add file rule for instaling rsync filters with extension @ext@, rewriting
-- source path @srcdir@ to install path @prefix@ in any rsync includes:
--
-- >    rsyncFilter ext srcdir prefix
--
rsyncFilter :: String       -- ^ Extension.
               -> FilePath  -- ^ Install path.
               -> FilePath  -- ^ Source path.
               -> Rules ()
rsyncFilter ext prefix srcdir   = prefix ++ "//*" <.> ext %> \out -> do
    let src  = replacePrefix prefix srcdir out
    ls <- readFileLines src
    let (rs, incs) = runWriter $ mapM (usedIncludes srcdir prefix) ls
    need incs
    putNormal $ "> Write " ++ out
    writeFileChanged out . unlines $ rs

