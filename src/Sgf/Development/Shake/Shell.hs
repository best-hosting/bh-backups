
-- |
-- Module:      Sgf.Development.Shake.Shell
-- Description: Install rsync filters.
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Development.Shake.Shell
    ( shellScript
    )
  where

import Development.Shake
import Development.Shake.FilePath

import Sgf.Development.Shake.FilePath


-- | Add file rule for installing shell script. Just copy file.
shellScript :: String       -- ^ Extension.
               -> FilePath  -- ^ Install path.
               -> FilePath  -- ^ Source path.
               -> Rules ()
shellScript ext prefix srcdir   = prefix ++ "//*" <.> ext %> \out -> do
              let src = replacePrefix prefix srcdir out
              need [src]
              unit $ cmd "cp -v" src out

