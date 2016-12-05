
-- |
-- Module:      Sgf.Development.Shake.FilePath
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Development.Shake.FilePath
    ( replacePrefix
    )
  where

import Data.List
import System.FilePath


-- | Replace path prefix @old@ (starting and ending at path component
-- boundaries) with @new@, if matched:
--
-- >    replacePrefix old new path
replacePrefix ::   FilePath     -- ^ @Old@ path prefix to replace with.
                -> FilePath     -- ^ @New@ path prefix to substitute to.
                -> FilePath     -- ^ Path.
                -> FilePath     -- ^ Resulting path.
replacePrefix old new x  = maybe x (combine new . joinPath) $
    -- For ensuring that path prefix starts and ends at path components
    -- (directories) boundaries, i first split them.
    stripPrefix (splitDirectories old) (splitDirectories x)

