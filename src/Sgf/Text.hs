
-- |
-- Module:      Sgf.Text
-- Description: Helper functions for 'String' parsing.
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Text
    (
      Serialize (..)
    )
  where

-- | Essentially this is just another 'Read' / 'Show' class, in case i'll want
-- to keep default 'Read' / 'Show' too.
class Serialize a where
    fromString  :: String -> Maybe a
    toString    :: a -> String

