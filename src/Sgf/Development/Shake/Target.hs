{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

-- |
-- Module:      Sgf.Development.Shake.Target
-- Description: Strictly typed targets.
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Development.Shake.Target
    ( BuildTarget (..)
    , Target (..)
    )
  where

import Development.Shake


-- | Kind polymorphic class with associated data family for defining build
-- targets. I need a kind polymorphism for making instances for promoted data
-- types (they'll have not a '*' kind).
--
-- Build target has the type 'Target a' (different targets have different
-- types) and its data constructor is defined in class instance. Thus,
-- type-checker will verify, that all used targets have instances defined.
-- E.g. if "build all" target refers to several specific targets and some of
-- them will be removed or renamed, type-checker will notice this.
class BuildTarget (a :: t) where
    data Target a   :: *
    wantTarget      :: Target a -> Rules ()

instance BuildTarget FilePath where
    data Target FilePath    = Files [FilePath]
    wantTarget (Files xs)   = want xs

