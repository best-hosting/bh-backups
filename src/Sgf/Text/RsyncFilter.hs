{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS_HADDOCK show-extensions     #-}

-- |
-- Module:      Sgf.Text.RsyncFilter
-- Description: Parse rsync filter (now only include lines are recognized).
-- Maintainer:  sgf.dma@gmail.com

module Sgf.Text.RsyncFilter
    ( -- * Types.
      -- $types
      IncludeT
    , LineT
    , RsyncFilter
    , includeL
    , lineL
    , AnyFilter (..)
    , getFilter
    , rsyncAnyL
    , rsyncIncludeL
    , rsyncIncludeL'
    , rsyncLineL
    , rsyncLineL'

      -- * Parse.
      -- $parse
    , Serialize (..)
    )
  where

import Prelude hiding (getLine)
import Data.Maybe
import Data.Typeable
import Control.Applicative

import Sgf.Control.Lens


-- $types

-- Data constructors for being promoted to type-level.
data RsyncFilterT   = IncludeT | LineT

type IncludeT       = 'IncludeT -- ^ Type of rsync include statement.
type LineT          = 'LineT    -- ^ Type of any other rsync statements.

-- | Rsync filter statements (lines) distinguishable at type-level.
data RsyncFilter   :: RsyncFilterT -> * where
    Include        :: {getInclude :: FilePath}  -> RsyncFilter 'IncludeT
    Line           :: {getLine :: String}       -> RsyncFilter 'LineT
deriving instance Show (RsyncFilter a)
deriving instance Typeable RsyncFilter

-- | Lens from 'RsyncFilter' to include statement (note, the type: i may only
-- apply this to rsync filter containing include statement).
includeL :: LensA (RsyncFilter 'IncludeT) FilePath
includeL f z@Include {getInclude = x}   = fmap (\x' -> z{getInclude = x'}) (f x)
-- | Lens from 'RsyncFilter' to any other statement (note, the type: i may
-- only apply this to rsync filter containing any other statements).
lineL :: LensA (RsyncFilter 'LineT) FilePath
lineL f z@Line {getLine = x}            = fmap (\x' -> z{getLine = x'}) (f x)

-- | Generic container for any type of rsync filter file statements.
data AnyFilter      = forall (a :: RsyncFilterT). Typeable a =>
                        AnyFilter (RsyncFilter a)
deriving instance Show AnyFilter
deriving instance Typeable AnyFilter

-- | Extract some rsync statement from 'AnyFilter'. The required type
-- determines what statement i'll try to extract.
getFilter :: (forall (a :: RsyncFilterT). Typeable a =>
             RsyncFilter a -> b) -> AnyFilter -> b
getFilter f (AnyFilter x)  = f x

-- | Lens from 'AnyFilter' to 'Serialize'-d rsync filter - 'String'. This is
-- effectively parse lens.
rsyncAnyL :: LensA String AnyFilter
rsyncAnyL f z       = maybe (pure z) (fmap toString . f) (fromString z)

-- | Lenses from 'AnyFilter' to rsync include statement.
rsyncIncludeL' :: LensA AnyFilter (RsyncFilter 'IncludeT)
rsyncIncludeL' f z  = maybe (pure z) (fmap AnyFilter . f) (getFilter cast z)
rsyncIncludeL :: LensA AnyFilter FilePath
rsyncIncludeL       = rsyncIncludeL' . includeL

-- | Lenses from 'AnyFilter' to any other statement of rsync filter file.
rsyncLineL' :: LensA AnyFilter (RsyncFilter 'LineT)
rsyncLineL' f z     = maybe (pure z) (fmap AnyFilter . f) (getFilter cast z)
rsyncLineL :: LensA AnyFilter String
rsyncLineL          = rsyncLineL' . lineL


-- $parse

-- | Essentially this is just another 'Read' / 'Show' class, because i want
-- to keep default 'Read' / 'Show' too.
class Serialize a where
    fromString  :: String -> Maybe a
    toString    :: a -> String

instance Serialize (RsyncFilter 'LineT) where
    fromString          = Just . Line
    toString (Line xs)  = xs

instance Serialize (RsyncFilter 'IncludeT) where
    fromString              = go . break (flip any [' ', '_'] . (==))
      where
        go :: (String, String) -> Maybe (RsyncFilter 'IncludeT)
        go (x, _ : z : zs)
          | isInclude (break (== ',') x)
                            = Just (Include (z : zs))
          where
            isInclude ::  (String, String) -> Bool
            -- Short rule name with modifier. Modifier is not checked, but it
            -- should be only one character.
            isInclude (['.', _], [])    = True
            -- Short or long rule name with explicit modifier separator
            -- (comma). Modifier is not checked, but it should be only one
            -- character (because 'break' leaves element matching predicate in
            -- second value, 'length' of second value must be < 3).
            isInclude (       r, ys)
              | length (take 3 ys) < 3  = r `elem` [".", "merge"]
              | otherwise               = False
        go _                = Nothing
    toString (Include xs)   = ". " ++ xs

instance Serialize AnyFilter where
    fromString x    =
                fmap AnyFilter (fromString x :: Maybe (RsyncFilter 'IncludeT))
            <|> fmap AnyFilter (fromString x :: Maybe (RsyncFilter 'LineT))
    toString x      = fromMaybe "" $
                fmap toString (viewAmaybe rsyncIncludeL' x)
            <|> fmap toString (viewAmaybe rsyncLineL' x)

