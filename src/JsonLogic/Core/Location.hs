{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Location
--
-- Where in a rule something is: a path from the root of the rule's JSON to
-- one node, written as a JSON Pointer (RFC 6901) when rendered.
--
-- In @{"if": [true, {"*": []}]}@ the @*@ is at @/if/1@: the key @if@, then
-- index 1 of its argument list. An operator written with a bare argument
-- instead of a list (@{"!": x}@) has that argument at @/!@. The root is the
-- empty pointer, @""@.
--
-- A JSON Pointer names a place in the rule's own JSON text, so a location is
-- meaningful to a tool in any language. Locations are attached to every
-- parse and evaluation error.
module JsonLogic.Core.Location
  ( Location
  , Segment (..)
  , root
  , child
  , fromSegments
  , locationSegments
  , renderLocation
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | One step from a JSON value into a part of it.
data Segment
  = -- | An object member, by key.
    Key !Text
  | -- | An array element, by zero-based index.
    Index !Int
  deriving (Eq, Ord, Show)

instance NFData Segment where
  rnf (Key key) = rnf key
  rnf (Index i) = rnf i

-- | A path from the root of a rule. Held innermost-first, so that stepping
-- into a child while reading a rule is constant time.
newtype Location = Location [Segment]
  deriving (Eq, Ord)

instance NFData Location where
  rnf (Location segments) = rnf segments

-- | Shown as the 'fromSegments' call that rebuilds it.
instance Show Location where
  showsPrec d location =
    showParen (d > 10) (showString "fromSegments " . showsPrec 11 (locationSegments location))

-- | The root of the rule.
root :: Location
root = Location []

-- | One step further in.
child :: Location -> Segment -> Location
child (Location reversed) segment = Location (segment : reversed)

-- | A location from its segments, outermost first.
fromSegments :: [Segment] -> Location
fromSegments = Location . reverse

-- | The segments of a location, outermost first.
locationSegments :: Location -> [Segment]
locationSegments (Location reversed) = reverse reversed

-- | The location as a JSON Pointer (RFC 6901): @""@ for the root,
-- otherwise @/@ before each segment, with @~@ written @~0@ and @/@ written
-- @~1@ inside keys.
renderLocation :: Location -> Text
renderLocation = foldMap (("/" <>) . renderSegment) . locationSegments
  where
    renderSegment (Key key) = Text.replace "/" "~1" (Text.replace "~" "~0" key)
    renderSegment (Index i) = Text.pack (show i)
