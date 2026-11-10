{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : JsonLogic.Core.Utf16
--
-- JavaScript strings are sequences of UTF-16 code units, not of Unicode
-- code points. The reference implementation's @length@, indexing, @substr@
-- and @<@ all work on code units, so a character outside the Basic
-- Multilingual Plane (an emoji, say) has length 2, and sorts /below/
-- U+E000 to U+FFFF because its first code unit is a surrogate in
-- U+D800 to U+DBFF.
--
-- This module is the one place those semantics are defined, over
-- 'Text'. Nothing here builds a list of code units: skipping and slicing
-- walk the text once, without allocating per code unit, so the cost of an
-- operation is its scan of the string and nothing more.
--
-- A 'Text' cannot hold a lone surrogate, so a slice that cuts through a
-- surrogate pair cannot be a 'Text'. 'utf16Slice' and 'utf16CharAt' answer
-- such a slice with U+FFFD for the severed half (what
-- @String.prototype.toWellFormed()@ makes of the reference's answer). The
-- evaluator does not use them for that: it uses 'utf16SliceExact', which
-- answers a 'Units' value when the slice is not well formed, so that the
-- halves keep their identity: they compare unequal, and joining them again
-- gives the original character, as in JavaScript.
module JsonLogic.Core.Utf16
  ( utf16Length
  , utf16Slice
  , utf16CharAt
  , compareUtf16

    -- * Strings that 'Text' cannot hold
  , Units
  , toUnits
  , fromUnits
  , unitsText
  , unitsLength
  , unitsSlice
  , unitsAppend
  , unitsIsInfixOf
  , utf16SliceExact
  , utf16CharAtExact
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Unboxed
import Data.Word (Word16)

-- | The number of UTF-16 code units: JavaScript's @s.length@.
utf16Length :: Text -> Int
utf16Length = Text.foldl' (\n c -> if ord c > 0xFFFF then n + 2 else n + 1) 0

charUnits :: Char -> [Word16]
charUnits c
  | n <= 0xFFFF = [fromIntegral n]
  | otherwise =
      let m = n - 0x10000
       in [ fromIntegral (0xD800 .|. (m `shiftR` 10))
          , fromIntegral (0xDC00 .|. (m .&. 0x3FF))
          ]
  where
    n = ord c

-- | @slice from count@: the @count@ code units starting at code unit
-- @from@, both already clamped into range by the caller. Cutting through a
-- surrogate pair gives U+FFFD for the severed half.
utf16Slice :: Int -> Int -> Text -> Text
utf16Slice from count text
  | count <= 0 = Text.empty
  | Text.all isBmp text = Text.take count (Text.drop from text)
  | otherwise =
      let (rest, severedStart) = dropUnits from text
          -- a severed low half is one code unit of the slice, written U+FFFD
          budget = if severedStart then count - 1 else count
          (whole, severedEnd) = fitUnits budget rest
       in (if severedStart then replacement else Text.empty)
            <> Text.take whole rest
            <> (if severedEnd then replacement else Text.empty)

-- | The code unit at an index, as a one-unit string, or 'Nothing' past the
-- end. A lone surrogate half is returned as U+FFFD.
utf16CharAt :: Int -> Text -> Maybe Text
utf16CharAt i text
  | i < 0 = Nothing
  | Text.all isBmp text =
      let picked = Text.take 1 (Text.drop i text)
       in if Text.null picked then Nothing else Just picked
  | otherwise =
      let (rest, severed) = dropUnits i text
       in if severed
            then Just replacement
            else case Text.uncons rest of
              Nothing -> Nothing
              Just (c, _)
                | isBmp c -> Just (Text.singleton c)
                | otherwise -> Just replacement

-- | U+FFFD, what a severed half of a surrogate pair becomes.
replacement :: Text
replacement = Text.singleton '\xFFFD'

-- | Code units a character takes.
width :: Char -> Int
width c = if isBmp c then 1 else 2

-- | Skip @n@ code units. The flag says the skip ended inside a surrogate
-- pair: the rest starts after that character, and its low half is owed.
dropUnits :: Int -> Text -> (Text, Bool)
dropUnits n text
  | n <= 0 = (text, False)
  | otherwise = case Text.uncons text of
      Nothing -> (text, False)
      Just (c, rest)
        | width c <= n -> dropUnits (n - width c) rest
        | otherwise -> (rest, True)

-- | How many whole characters fit in @n@ code units, and whether the next
-- character would be cut after its high half.
fitUnits :: Int -> Text -> (Int, Bool)
fitUnits = go 0
  where
    go !whole n text
      | n <= 0 = (whole, False)
      | otherwise = case Text.uncons text of
          Nothing -> (whole, False)
          Just (c, rest)
            | width c <= n -> go (whole + 1) (n - width c) rest
            | otherwise -> (whole, True)

isBmp :: Char -> Bool
isBmp c = ord c <= 0xFFFF

-- | JavaScript's string ordering: lexicographic by UTF-16 code unit.
--
-- It differs from ordering by code point only when an astral character
-- (first unit U+D800 to U+DBFF) meets a BMP character in U+E000 to U+FFFF:
-- by code unit the astral one is smaller. The common prefix is skipped
-- without allocating, so only the first pair of characters that differ is
-- ever looked at.
compareUtf16 :: Text -> Text -> Ordering
compareUtf16 a b = case Text.commonPrefixes a b of
  Nothing -> firstDifference a b
  Just (_, restA, restB) -> firstDifference restA restB
  where
    firstDifference x y = case (Text.uncons x, Text.uncons y) of
      (Nothing, Nothing) -> EQ
      (Nothing, Just _) -> LT
      (Just _, Nothing) -> GT
      (Just (p, _), Just (q, _))
        | p == q -> EQ
        | otherwise -> compare (charUnits p) (charUnits q)

-- ---------------------------------------------------------------------
-- Strings with lone surrogates
-- ---------------------------------------------------------------------

-- | A JavaScript string as its UTF-16 code units. Used only for the rare
-- string that is not well formed (it holds a lone surrogate), which a
-- 'Text' cannot; every other string stays a 'Text'.
newtype Units = Units (Unboxed.Vector Word16)
  deriving (Eq, Ord, Show)

isHigh, isLow :: Word16 -> Bool
isHigh u = u >= 0xD800 && u <= 0xDBFF
isLow u = u >= 0xDC00 && u <= 0xDFFF

-- | The code units of a 'Text'.
toUnits :: Text -> Units
toUnits = Units . Unboxed.fromList . concatMap charUnits . Text.unpack

-- | The characters of a unit sequence, with each lone surrogate written
-- U+FFFD, and whether there were none.
decode :: [Word16] -> ([Char], Bool)
decode (high : low : rest)
  | isHigh high && isLow low =
      let (chars, ok) = decode rest
          cp = 0x10000 + ((fromIntegral high - 0xD800) `shiftL` 10) + (fromIntegral low - 0xDC00)
       in (toEnum cp : chars, ok)
decode (unit : rest)
  | isHigh unit || isLow unit = let (chars, _) = decode rest in ('\xFFFD' : chars, False)
  | otherwise = let (chars, ok) = decode rest in (toEnum (fromIntegral unit) : chars, ok)
decode [] = ([], True)

-- | A 'Text' if the units are well formed, otherwise the units themselves.
fromUnits :: Units -> Either Units Text
fromUnits units@(Units v) = case decode (Unboxed.toList v) of
  (chars, True) -> Right (Text.pack chars)
  (_, False) -> Left units

-- | The units as text, each lone surrogate written U+FFFD. This is the form
-- used where a string is read as a number or a path, and in the JSON a rule
-- returns, where a lone surrogate cannot appear.
unitsText :: Units -> Text
unitsText (Units v) = Text.pack (fst (decode (Unboxed.toList v)))

-- | JavaScript's @length@.
unitsLength :: Units -> Int
unitsLength (Units v) = Unboxed.length v

-- | @unitsSlice from count@, both already clamped into range.
unitsSlice :: Int -> Int -> Units -> Units
unitsSlice from count (Units v) = Units (Unboxed.slice from count v)

-- | Concatenation: halves of a pair that meet become the pair again.
unitsAppend :: [Units] -> Units
unitsAppend pieces = Units (Unboxed.concat [v | Units v <- pieces])

-- | @haystack.indexOf(needle) !== -1@, in linear time (Knuth-Morris-Pratt),
-- so a hostile needle cannot make a search quadratic.
unitsIsInfixOf :: Units -> Units -> Bool
unitsIsInfixOf (Units needle) (Units haystack)
  | m == 0 = True
  | otherwise = go 0 0
  where
    m = Unboxed.length needle
    n = Unboxed.length haystack
    failure :: Unboxed.Vector Int
    failure = Unboxed.constructN m step
      where
        step prefix
          | i == 0 = 0
          | otherwise = extend (prefix Unboxed.! (i - 1))
          where
            i = Unboxed.length prefix
            extend k
              | needle Unboxed.! k == needle Unboxed.! i = k + 1
              | k == 0 = 0
              | otherwise = extend (prefix Unboxed.! (k - 1))
    go !i !k
      | k == m = True
      | i == n = False
      | haystack Unboxed.! i == needle Unboxed.! k = go (i + 1) (k + 1)
      | k == 0 = go (i + 1) 0
      | otherwise = go i (failure Unboxed.! (k - 1))

-- | 'utf16Slice' that keeps a severed half as itself: 'Right' a 'Text' when
-- the slice is well formed (the common case, with no allocation per code
-- unit when the text has no astral character), 'Left' its units when it
-- cuts a surrogate pair. Both arguments are already clamped into range.
utf16SliceExact :: Int -> Int -> Text -> Either Units Text
utf16SliceExact from count text
  | count <= 0 = Right Text.empty
  | Text.all isBmp text = Right (Text.take count (Text.drop from text))
  | otherwise = fromUnits (unitsSlice from count (toUnits text))

-- | The code unit at an index as a one-unit string, or 'Nothing' past the
-- end. A surrogate half is itself, not U+FFFD.
utf16CharAtExact :: Int -> Text -> Maybe (Either Units Text)
utf16CharAtExact i text
  | i < 0 || i >= utf16Length text = Nothing
  | otherwise = Just (utf16SliceExact i 1 text)
