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
-- surrogate pair yields U+FFFD for the severed half. That is the
-- single accepted deviation from the reference here, and it is exactly what
-- @String.prototype.toWellFormed()@ would make of the reference's answer.
module JsonLogic.Core.Utf16
  ( utf16Length
  , utf16Slice
  , utf16CharAt
  , compareUtf16
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
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
