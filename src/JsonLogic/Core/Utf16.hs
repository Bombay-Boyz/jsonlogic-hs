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
-- 'Text'. A 'Text' cannot hold a lone surrogate, so a slice that cuts
-- through a surrogate pair yields U+FFFD for the severed half. That is the
-- single accepted deviation from the reference here, and it is exactly what
-- @String.prototype.toWellFormed()@ would make of the reference's answer.
module JsonLogic.Core.Utf16
  ( utf16Length
  , utf16Slice
  , utf16CharAt
  , compareUtf16
  , utf16Units
  , fromUtf16Units
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)

-- | The number of UTF-16 code units: JavaScript's @s.length@.
utf16Length :: Text -> Int
utf16Length = Text.foldl' (\n c -> if ord c > 0xFFFF then n + 2 else n + 1) 0

-- | The UTF-16 code units of a text.
utf16Units :: Text -> [Word16]
utf16Units = concatMap charUnits . Text.unpack

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

-- | Rebuild a text from UTF-16 code units. A surrogate with no partner
-- becomes U+FFFD.
fromUtf16Units :: [Word16] -> Text
fromUtf16Units = Text.pack . go
  where
    go [] = []
    go (hi : lo : rest)
      | isHigh hi && isLow lo = pair hi lo : go rest
    go (u : rest)
      | isHigh u || isLow u = '\xFFFD' : go rest
      | otherwise = chr (fromIntegral u) : go rest
    isHigh u = u >= 0xD800 && u <= 0xDBFF
    isLow u = u >= 0xDC00 && u <= 0xDFFF
    pair hi lo =
      chr
        ( 0x10000
            + ((fromIntegral hi - 0xD800) `shiftL` 10)
            + (fromIntegral lo - 0xDC00)
        )

-- | @slice from count@: the @count@ code units starting at code unit
-- @from@, both already clamped into range by the caller. Cutting through a
-- surrogate pair gives U+FFFD for the severed half.
utf16Slice :: Int -> Int -> Text -> Text
utf16Slice from count text
  | count <= 0 = Text.empty
  | Text.all isBmp text = Text.take count (Text.drop from text)
  | otherwise = fromUtf16Units (take count (drop from (utf16Units text)))

-- | The code unit at an index, as a one-unit string, or 'Nothing' past the
-- end. A lone surrogate half is returned as U+FFFD.
utf16CharAt :: Int -> Text -> Maybe Text
utf16CharAt i text
  | i < 0 = Nothing
  | Text.all isBmp text =
      let picked = Text.take 1 (Text.drop i text)
       in if Text.null picked then Nothing else Just picked
  | otherwise = case drop i (utf16Units text) of
      [] -> Nothing
      unit : _ -> Just (fromUtf16Units [unit])

isBmp :: Char -> Bool
isBmp c = ord c <= 0xFFFF

-- | JavaScript's string ordering: lexicographic by UTF-16 code unit.
--
-- It differs from ordering by code point only when an astral character
-- (first unit U+D800 to U+DBFF) meets a BMP character in U+E000 to U+FFFF:
-- by code unit the astral one is smaller.
compareUtf16 :: Text -> Text -> Ordering
compareUtf16 a b = go (Text.unpack a) (Text.unpack b)
  where
    go [] [] = EQ
    go [] _ = LT
    go _ [] = GT
    go (x : xs) (y : ys)
      | x == y = go xs ys
      | otherwise = compare (charUnits x) (charUnits y)
