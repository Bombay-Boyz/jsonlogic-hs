{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : JsonLogic.Core.Ryu
--
-- The shortest decimal that reads back as a double: Ryū, by Ulf Adams
-- (\"Ryū: fast float-to-string conversion\", PLDI 2018), following the
-- reference implementation's @d2d@ (github.com/ulfjack/ryu, @d2s.c@).
--
-- Ryū's answer is the one ECMAScript's @Number::toString@ asks for: the
-- fewest significant digits whose decimal reads back as the double under
-- round-half-even (so an endpoint of the rounding interval counts when the
-- double's significand is even), and among those the closest to the double,
-- an exact tie going to the even digit.
--
-- The 128-bit multipliers of the reference implementation's tables are
-- computed here once, by the formulas its table generator uses, and the
-- products are taken in 64-bit words, as the reference does. The test suite checks this module against
-- an independent exact algorithm (@JsonLogic.Core.Number.shortestDigitsExact@)
-- on hundreds of thousands of doubles.
module JsonLogic.Core.Ryu
  ( shortestDecimalRyu
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed as Unboxed
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)

-- | For a positive, finite, non-zero double, @(digits, exponent)@ with the
-- double's shortest decimal equal to @digits * 10^exponent@. The digits may
-- end in zeros; the caller strips them.
shortestDecimalRyu :: Double -> (Word64, Int)
shortestDecimalRyu d = removeDigits acceptBounds (interval acceptBounds e2 m2 mmShift)
  where
    bits = castDoubleToWord64 d
    ieeeMantissa = bits .&. ((1 `shiftL` mantissaBits) - 1)
    ieeeExponent = fromIntegral ((bits `shiftR` mantissaBits) .&. 0x7FF) :: Int
    -- Step 1: the double as m2 * 2^e2, with two bits of room below.
    e2
      | ieeeExponent == 0 = 1 - bias - mantissaBits - 2
      | otherwise = ieeeExponent - bias - mantissaBits - 2
    m2
      | ieeeExponent == 0 = ieeeMantissa
      | otherwise = (1 `shiftL` mantissaBits) + ieeeMantissa
    -- An endpoint of the rounding interval reads back to the double under
    -- round-half-even exactly when the double's significand is even.
    acceptBounds = even m2
    -- The lower end of the interval is closer when the significand is a power
    -- of two (the gap below it is half the gap above).
    mmShift = if ieeeMantissa /= 0 || ieeeExponent <= 1 then 1 else 0

-- | Step 2 and 3: the decimals that read back, @[vm, vp]@ around the double's
-- own @vr@, scaled by @10^e10@, and whether the digits dropped by the scaling
-- were all zeros (for @vm@ and for @vr@).
data Interval = Interval
  { _vr :: {-# UNPACK #-} !Word64
  , _vp :: {-# UNPACK #-} !Word64
  , _vm :: {-# UNPACK #-} !Word64
  , _e10 :: {-# UNPACK #-} !Int
  , _vmZeros :: !Bool
  , _vrZeros :: !Bool
  }

interval :: Bool -> Int -> Word64 -> Word64 -> Interval
interval acceptBounds e2 m2 mmShift
  | e2 >= 0 =
      let q = log10Pow2 e2 - (if e2 > 3 then 1 else 0)
          k = pow5InvBitCount + pow5bits q - 1
          i = negate e2 + q + k
          (vr, vp, vm) = mulShiftAll m2 (pow5InvSplit q) i mmShift
       in if q > 21
            then Interval vr vp vm q False False
            else
              if mv `rem` 5 == 0
                then Interval vr vp vm q False (multipleOfPowerOf5 mv q)
                else
                  if acceptBounds
                    then Interval vr vp vm q (multipleOfPowerOf5 (mv - 1 - mmShift) q) False
                    else Interval vr (vp - (if multipleOfPowerOf5 (mv + 2) q then 1 else 0)) vm q False False
  | otherwise =
      let q = log10Pow5 (negate e2) - (if negate e2 > 1 then 1 else 0)
          i = negate e2 - q
          k = pow5bits i - pow5BitCount
          j = q - k
          (vr, vp, vm) = mulShiftAll m2 (pow5Split i) j mmShift
       in if q <= 1
            then
              -- mv has at least q trailing zero bits, so vr is exact
              if acceptBounds
                then Interval vr vp vm (q + e2) (mmShift == 1) True
                else Interval vr (vp - 1) vm (q + e2) False True
            else Interval vr vp vm (q + e2) False (q < 63 && multipleOfPowerOf2 mv q)
  where
    mv = 4 * m2

-- | Step 4: remove digits while the interval still holds a shorter decimal,
-- then round.
removeDigits :: Bool -> Interval -> (Word64, Int)
removeDigits acceptBounds (Interval vr0 vp0 vm0 e10 vmZeros0 vrZeros0)
  | vmZeros0 || vrZeros0 =
      -- The general case, which keeps track of whether the removed digits of
      -- vm and vr were all zeros, so that bounds and exact ties are decided
      -- right.
      case dropWhileWide vr0 vp0 vm0 vmZeros0 vrZeros0 0 0 of
        Dropped vr1 vp1 vm1 vmZeros1 vrZeros1 last1 removed1 ->
          let Dropped vr2 _ vm2 _ vrZeros2 last2 removed2
                | vmZeros1 = dropZerosOfVm vr1 vp1 vm1 vrZeros1 last1 removed1
                | otherwise = Dropped vr1 vp1 vm1 vmZeros1 vrZeros1 last1 removed1
              -- round half to even when the exact value is ...50...0
              last3 = if vrZeros2 && last2 == 5 && even vr2 then 4 else last2
              up = (vr2 == vm2 && (not acceptBounds || not vmZeros1)) || last3 >= 5
           in (vr2 + (if up then 1 else 0), e10 + removed2)
  | otherwise =
      -- The common case: no exact ties or bounds to worry about. Two digits
      -- at a time first, while that is possible.
      let wide = vp0 `quot` 100 > vm0 `quot` 100
          vr1 = if wide then vr0 `quot` 100 else vr0
          vp1 = if wide then vp0 `quot` 100 else vp0
          vm1 = if wide then vm0 `quot` 100 else vm0
          roundUp1 = wide && vr0 `rem` 100 >= 50
       in case dropCommon vr1 vp1 vm1 roundUp1 (if wide then 2 else 0) of
            Dropped vr2 _ vm2 _ roundUp2 _ removed2 ->
              (vr2 + (if vr2 == vm2 || roundUp2 then 1 else 0), e10 + removed2)

-- | The state of digit removal: vr, vp, vm, whether vm's removed digits
-- were zeros, whether vr's were (or, in the common case, whether to round
-- up), the last digit removed from vr, and how many were removed.
data Dropped
  = Dropped
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      !Bool
      !Bool
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Int

-- | Remove digits while the shortened interval is still non-empty, tracking
-- trailing zeros.
dropWhileWide :: Word64 -> Word64 -> Word64 -> Bool -> Bool -> Word64 -> Int -> Dropped
dropWhileWide !vr !vp !vm !vmZeros !vrZeros !lastDigit !removed
  | vp `quot` 10 <= vm `quot` 10 = Dropped vr vp vm vmZeros vrZeros lastDigit removed
  | otherwise =
      dropWhileWide
        (vr `quot` 10)
        (vp `quot` 10)
        (vm `quot` 10)
        (vmZeros && vm `rem` 10 == 0)
        (vrZeros && lastDigit == 0)
        (vr `rem` 10)
        (removed + 1)

-- | When vm ends in zeros, remove them too: the shorter vm is still in range.
dropZerosOfVm :: Word64 -> Word64 -> Word64 -> Bool -> Word64 -> Int -> Dropped
dropZerosOfVm !vr !vp !vm !vrZeros !lastDigit !removed
  | vm == 0 || vm `rem` 10 /= 0 = Dropped vr vp vm True vrZeros lastDigit removed
  | otherwise =
      dropZerosOfVm
        (vr `quot` 10)
        (vp `quot` 10)
        (vm `quot` 10)
        (vrZeros && lastDigit == 0)
        (vr `rem` 10)
        (removed + 1)

-- | The common case's digit removal: remember only whether to round up.
dropCommon :: Word64 -> Word64 -> Word64 -> Bool -> Int -> Dropped
dropCommon !vr !vp !vm !roundUp !removed
  | vp `quot` 10 <= vm `quot` 10 = Dropped vr vp vm False roundUp 0 removed
  | otherwise = dropCommon (vr `quot` 10) (vp `quot` 10) (vm `quot` 10) (vr `rem` 10 >= 5) (removed + 1)

-- | @floor (m * multiplier / 2^shift)@ for the centre and both ends of the
-- interval: @4m@, @4m + 2@ and @4m - 1 - mmShift@. The multiplier is 128
-- bits, as a (low, high) pair.
mulShiftAll :: Word64 -> (Word64, Word64) -> Int -> Word64 -> (Word64, Word64, Word64)
{-# INLINE mulShiftAll #-}
mulShiftAll m multiplier shift mmShift =
  ( mulShift64 (4 * m) multiplier shift
  , mulShift64 (4 * m + 2) multiplier shift
  , mulShift64 (4 * m - 1 - mmShift) multiplier shift
  )

-- | @floor (m * multiplier / 2^shift)@ for a 64-bit @m@ and a 128-bit
-- multiplier, with @64 < shift < 128@: the reference implementation's
-- @mulShift64@.
mulShift64 :: Word64 -> (Word64, Word64) -> Int -> Word64
{-# INLINE mulShift64 #-}
mulShift64 m (mulLow, mulHigh) shift = shiftRight128 sumLow sumHigh (shift - 64)
  where
    (high0, _) = umul128 m mulLow
    (high2, low2) = umul128 m mulHigh
    sumLow = high0 + low2
    sumHigh = high2 + (if sumLow < high0 then 1 else 0)

-- | The low 64 bits of a 128-bit value shifted right by @0 < n < 64@.
shiftRight128 :: Word64 -> Word64 -> Int -> Word64
{-# INLINE shiftRight128 #-}
shiftRight128 low high n = (high `shiftL` (64 - n)) .|. (low `shiftR` n)

-- | The full 128-bit product of two 64-bit words, as (high, low), from
-- 32-bit halves (the reference implementation's portable @umul128@).
umul128 :: Word64 -> Word64 -> (Word64, Word64)
{-# INLINE umul128 #-}
umul128 a b = (high, low)
  where
    lowMask = 0xFFFFFFFF
    aLow = a .&. lowMask
    aHigh = a `shiftR` 32
    bLow = b .&. lowMask
    bHigh = b `shiftR` 32
    b00 = aLow * bLow
    b01 = aLow * bHigh
    b10 = aHigh * bLow
    b11 = aHigh * bHigh
    mid1 = b10 + (b00 `shiftR` 32)
    mid2 = b01 + (mid1 .&. lowMask)
    high = b11 + (mid1 `shiftR` 32) + (mid2 `shiftR` 32)
    low = ((mid2 .&. lowMask) `shiftL` 32) .|. (b00 .&. lowMask)

mantissaBits :: Int
mantissaBits = 52

bias :: Int
bias = 1023

pow5InvBitCount :: Int
pow5InvBitCount = 125

pow5BitCount :: Int
pow5BitCount = 125

-- | The bit length of @5^e@, for @0 <= e <= 3528@.
pow5bits :: Int -> Int
pow5bits e = ((e * 1217359) `shiftR` 19) + 1

-- | @floor (log10 (2^e))@, for @0 <= e <= 1650@.
log10Pow2 :: Int -> Int
log10Pow2 e = (e * 78913) `shiftR` 18

-- | @floor (log10 (5^e))@, for @0 <= e <= 2620@.
log10Pow5 :: Int -> Int
log10Pow5 e = (e * 732923) `shiftR` 20

multipleOfPowerOf5 :: Word64 -> Int -> Bool
multipleOfPowerOf5 value p = pow5Factor value 0 >= p
  where
    pow5Factor !v !count
      | v /= 0 && v `rem` 5 == 0 = pow5Factor (v `quot` 5) (count + 1)
      | otherwise = count :: Int

multipleOfPowerOf2 :: Word64 -> Int -> Bool
multipleOfPowerOf2 value p = value .&. ((1 `shiftL` p) - 1) == 0

-- | @floor (2^(bitLength (5^q) - 1 + 125) / 5^q) + 1@, the reference
-- implementation's @DOUBLE_POW5_INV_SPLIT[q]@, as (low, high) words.
pow5InvSplit :: Int -> (Word64, Word64)
pow5InvSplit q = (pow5InvLow Unboxed.! q, pow5InvHigh Unboxed.! q)

-- | @5^i@ normalised to 125 bits, the reference implementation's
-- @DOUBLE_POW5_SPLIT[i]@, as (low, high) words.
pow5Split :: Int -> (Word64, Word64)
pow5Split i = (pow5Low Unboxed.! i, pow5High Unboxed.! i)

-- | Both tables are computed once, on first use, from their definitions.
pow5InvLow, pow5InvHigh, pow5Low, pow5High :: Unboxed.Vector Word64
pow5InvLow = Unboxed.generate pow5InvTableSize (lowWord . pow5InvEntry)
pow5InvHigh = Unboxed.generate pow5InvTableSize (highWord . pow5InvEntry)
pow5Low = Unboxed.generate pow5TableSize (lowWord . pow5Entry)
pow5High = Unboxed.generate pow5TableSize (highWord . pow5Entry)
{-# NOINLINE pow5InvLow #-}
{-# NOINLINE pow5InvHigh #-}
{-# NOINLINE pow5Low #-}
{-# NOINLINE pow5High #-}

-- | Large enough for every double: @q@ reaches 291 and @i@ reaches 325.
pow5InvTableSize, pow5TableSize :: Int
pow5InvTableSize = 342
pow5TableSize = 326

pow5InvEntry :: Int -> Integer
pow5InvEntry q = ((1 :: Integer) `shiftL` (bitLength power - 1 + pow5InvBitCount)) `quot` power + 1
  where
    power = 5 ^ q

pow5Entry :: Int -> Integer
pow5Entry i
  | excess >= 0 = power `shiftR` excess
  | otherwise = power `shiftL` negate excess
  where
    power = 5 ^ i :: Integer
    excess = bitLength power - pow5BitCount

lowWord :: Integer -> Word64
lowWord = fromInteger . (.&. 0xFFFFFFFFFFFFFFFF)

highWord :: Integer -> Word64
highWord = fromInteger . (`shiftR` 64)

bitLength :: Integer -> Int
bitLength = go 0
  where
    go n v
      | v == 0 = n
      | otherwise = go (n + 1) (v `shiftR` 1)
