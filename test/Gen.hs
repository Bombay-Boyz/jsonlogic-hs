{-# LANGUAGE OverloadedStrings #-}

-- | Generators shared by the property tests: bounded random rules and data
-- over every builtin operator, deliberately including the awkward values
-- (empty and non-ASCII strings, signed zero, huge and tiny numbers).
module Gen
  ( genData
  , genRule
  , genText
  , allOperators
  ) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as Text
import Test.QuickCheck

import JsonLogic.Core.Operator (allBuiltins, builtinName)

-- | Every builtin operator's name, from the library's own table.
allOperators :: [Text]
allOperators = map builtinName allBuiltins

-- | Text drawn from ASCII, BMP characters above U+E000, and astral
-- characters, so UTF-16 effects show up.
genText :: Gen Text
genText = Text.pack <$> listOf (elements pool)
  where
    pool = "ab0 1.-e" ++ ['\xE000', '\xFFFF', '\xFFFD', '\x1F600', '\x10000', '\x10FFFF', '\x0301']

genScalar :: Gen Value
genScalar =
  oneof
    [ pure Null
    , Bool <$> arbitrary
    , toJSON <$> (choose (-5, 12) :: Gen Int)
    , toJSON <$> elements [0.5, -2.25, 1e21, 1e-7, 5e-324, 1.7976931348623157e308 :: Double]
    , String <$> genText
    , String <$> elements ["", "0", "1", "10", "9", "0x10", " 12 ", "Infinity", "NaN", "a.b"]
    ]

-- | Data: scalars, arrays and objects with the keys rules look for.
genData :: Int -> Gen Value
genData 0 = genScalar
genData n =
  frequency
    [ (3, genScalar)
    , (2, toJSON <$> listOf (genData (n - 1)))
    ,
      ( 2
      , do
          a <- genData (n - 1)
          b <- genData (n - 1)
          l <- listOf (genData (n - 1))
          pure (object ["a" .= a, "b" .= b, "l" .= l])
      )
    ]

-- | A rule of bounded size. Argument counts are arbitrary on purpose, so
-- both arity modes get exercised.
genRule :: Int -> Gen Value
genRule 0 =
  oneof
    [ genScalar
    , (\p -> object ["var" .= (p :: Text)]) <$> elements ["a", "b", "l", "l.0", "l.length", "", "a.b"]
    ]
genRule n = frequency [(2, genRule 0), (6, call)]
  where
    call = do
      name <- elements allOperators
      k <- choose (0, 3)
      args <- vectorOf k (genRule (n `div` 2))
      pure (object [Key.fromText name .= args])
