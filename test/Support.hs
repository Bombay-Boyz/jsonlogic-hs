-- | Helpers shared by the test modules.
module Support
  ( json
  , Problem (..)
  , problem
  , at
  , isRight'
  ) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import JsonLogic

{- HLINT ignore json "Avoid restricted function" -}

-- | A JSON literal written in the test. A malformed one is a bug in the
-- test, so it stops the test with the literal in the message (ADR 0004).
json :: String -> Value
json text = fromMaybe (error ("bad test JSON: " ++ text)) (decode (Char8.pack text))

-- | What went wrong, without where: most tests are about the first.
data Problem
  = ParseProblem ParseError
  | EvalProblem EvalError
  deriving (Eq, Show)

-- | An answer with the location dropped from a failure.
problem :: Either Error a -> Either Problem a
problem (Left (ParseFailure _ e)) = Left (ParseProblem e)
problem (Left (EvalFailure _ e)) = Left (EvalProblem e)
problem (Right a) = Right a

-- | A location from its JSON Pointer segments, written as keys and indices.
at :: [Either Text Int] -> Location
at = fromSegments . map (either Key Index)

isRight' :: Either a b -> Bool
isRight' = either (const False) (const True)
