{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Operator
--
-- The names of operators, and the closed set of builtin ones.
--
-- This module is the single source of truth for which operator names are
-- builtin. The parser, the custom-operator registry (which refuses builtin
-- names), the test generators and the conformance runners all take the set
-- from 'allBuiltins', so adding an operator is one constructor here plus the
-- cases the compiler then demands, never an edit to a list kept elsewhere.
module JsonLogic.Core.Operator
  ( OperatorName (..)
  , Builtin (..)
  , allBuiltins
  , builtinName
  , builtinOperatorName
  , lookupBuiltin
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | An operator's name, as it appears as the only key of a rule object.
-- A newtype, so it cannot be confused with a variable path or any other
-- string.
newtype OperatorName = OperatorName Text
  deriving (Eq, Ord, Show)

-- | The operators of the classic JsonLogic specification
-- (<https://jsonlogic.com/operations.html>), as implemented by the reference,
-- json-logic-js 2.0.5. @?:@ is the reference's alias of @if@ and has its own
-- constructor because it has its own name.
data Builtin
  = BVar
  | BMissing
  | BMissingSome
  | BIf
  | BTernary
  | BAnd
  | BOr
  | BNot
  | BDoubleNot
  | BLooseEq
  | BStrictEq
  | BLooseNeq
  | BStrictNeq
  | BLt
  | BLte
  | BGt
  | BGte
  | BMax
  | BMin
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BMap
  | BFilter
  | BReduce
  | BAll
  | BNone
  | BSome
  | BMerge
  | BIn
  | BCat
  | BSubstr
  | BLog
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Every builtin operator, in declaration order.
allBuiltins :: [Builtin]
allBuiltins = [minBound .. maxBound]

-- | The name a rule uses for a builtin.
builtinName :: Builtin -> Text
builtinName builtin = case builtin of
  BVar -> "var"
  BMissing -> "missing"
  BMissingSome -> "missing_some"
  BIf -> "if"
  BTernary -> "?:"
  BAnd -> "and"
  BOr -> "or"
  BNot -> "!"
  BDoubleNot -> "!!"
  BLooseEq -> "=="
  BStrictEq -> "==="
  BLooseNeq -> "!="
  BStrictNeq -> "!=="
  BLt -> "<"
  BLte -> "<="
  BGt -> ">"
  BGte -> ">="
  BMax -> "max"
  BMin -> "min"
  BAdd -> "+"
  BSub -> "-"
  BMul -> "*"
  BDiv -> "/"
  BMod -> "%"
  BMap -> "map"
  BFilter -> "filter"
  BReduce -> "reduce"
  BAll -> "all"
  BNone -> "none"
  BSome -> "some"
  BMerge -> "merge"
  BIn -> "in"
  BCat -> "cat"
  BSubstr -> "substr"
  BLog -> "log"

-- | 'builtinName' as an 'OperatorName'.
builtinOperatorName :: Builtin -> OperatorName
builtinOperatorName = OperatorName . builtinName

-- | The builtin with exactly this name, if there is one. There is no
-- trimming, no case folding and no dotted-path resolution.
lookupBuiltin :: Text -> Maybe Builtin
lookupBuiltin name = Map.lookup name byName

byName :: Map.Map Text Builtin
byName = Map.fromList [(builtinName builtin, builtin) | builtin <- allBuiltins]
