{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : JsonLogic.Core.Parse
--
-- Reads a JSON 'Value' into a 'Rule'. Reading is an unfold
-- ('unfoldFixM'): a seed is a piece of JSON still to read, with how deeply it
-- is nested and where it is, and each step produces one node whose children
-- are further seeds.
--
-- What counts as a rule follows the reference implementation: an object with
-- exactly one key is an operator call, an array is read element by element,
-- and anything else (including an object with zero or several keys) is a
-- literal.
--
-- __Arity.__ The reference ignores surplus operands and treats missing ones as
-- JavaScript @undefined@. In 'Strict' mode (the default) a wrong count is a
-- precise 'ArityMismatch' instead. In 'Lenient' mode the count is repaired
-- here, exactly as the reference behaves: missing operands become
-- @undefined@ (@null@ for the initial value of @reduce@), and surplus ones are
-- dropped, after being evaluated for their failures where the reference
-- evaluates them ('Discard'). How each builtin treats its operands is stated
-- once, in @shapeOf@.
module JsonLogic.Core.Parse
  ( parseRuleWith
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (unfoldFixM)
import Data.Foldable (toList)
import Data.Text (Text)

import JsonLogic.Core.Config (ArityMode (..), Config (..))
import JsonLogic.Core.Errors (Arity (..), ParseError (..))
import JsonLogic.Core.Location (Location, Segment (..), child, root)
import JsonLogic.Core.Operator
  ( Builtin (..)
  , OperatorName (..)
  , builtinOperatorName
  , lookupBuiltin
  )
import JsonLogic.Core.Rule (Node (..), Rule, RuleF (..))
import JsonLogic.Core.Value (jsonWeightUpTo, nestsDeeperThan, saturatingSucc)

-- | Read a rule with the configured depth limit, node limit and arity mode.
-- A failure says where in the rule it happened. A rule with too many nodes
-- is refused before any of it is read.
parseRuleWith :: Config -> Value -> Either (Location, ParseError) Rule
parseRuleWith config value
  | jsonWeightUpTo (saturatingSucc limit) value > limit = Left (root, NodeLimitExceeded limit)
  | otherwise = unfoldFixM (step config) (Seed 0 root (Json value))
  where
    limit = maxNodes config

-- | Something still to read: how many levels deep it is, where it is, and
-- what it is.
data Seed = Seed !Int !Location !Source

data Source
  = -- | JSON from the rule.
    Json Value
  | -- | An operand the rule leaves out, which the reference reads as
    -- @undefined@ or, in two places, @null@.
    Absent Absence
  | -- | An operator call whose operand count has already been repaired.
    Repaired Builtin [Seed]

data Absence = AbsentUndefined | AbsentNull

-- | One layer of the unfold.
step :: Config -> Seed -> Either (Location, ParseError) (Node Seed)
step config (Seed depth location source)
  | depth > limit = Left (location, DepthExceeded limit)
  | otherwise = Node location <$> layer
  where
    limit = maxDepth config
    layer = case source of
      Json (Array items) ->
        Right (Arr [Seed (depth + 1) (child location (Index i)) (Json item) | (i, item) <- zip [0 ..] (toList items)])
      Json value@(Object fields) -> case KeyMap.toList fields of
        [(key, operands)] -> operator config depth location (Key.toText key) operands
        _ -> literal value
      Json value -> literal value
      -- @{"and": []}@ is how the reference's @undefined@ is written as a rule
      Absent AbsentUndefined -> Right (And [])
      Absent AbsentNull -> Right (Lit Null)
      Repaired builtin operands -> build location builtin operands
    -- A literal's own nesting counts towards the limit, so a deeply nested
    -- object cannot be smuggled past it inside a rule.
    literal value
      | nestsDeeperThan (limit - depth) value = Left (location, DepthExceeded limit)
      | otherwise = Right (Lit value)

-- | An operator call: a builtin, read to its shape, or a custom operator.
operator :: Config -> Int -> Location -> Text -> Value -> Either (Location, ParseError) (RuleF Seed)
operator config depth location name operands = case lookupBuiltin name of
  Nothing -> Right (CustomOp (OperatorName name) seeds)
  Just builtin -> case arityMode config of
    Strict
      | admits (shapeArity shape) (length seeds) -> build location builtin (padded depth location builtin seeds)
      | otherwise -> Left (location, ArityMismatch (builtinOperatorName builtin) (shapeArity shape) (length seeds))
      where
        shape = shapeOf builtin
    Lenient -> lenient depth location builtin seeds
  where
    seeds = case operands of
      Array items ->
        [Seed (depth + 1) (child here (Index i)) (Json item) | (i, item) <- zip [0 ..] (toList items)]
      -- a bare operand is shorthand for a list of one
      other -> [Seed (depth + 1) here (Json other)]
    here = child location (Key name)

-- | Repair the operand count as the reference would, and build the call.
lenient :: Int -> Location -> Builtin -> [Seed] -> Either (Location, ParseError) (RuleF Seed)
lenient depth location builtin seeds = case maximumOf (shapeArity shape) of
  Just most
    | length full > most -> case shapeSurplus shape of
        NeverEvaluated -> build location builtin (take most full)
        Evaluated -> Right (Discard (Seed depth location (Repaired builtin (take most full))) (drop most full))
  _ -> build location builtin full
  where
    shape = shapeOf builtin :: Shape Seed
    full = padded depth location builtin seeds

-- | The operands, followed by the ones the reference reads by position but
-- the rule leaves out, as absent values. There is no JSON for those, so they
-- are given the location of the operator itself.
padded :: Int -> Location -> Builtin -> [Seed] -> [Seed]
padded depth location builtin seeds =
  seeds
    ++ [ Seed (depth + 1) location (Absent (absence builtin i))
       | i <- [length seeds .. shapePositional (shapeOf builtin :: Shape Seed) - 1]
       ]

-- | What the reference reads for a missing operand: @undefined@, except
-- that a missing @var@ path is the data itself (the classic suite requires
-- @{"var": []}@ to mean the data, as @{"var": null}@ does), and the missing
-- initial value of @reduce@ is @null@.
absence :: Builtin -> Int -> Absence
absence BVar 0 = AbsentNull
absence BReduce 2 = AbsentNull
absence _ _ = AbsentUndefined

-- | Build a call whose operand count the shape admits.
build :: Location -> Builtin -> [Seed] -> Either (Location, ParseError) (RuleF Seed)
build location builtin seeds = case shapeBuild shape seeds of
  Just node -> Right node
  Nothing -> Left (location, ArityMismatch (builtinOperatorName builtin) (shapeArity shape) (length seeds))
  where
    shape = shapeOf builtin

-- ---------------------------------------------------------------------
-- How each builtin takes its operands
-- ---------------------------------------------------------------------

-- | How an operator takes its operands, building nodes with children of
-- type @a@.
data Shape a = Shape
  { shapeArity :: Arity
  -- ^ The operand counts strict mode accepts.
  , shapePositional :: Int
  -- ^ How many leading operands the reference reads by position, so that a
  -- missing one is read as absent ('absence').
  , shapeSurplus :: Surplus
  -- ^ Whether the reference evaluates operands past the most it uses.
  , shapeBuild :: [a] -> Maybe (RuleF a)
  -- ^ The node, if the count is one the constructor can hold.
  }

data Surplus
  = -- | Every operand is evaluated before the operator runs.
    Evaluated
  | -- | The operator evaluates only the operands it uses (@map@ and the
    -- other operators with a body).
    NeverEvaluated

-- | The single statement of every builtin's operands. The evaluator follows
-- it; the outlier suite checks it against the reference in both modes.
shapeOf :: Builtin -> Shape a
shapeOf builtin = case builtin of
  BVar -> Shape (Between 0 2) 1 Evaluated (oneOrTwo Var)
  BMissing -> variadic Missing
  BMissingSome -> exactlyTwo MissingSome
  BIf -> variadic If
  BTernary -> variadic If
  BAnd -> variadic And
  BOr -> variadic Or
  BNot -> exactlyOne Not
  BDoubleNot -> exactlyOne DoubleNot
  BLooseEq -> exactlyTwo LooseEq
  BStrictEq -> exactlyTwo StrictEq
  BLooseNeq -> exactlyTwo LooseNeq
  BStrictNeq -> exactlyTwo StrictNeq
  BLt -> twoOrThree Lt
  BLte -> twoOrThree Lte
  BGt -> exactlyTwo Gt
  BGte -> exactlyTwo Gte
  BMax -> variadic Max
  BMin -> variadic Min
  BAdd -> variadic Add
  BSub -> Shape (Between 1 2) 1 Evaluated (oneOrTwo Sub)
  -- strict mode refuses an empty @*@; lenient mode keeps it, and it throws
  -- if it is ever evaluated, as in the reference
  BMul -> Shape (AtLeast 1) 0 Evaluated (Just . Mul)
  BDiv -> exactlyTwo Div
  BMod -> exactlyTwo Mod
  BMap -> lazily (exactlyTwo Map)
  BFilter -> lazily (exactlyTwo Filter)
  BReduce -> lazily (exactlyThree Reduce)
  BAll -> lazily (exactlyTwo All)
  BNone -> lazily (exactlyTwo None)
  BSome -> lazily (exactlyTwo Some)
  BMerge -> variadic Merge
  BIn -> exactlyTwo In
  BCat -> variadic Cat
  BSubstr -> twoOrThree Substr
  BLog -> exactlyOne Log
  where
    variadic make = Shape (AtLeast 0) 0 Evaluated (Just . make)
    exactlyOne make = Shape (Exactly 1) 1 Evaluated $ \case
      [x] -> Just (make x)
      _ -> Nothing
    exactlyTwo make = Shape (Exactly 2) 2 Evaluated $ \case
      [x, y] -> Just (make x y)
      _ -> Nothing
    exactlyThree make = Shape (Exactly 3) 3 Evaluated $ \case
      [x, y, z] -> Just (make x y z)
      _ -> Nothing
    oneOrTwo make = \case
      [x] -> Just (make x Nothing)
      [x, y] -> Just (make x (Just y))
      _ -> Nothing
    twoOrThree make = Shape (Between 2 3) 2 Evaluated $ \case
      [x, y] -> Just (make x y Nothing)
      [x, y, z] -> Just (make x y (Just z))
      _ -> Nothing
    lazily shape = shape{shapeSurplus = NeverEvaluated}

admits :: Arity -> Int -> Bool
admits (Exactly n) given = given == n
admits (AtLeast n) given = given >= n
admits (Between low high) given = low <= given && given <= high

maximumOf :: Arity -> Maybe Int
maximumOf (Exactly n) = Just n
maximumOf (AtLeast _) = Nothing
maximumOf (Between _ high) = Just high
