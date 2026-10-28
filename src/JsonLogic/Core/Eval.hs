{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : JsonLogic.Core.Eval
--
-- Compiling a rule to a function of the data, and running it.
--
-- Compilation is a fold ('foldFix') over the rule tree, done once. Its
-- carrier is a /function/ from the datum a rule is evaluated against to a
-- result (@Eval@), not a value. That is what lets @map@, @filter@, @reduce@
-- and the others run the same sub-rule once per element against a different
-- datum, and what makes @if@, @and@ and @or@ short-circuit with no extra
-- code: a branch not taken is a function never applied. Everything that does
-- not depend on the data (converting literals, resolving custom operators,
-- choosing each node's code) happens during the fold, so running a compiled
-- rule many times pays for it once.
--
-- Evaluation runs in 'Run', a small state-passing monad whose state is the
-- remaining step budget. It is pure: the budget is threaded through the
-- evaluation, never held in a global or in 'IO'. A failure carries the
-- location of the node that failed. See "JsonLogic.Core.Config" for the
-- limits.
--
-- Operand evaluation order follows the reference: every operand is evaluated
-- before its operator runs, except where the reference evaluates lazily
-- (@if@, @and@, @or@) or against a scoped datum (@map@, @filter@, @reduce@,
-- @all@, @none@, @some@).
module JsonLogic.Core.Eval
  ( Program
  , compileRule
  , runProgram
  ) where

import Control.Monad (foldM, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (digitToInt, isDigit)
import Data.Fix (foldFix)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Unsafe (lengthWord8)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import JsonLogic.Core.Config (Config (..))
import JsonLogic.Core.Custom (Operators, lookupOperator)
import JsonLogic.Core.Errors (EvalError (..), LimitKind (..))
import JsonLogic.Core.Location (Location)
import JsonLogic.Core.Number (maximumJs, minimumJs, remainderJs)
import JsonLogic.Core.Operator (Builtin (..), builtinOperatorName)
import JsonLogic.Core.Rule (Node (..), Rule, RuleF (..))
import JsonLogic.Core.Utf16 (utf16CharAt, utf16Length, utf16Slice)
import JsonLogic.Core.Value

-- ---------------------------------------------------------------------
-- The evaluation monad
-- ---------------------------------------------------------------------

-- | The outcome of a step: its value and the budget left, or where and why
-- it failed. One constructor per outcome, strict, so the budget cannot pile
-- up as a chain of thunks and a step allocates one cell.
data Result a
  = Done !a {-# UNPACK #-} !Int
  | Failed !Location !EvalError

-- | A computation that may fail, at a location in the rule, and that spends
-- steps from a budget.
newtype Run a = Run {unRun :: Int -> Result a}

instance Functor Run where
  fmap f (Run m) = Run $ \budget -> case m budget of
    Done a left -> Done (f a) left
    Failed location e -> Failed location e
  {-# INLINE fmap #-}

instance Applicative Run where
  pure a = Run (Done a)
  {-# INLINE pure #-}
  Run mf <*> Run ma = Run $ \budget -> case mf budget of
    Failed location e -> Failed location e
    Done f left -> case ma left of
      Failed location e -> Failed location e
      Done a left' -> Done (f a) left'
  {-# INLINE (<*>) #-}

instance Monad Run where
  Run m >>= k = Run $ \budget -> case m budget of
    Failed location e -> Failed location e
    Done a left -> unRun (k a) left
  {-# INLINE (>>=) #-}

-- | What every node's code knows about itself: the configuration it was
-- compiled with and where it is in the rule.
data At = At !Config !Location

-- | Fail here.
failAt :: At -> EvalError -> Run a
failAt (At _ location) e = Run (const (Failed location e))

-- | Fail here because a limit was exceeded.
exceeded :: At -> (Config -> LimitKind) -> Run a
exceeded at@(At config _) kind = failAt at (LimitExceeded (kind config))

-- | Spend @n@ steps, or fail here if fewer than @n@ remain.
spend :: At -> Int -> Run ()
spend (At config location) n = Run $ \budget ->
  if n > budget
    then Failed location (LimitExceeded (StepLimit (maxSteps config)))
    else Done () (budget - n)
{-# INLINE spend #-}

-- | Spend steps for converting an operand to a string, which costs as many
-- steps as an array operand has elements, at any depth. Every other value
-- converts for free.
spendOnConversion :: At -> Val -> Run ()
spendOnConversion at value = case value of
  VArr _ -> Run $ \budget -> unRun (spend at (valWeightUpTo (saturatingSucc budget) value)) budget
  _ -> pure ()

-- | How many bytes of a string one step pays for when an operator only
-- /scans/ it (compares, searches, slices): the cost of a scan is its length,
-- whatever the rule around it looks like, so a string is charged by length,
-- from a length that is known in constant time. Short strings cost nothing
-- extra.
bytesPerScanStep :: Int
bytesPerScanStep = 64

-- | The same when an operator may /read a string as a number/, which is
-- about ten times the work per byte of a scan.
bytesPerReadStep :: Int
bytesPerReadStep = 8

-- | Spend steps for an operand an operator scans: an array by its elements,
-- a string by its length.
spendOnScan :: At -> Val -> Run ()
spendOnScan at value = case value of
  VStr _ -> spendOnString at value
  _ -> spendOnConversion at value

-- | Spend steps for a string an operator scans, and nothing for any other
-- operand.
spendOnString :: At -> Val -> Run ()
spendOnString at (VStr s) = spend at (lengthWord8 s `quot` bytesPerScanStep)
spendOnString _ _ = pure ()

-- | Spend steps for an operand an operator may read as a number: an array
-- by its elements, a string by its length at the reading rate.
spendOnReading :: At -> Val -> Run ()
spendOnReading at value = case value of
  VStr s -> spend at (lengthWord8 s `quot` bytesPerReadStep)
  _ -> spendOnConversion at value

-- | Spend steps for two operands that are compared or equated. If both are
-- strings (arrays and objects compare as the strings they convert to) the
-- comparison scans them; otherwise a string is read as a number.
spendOnComparison :: At -> Val -> Val -> Run ()
spendOnComparison at a b
  | stringlike a && stringlike b = spendOnScan at a >> spendOnScan at b
  | otherwise = spendOnReading at a >> spendOnReading at b
  where
    stringlike v = case v of
      VStr _ -> True
      VArr _ -> True
      VObj _ -> True
      _ -> False

-- ---------------------------------------------------------------------
-- Compiling and running
-- ---------------------------------------------------------------------

-- | The datum a node is evaluated against: the value, and the same datum as
-- JSON, which a custom operator is handed. The JSON is built the first time
-- it is asked for and then shared by every node evaluated against this
-- datum, so an operator that reads one field of a large datum does not
-- rebuild the whole of it on every call. At the root it is the input itself.
data Env = Env
  { envValue :: !Val
  , envJson :: Aeson.Value
  }

-- | A datum that came from evaluation: its JSON is built when needed.
mkEnv :: Val -> Env
mkEnv value = Env value (toValue value)

-- | A rule compiled to a function of the data.
type Eval = Env -> Run Val

-- | A compiled node: its code, and its value if it is a literal (so that a
-- parent can do work ahead of time, such as splitting a @var@ path once).
-- Strict, so that compiling a rule builds all of it, not just its root.
data Compiled = Compiled
  { compiledLiteral :: !(Maybe Val)
  , compiledEval :: !Eval
  }

-- | A compiled rule, with the configuration it was compiled with.
data Program = Program !Config !Eval

-- | Compile a rule once: convert its literals, resolve its custom operators
-- against the registry, and build each node's code. All of the work is done
-- when the result is first demanded, not spread over the first run.
compileRule :: Config -> Operators -> Rule -> Program
compileRule config operators rule = case foldFix (compileNode config operators) rule of
  Compiled _ eval -> Program config eval

-- | Run a compiled rule against some data, with the configured step budget.
runProgram :: Program -> Aeson.Value -> Either (Location, EvalError) Aeson.Value
runProgram (Program config eval) input =
  case unRun (eval (Env (fromValue input) input)) (maxSteps config) of
    Failed location e -> Left (location, e)
    Done result _ -> Right (toValue result)

-- | One layer of the fold: given each child already compiled, compile this
-- node. Evaluating a node costs one step, before its own work.
compileNode :: Config -> Operators -> Node Compiled -> Compiled
compileNode config operators (Node location node) =
  -- every child, then this node's own code, is built before the node is
  foldr seq () node `seq` body `seq` Compiled literal (\env -> spend at 1 >> body env)
  where
    at = At config location
    literal = case node of
      Lit value -> Just (fromValue value)
      _ -> Nothing
    body = case node of
      -- the path of a @var@ is almost always a literal: read it once, here
      Var path fallback
        | Just key <- compiledLiteral path ->
            let known = pathOf key
             in variable at (compiledEval path) (compiledEval <$> fallback) (const (pure known))
      _ -> compileBody at operators (fmap compiledEval node)

compileBody :: At -> Operators -> RuleF Eval -> Eval
compileBody at@(At config _) operators node = case node of
  Lit literal ->
    -- converted once, when the rule is compiled, and shared by every run
    let converted = fromValue literal in const (pure converted)
  Arr items ->
    let elements = Vector.fromList items in \env -> VArr <$> Vector.mapM ($ env) elements
  Var path fallback -> variable at path fallback (pure . pathOf)
  Missing keys -> \env -> do
    names <- evalAll keys env
    mapM_ (spendOnConversion at) names
    let (work, absent) = missingKeys (workCap config) names (envValue env)
    spend at work
    pure (VArr (Vector.fromList absent))
  MissingSome needed options -> \env -> do
    count <- needed env
    listed <- options env
    missingSome at count listed (envValue env)
  If branches -> evalIf branches
  And operands -> evalAnd operands
  Or operands -> evalOr operands
  Not operand -> fmap (VBool . not . truthy) . operand
  DoubleNot operand -> fmap (VBool . truthy) . operand
  LooseEq x y -> coercing at (\a b -> VBool (looseEquals a b)) x y
  StrictEq x y -> binary at (\a b -> VBool (strictEquals a b)) x y
  LooseNeq x y -> coercing at (\a b -> VBool (not (looseEquals a b))) x y
  StrictNeq x y -> binary at (\a b -> VBool (not (strictEquals a b))) x y
  Lt x y z -> between at lessThan x y z
  Lte x y z -> between at lessOrEqual x y z
  Gt x y -> coercing at (\a b -> VBool (lessThan b a)) x y
  Gte x y -> coercing at (\a b -> VBool (lessOrEqual b a)) x y
  Max operands -> fmap (VNum . maximumJs . map toNumber) . numeric at operands
  Min operands -> fmap (VNum . minimumJs . map toNumber) . numeric at operands
  Add operands ->
    fmap (VNum . List.foldl' (\total v -> total + parseFloat v) 0) . numeric at operands
  Sub x y -> \env -> do
    a <- x env
    b <- definedOperand <$> traverse ($ env) y
    spendOnReading at a
    mapM_ (spendOnReading at) b
    pure (VNum (maybe (negate (toNumber a)) (\v -> toNumber a - toNumber v) b))
  Mul [] -> const (failAt at (InvalidOperand (builtinOperatorName BMul)))
  Mul (first : rest) -> \env -> do
    -- the reference folds without a seed: a single operand is returned as
    -- it is, not converted to a number
    a <- first env
    bs <- evalAll rest env
    mapM_ (spendOnReading at) (a : bs)
    pure (List.foldl' (\acc v -> VNum (parseFloat acc * parseFloat v)) a bs)
  Div x y -> coercing at (\a b -> VNum (toNumber a / toNumber b)) x y
  Mod x y -> coercing at (\a b -> VNum (remainderJs (toNumber a) (toNumber b))) x y
  Map collection logic ->
    withCollection collection (VArr Vector.empty) $ \items -> do
      guardInput at items
      VArr <$> Vector.mapM (logic . mkEnv) items
  Filter collection logic ->
    withCollection collection (VArr Vector.empty) $ \items -> do
      guardInput at items
      VArr <$> Vector.filterM (fmap truthy . logic . mkEnv) items
  Reduce collection logic initial -> reduceRule collection logic initial
  All collection logic ->
    withCollection collection (VBool False) $ \items ->
      if Vector.null items then pure (VBool False) else VBool <$> allTruthy (logic . mkEnv) (Vector.toList items)
  None collection logic ->
    withCollection collection (VBool True) $ \items ->
      if Vector.null items then pure (VBool True) else VBool . not <$> anyTruthy (logic . mkEnv) (Vector.toList items)
  Some collection logic ->
    withCollection collection (VBool False) $ \items ->
      if Vector.null items then pure (VBool False) else VBool <$> anyTruthy (logic . mkEnv) (Vector.toList items)
  Merge operands -> \env -> do
    values <- evalAll operands env
    let pieces = map flattenOnce values
        size = List.foldl' (\total piece -> total + Vector.length piece) 0 pieces
    when (size > maxArrayLength config) $ exceeded at (ArrayLimit . maxArrayLength)
    spend at size
    pure (VArr (Vector.concat pieces))
  In needle haystack -> scanning at (\a b -> VBool (contains a b)) needle haystack
  Cat operands -> \env -> do
    values <- evalAll operands env
    mapM_ (spendOnConversion at) values
    let pieces = map toJoinText values
    total <- foldM (addLength at) 0 pieces
    spend at total
    pure (VStr (Text.concat pieces))
  Substr source start len -> \env -> do
    text <- source env
    from <- start env
    limit <- definedOperand <$> traverse ($ env) len
    spendOnScan at text
    pure (VStr (substring (toJsString text) from limit))
  Log operand -> operand
  Discard inner extras -> \env -> do
    result <- inner env
    mapM_ ($ env) extras
    pure result
  CustomOp name operands -> case lookupOperator operators name of
    Nothing -> \env -> evalAll operands env >> failAt at (UnknownOperator name)
    Just implementation -> \env -> do
      arguments <- evalAll operands env
      case implementation (envJson env) (map toValue arguments) of
        Left reason -> failAt at (CustomOperatorFailed name reason)
        Right result -> do
          when (nestsDeeperThan (maxDepth config) result) $
            exceeded at (ValueDepthLimit . maxDepth)
          spend at (jsonWeightUpTo (saturatingSucc (maxSteps config)) result)
          pure (fromValue result)

-- | @var@: evaluate the path (for its step and its failures), turn its value
-- into a 'Path' (ahead of time, when it is a literal), and look it up.
variable :: At -> Eval -> Maybe Eval -> (Val -> Run Path) -> Eval
variable at@(At config _) path fallback toPath env = do
  key <- path env >>= toPath
  notFound <- maybe (pure VNull) ($ env) fallback
  let (work, found) = lookupPath (workCap config) (envValue env) key notFound
  spend at work
  pure found

-- ---------------------------------------------------------------------
-- Combinators over compiled operands
-- ---------------------------------------------------------------------

evalAll :: [Eval] -> Env -> Run [Val]
evalAll operands env = traverse ($ env) operands

-- | Two operands the combiner compares without converting: no cost for an
-- array (it is never walked), and a string costs by its length.
binary :: At -> (Val -> Val -> Val) -> Eval -> Eval -> Eval
binary at combine x y env = do
  a <- x env
  b <- y env
  spendOnString at a
  spendOnString at b
  pure (combine a b)

-- | Two operands that the combiner may convert to a string or number, or
-- compare or search ('spendOnComparison').
coercing :: At -> (Val -> Val -> Val) -> Eval -> Eval -> Eval
coercing at combine x y env = do
  a <- x env
  b <- y env
  spendOnComparison at a b
  pure (combine a b)

-- | Two operands the combiner scans, a string by its length and an array by
-- its elements.
scanning :: At -> (Val -> Val -> Val) -> Eval -> Eval -> Eval
scanning at combine x y env = do
  a <- x env
  b <- y env
  spendOnScan at a
  spendOnScan at b
  pure (combine a b)

-- | Operands about to be converted to numbers, charged for the conversion.
numeric :: At -> [Eval] -> Env -> Run [Val]
numeric at operands env = do
  values <- evalAll operands env
  mapM_ (spendOnReading at) values
  pure values

-- | @x op y@, or the chained @x op y && y op z@ when a third operand is
-- given. All operands are evaluated first, as in the reference.
between :: At -> (Val -> Val -> Bool) -> Eval -> Eval -> Maybe Eval -> Eval
between at holds x y z env = do
  a <- x env
  b <- y env
  c <- definedOperand <$> traverse ($ env) z
  spendOnComparison at a b
  mapM_ (spendOnComparison at b) c
  pure (VBool (maybe (holds a b) (\third -> holds a b && holds b third) c))

-- | Condition/consequent pairs, then an optional else; no branch means null.
--
-- Hand-written recursion (Standard 4.1): a search that stops at the first
-- condition that holds, in 'Run'. It is a right fold with early exit, and
-- @base@ has no monadic combinator for that shape.
evalIf :: [Eval] -> Eval
evalIf (condition : consequent : rest) env = do
  decided <- condition env
  if truthy decided then consequent env else evalIf rest env
evalIf [fallback] env = fallback env
evalIf [] _ = pure VNull

-- | The first falsy operand, or the last; JavaScript's @undefined@ (not
-- null) when there are none.
--
-- Hand-written recursion (Standard 4.1): a right fold with early exit in
-- 'Run'; @base@ has no monadic combinator for that shape.
evalAnd :: [Eval] -> Eval
evalAnd [] _ = pure VUndefined
evalAnd [only] env = only env
evalAnd (first : rest) env = do
  value <- first env
  if truthy value then evalAnd rest env else pure value

-- | The first truthy operand, or the last; JavaScript's @undefined@ (not
-- null) when there are none.
--
-- Hand-written recursion (Standard 4.1): the dual of 'evalAnd'.
evalOr :: [Eval] -> Eval
evalOr [] _ = pure VUndefined
evalOr [only] env = only env
evalOr (first : rest) env = do
  value <- first env
  if truthy value then pure value else evalOr rest env

-- | Evaluate a collection operand; run @onItems@ if it is an array,
-- otherwise answer @fallback@.
withCollection :: Eval -> Val -> (Vector Val -> Run Val) -> Eval
withCollection collection fallback onItems env = do
  evaluated <- collection env
  case evaluated of
    VArr items -> onItems items
    _ -> pure fallback

-- | Refuse to iterate an array longer than the array limit, before doing
-- any of the work.
guardInput :: At -> Vector Val -> Run ()
guardInput at@(At config _) items =
  when (Vector.length items > maxArrayLength config) $ exceeded at (ArrayLimit . maxArrayLength)

-- | Add a piece's length (in UTF-16 code units) to a running total,
-- failing as soon as the total passes the string limit.
addLength :: At -> Int -> Text -> Run Int
addLength at@(At config _) total piece
  | total' > maxStringLength config = exceeded at (StringLimit . maxStringLength)
  | otherwise = pure total'
  where
    total' = total + utf16Length piece

-- | Fold the array with the sub-rule, evaluated against an object holding
-- the current element and the accumulator.
reduceRule :: Eval -> Eval -> Eval -> Eval
reduceRule collection logic initial env = do
  evaluated <- collection env
  start <- initial env
  case evaluated of
    VArr items -> Vector.foldM' step start items
    _ -> pure start
  where
    step accumulator item =
      logic (mkEnv (VObj (KeyMap.fromList [("current", item), ("accumulator", accumulator)])))

-- | Does the sub-rule hold for every item? Stops at the first that does not.
--
-- Hand-written recursion (Standard 4.1): a monadic @all@, which @base@ does
-- not provide, in 'Run'.
allTruthy :: (Val -> Run Val) -> [Val] -> Run Bool
allTruthy _ [] = pure True
allTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then allTruthy logic rest else pure False

-- | Does the sub-rule hold for some item? Stops at the first that does.
--
-- Hand-written recursion (Standard 4.1): a monadic @any@, the dual of
-- 'allTruthy'.
anyTruthy :: (Val -> Run Val) -> [Val] -> Run Bool
anyTruthy _ [] = pure False
anyTruthy logic (item : rest) = do
  value <- logic item
  if truthy value then pure True else anyTruthy logic rest

-- ---------------------------------------------------------------------
-- Operator semantics
-- ---------------------------------------------------------------------

-- | @merge@ flattens one level; anything that is not an array is one
-- element.
flattenOnce :: Val -> Vector Val
flattenOnce (VArr items) = items
flattenOnce other = Vector.singleton other

-- | @in@: substring of a string, or strict-equal member of an array.
-- A falsy container (including the empty string) contains nothing.
contains :: Val -> Val -> Bool
contains _ (VStr s) | Text.null s = False
contains needle (VStr s) = Text.isInfixOf (toJsString needle) s
contains needle (VArr items) = Vector.any (strictEquals needle) items
contains _ _ = False

-- | Which of the keys have no value, or an empty-string one. If the first
-- argument is itself an array, it is the list of keys.
--
-- The first component is the work the lookups did, in steps (see
-- 'lookupVariable'); the cap bounds how far any one lookup is followed.
missingKeys :: Int -> [Val] -> Val -> (Int, [Val])
missingKeys cap args dat =
  ( List.foldl' (+) 0 [work | (_, work, _) <- probes]
  , [key | (key, _, True) <- probes]
  )
  where
    keys = case args of
      VArr listed : _ -> Vector.toList listed
      _ -> args
    probes = map probe keys
    probe key =
      let (work, found) = lookupKey key
       in (key, work, isMissing found)
    isMissing found = case found of
      VNull -> True
      VStr s -> Text.null s
      _ -> False
    -- The reference looks each key up with @{"var": key}@, so a key that is
    -- itself an array is a @var@'s argument list: path, then default.
    lookupKey key = case key of
      VArr listed -> case Vector.toList listed of
        [] -> (0, dat)
        path : rest -> lookupVariable cap dat path (case rest of fallback : _ -> fallback; [] -> VNull)
      _ -> lookupVariable cap dat key VNull

-- | @missing_some@: nothing is missing if at least @count@ of the options
-- are present; otherwise the missing ones.
--
-- The reference reads @options.length@, so a @null@ or undefined option
-- list throws; a string counts its code units; anything without a length
-- compares as @NaN@, which is never enough, so the missing ones are
-- returned.
missingSome :: At -> Val -> Val -> Val -> Run Val
missingSome at@(At config _) count options env = case options of
  VNull -> failAt at (InvalidOperand (builtinOperatorName BMissingSome))
  VUndefined -> failAt at (InvalidOperand (builtinOperatorName BMissingSome))
  _ -> do
    spendOnConversion at options
    let names = case options of
          VArr items -> Vector.toList items
          other -> [other]
        (work, absent) = missingKeys (workCap config) names env
    spend at work
    let have = case optionLength of
          Just n -> n - fromIntegral (length absent)
          Nothing -> 0 / 0
    pure (if lessOrEqual count (VNum have) then VArr Vector.empty else VArr (Vector.fromList absent))
  where
    optionLength = case options of
      VArr items -> Just (fromIntegral (Vector.length items))
      VStr s -> Just (fromIntegral (utf16Length s))
      VObj fields -> toNumber <$> KeyMap.lookup "length" fields
      _ -> Nothing

-- | The most work a lookup is followed for: one more than the whole budget,
-- so a lookup that cannot be paid for is cut short rather than completed.
workCap :: Config -> Int
workCap config = saturatingSucc (maxSteps config)

-- | A @var@ path, read: the data itself, or the property names to follow.
data Path = WholeData | Steps [Text]

-- | Read a @var@ path the way the reference does: an empty or null path is
-- the data itself; anything else is converted to a string and split at dots.
pathOf :: Val -> Path
pathOf VNull = WholeData
pathOf VUndefined = WholeData
pathOf (VStr s) | Text.null s = WholeData
pathOf key = Steps (Text.splitOn "." (toJsString key))

-- | @var@: look a path up in the data; a missing step gives the fallback.
lookupVariable :: Int -> Val -> Val -> Val -> (Int, Val)
lookupVariable cap dat = lookupPath cap dat . pathOf

-- | Follow a path. The first component is the work the walk did, in steps:
-- one per code unit of a string that is indexed or measured (see
-- 'property'). The caller charges it. It is counted only up to @cap@.
lookupPath :: Int -> Val -> Path -> Val -> (Int, Val)
lookupPath _ dat WholeData _ = (0, dat)
lookupPath cap dat (Steps keys) notFound = (work, fromMaybe (nullIfUndefined notFound) found)
  where
    (work, found) = descend cap dat keys

-- | An optional operand that evaluates to undefined counts as absent: the
-- reference tests the /value/ (@c === undefined@), not how many
-- arguments were written. So @{"-": [x, y]}@ negates @x@ when @y@ is
-- undefined, @<@ and @<=@ drop an undefined third operand, and @substr@
-- ignores an undefined end.
definedOperand :: Maybe Val -> Maybe Val
definedOperand (Just VUndefined) = Nothing
definedOperand other = other

-- | The reference's fallback for a missing variable: an undefined
-- fallback becomes null.
nullIfUndefined :: Val -> Val
nullIfUndefined VUndefined = VNull
nullIfUndefined value = value

-- | Follow the path one key at a time; 'Nothing' is "no such property".
-- The first component is the work done, summed over the keys and given up
-- as soon as it passes @cap@ (the caller then fails the evaluation).
descend :: Int -> Val -> [Text] -> (Int, Maybe Val)
descend cap = go 0
  where
    go spent current keys
      | spent > cap = (spent, Nothing)
      | otherwise = case keys of
          [] -> (spent, Just current)
          key : rest -> case current of
            VNull -> (spent, Nothing)
            VUndefined -> (spent, Nothing)
            _ ->
              let (work, found) = property cap current key
                  spent' = spent + work
               in case found of
                    Nothing -> (spent', Nothing)
                    -- a property that is itself undefined counts as missing,
                    -- as in the reference
                    Just VUndefined -> (spent', Nothing)
                    Just next -> go spent' next rest

-- | JavaScript property access on a JSON value: object keys, array and
-- string indices, and @length@. Only a value's own properties are seen:
-- the reference would also find names inherited through the prototype
-- chain (@constructor@, @toString@, ...), which JSON data does not have
-- (docs/COMPATIBILITY.md, DEV-INHERITED).
--
-- The first component is the work, in steps: object and array access is
-- constant time and free; indexing or measuring a string walks it, and costs
-- its length in code points, counted up to @cap@.
property :: Int -> Val -> Text -> (Int, Maybe Val)
property _ (VObj fields) key = (0, KeyMap.lookup (Key.fromText key) fields)
property _ (VArr items) key
  | key == "length" = (0, Just (VNum (fromIntegral (Vector.length items))))
  | otherwise = (0, arrayIndex key >>= (items Vector.!?))
property cap (VStr s) key
  | key == "length" = (size, Just (VNum (fromIntegral (utf16Length s))))
  | otherwise = case arrayIndex key of
      Nothing -> (0, Nothing)
      Just i -> (size, VStr <$> utf16CharAt i s)
  where
    size = min cap (Text.length s)
property _ _ _ = (0, Nothing)

-- | A canonical array index: digits, no leading zero, below 10^9.
arrayIndex :: Text -> Maybe Int
arrayIndex key
  | Text.null key || Text.length key > 9 = Nothing
  | not (Text.all isDigit key) = Nothing
  | Text.length key > 1 && "0" `Text.isPrefixOf` key = Nothing
  | otherwise = Just (Text.foldl' (\acc c -> acc * 10 + digitToInt c) 0 key)

-- | @substr@: JavaScript's @substr@, plus the reference's PHP-style
-- twist that a negative third argument means "stop that many characters
-- before the end" rather than a negative length.
substring :: Text -> Val -> Maybe Val -> Text
substring source start end = case end of
  Just limit | lessThan limit (VNum 0) -> negativeEnd limit
  _ -> substrJs source (toNumber start) (fmap toNumber end)
  where
    tailText = substrJs source (toNumber start) Nothing
    negativeEnd (VNum n) =
      substrJs tailText 0 (Just (fromIntegral (utf16Length tailText) + n))
    negativeEnd _ = ""

-- | @String.prototype.substr(start, length)@, in UTF-16 code units.
substrJs :: Text -> Double -> Maybe Double -> Text
substrJs source start len = utf16Slice from count source
  where
    size = utf16Length source
    relative = toClampedInt (negate size, size) start
    from = if relative < 0 then size + relative else relative
    available = size - from
    count = maybe available (toClampedInt (0, available)) len

-- | Truncate toward zero, clamped into the inclusive range; @NaN@ is zero.
toClampedInt :: (Int, Int) -> Double -> Int
toClampedInt (low, high) d
  | isNaN d = max low (min high 0)
  | otherwise = truncate (max (fromIntegral low) (min (fromIntegral high) d))
