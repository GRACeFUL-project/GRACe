{-# LANGUAGE LambdaCase #-}

-- | Parser for the minizinc output.
-- Unsat not parsed into anything useful at the moment, the message
-- is returned as a string.
-- Assumes variables/labels outputted as
--    label or name : value
-- where value is either a boolean, integer or decimal number.
-- Example:
--    output ["pump cap : ", show(cap), "\noverflow : ", show(of)];
module OutParser (module OutParser) where

import System.Process

import Text.Parsec
import Text.ParserCombinators.Parsec.Number

type Parser a = Parsec String () a

data Output
  = Sat [Solution]
    -- ^ @[Solution]@ contains all reported solutions,
  | Unsat String
    -- ^ The unsat message.
  | ParseErr String
    -- ^ Parsec error message.
  deriving Eq

-- | Show instance to match the parsing.
--
-- show . par == id
instance Show Output where
  show = \case
    Sat sols -> concatMap show sols
    Unsat msg -> msg
    ParseErr msg -> msg

-- | A solution with variables and their value assignments.
data Solution
  = Sol [(String, Value)]
    -- ^ Non-optimal solution
  | OptSol [(String, Value)]
    -- ^ Optimal solution
  deriving Eq

instance Show Solution where
  show = \case
      Sol vars -> showVars vars
      OptSol vars -> showVars vars ++ "==========\n"
    where
      showVars vs = unlines $ map showVar vs ++ ["----------"]
      showVar (lbl, val) = lbl ++ " : " ++ show val

-- | Currently supported MiniZinc data types.
data Value
  = B Bool
  | N Int
  | D Double

-- | Eq instance allow for some rounding errors between parsing and showing.
--
-- @eps = 1e-13@
instance Eq Value where
  B b1 == B b2 = b1 == b2
  N n1 == N n2 = n1 == n2
  D d1 == D d2 = let eps = 1e-13 in abs (d1 -d2) < eps
  _ == _ = False

-- | Show a parsed value.
instance Show Value where
  show = \case
    B b -> if b then "true" else "false"
    N n -> show n
    D d -> show d

-- | Parse a boolean @true@ or @false@.
bool :: Parser Value
bool = B <$> try (true <|> false)
  where
    true = True <$ string "true"
    false = False <$ string "false"

-- | Parse a signed integer.
integ :: Parser Value
integ = N <$> try int

-- | Parse a signed decimal value.
sigDbl :: Parser Value
sigDbl = D <$> try (sign <*> floating)

-- | Parse a line with a label and its value.
var :: Parser (String, Value)
var = do
  l <- manyTill (alphaNum <|> char ' ') (try (string " : "))
  v <- bool <|> integ <|> sigDbl
  return (l, v)

-- | Parse a solution.
--
-- Parsing
--
-- @
-- a : 3
-- b : true
-- ----------
-- @
--
-- results in
--
-- > Sat [Sol [("a", N 3), ("b", B True)]]
solution :: Parser Solution
solution = do
  vars <- var `sepEndBy1` newline
  string "----------"
  sol <- option Sol (OptSol <$ string "==========")
  return $ sol vars

-- | Top level parser.
--
-- Parses all solutions produced.
--
-- @
-- a : true
-- b : 1
-- ----------
-- a : False
-- b : 2
-- ----------
-- ==========
-- @
--
-- results in
--
-- @
-- Sat [ Sol [("a", B True), ("b", N 1)]
--     , OptSol [("a", B False), ("b", N 2)]
--     ]
-- @
output :: Parser Output
output = try sat <|> unsat <|> parseErr
  where
    sat = Sat <$> solution `sepEndBy1` newline
    unsat = Unsat <$> manyTill anyToken eof
    parseErr = undefined

-- | Run the top level parser.
--
-- @Parsec@ parse errors are wrapped in @ParseErr@.
par :: String -> Output
par s = case parse output "" s of
  Left err -> ParseErr $ show err
  Right out -> out

-- | Extracts the labels printed in the solutions.
--
-- Only looks at the first solution.
lbls :: Output -> [String]
lbls = \case
  Sat (Sol vars:_) -> map fst vars
  Sat (OptSol vars:_) -> map fst vars
  _ -> []

type Args = String

-- | @mzn-gecode@ default arguments
--
-- > -p 4 -n 10
defArgs :: Args
defArgs = "-p 4 -n 10"

-- | Run @mzn-gecode@ on the given input string, arguments and parse the result.
--
-- Creates a temporary file called @tmp.mzn@,
-- if it exists it will be overwritten.
run :: Args -> String -> IO Output
run args s = do
  let tmp = "tmp.mzn"
  writeFile tmp s
  out <- readProcess "mzn-gecode" (tmp:words args) ""
  callProcess "rm" [tmp]
  return (par out)

-- | Run @mzn-gecode@ on the given input string, default arguments
-- and parse the result.
--
-- Creates a temporary file called @tmp.mzn@,
-- if it exists it will be overwritten.
runDef :: String -> IO Output
runDef = run defArgs

-- | Run @mzn-gecode@ on the given file path and parse the result.
runFile :: Args -> FilePath -> IO Output
runFile args file = do
  out <- readProcess "mzn-gecode" (file:words args) ""
  return (par out)

-- | Run @mzn-gecode@ on the given file path, default arguments
-- and parse the result.
runFileDef :: FilePath -> IO Output
runFileDef = runFile defArgs
