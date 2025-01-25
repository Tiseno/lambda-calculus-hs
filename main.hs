{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use lambda-case" #-}
import           Control.Applicative      (Alternative (..))
import qualified Control.Monad            as Monad
import           Control.Monad.IO.Class   (MonadIO)
import qualified Control.Monad.IO.Class   as MonadIO
import           Control.Monad.State      (State)
import qualified Control.Monad.State      as State
import qualified Data.Char                as Char
import qualified Data.List                as List
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.Maybe               as Maybe
import qualified Data.Set                 as Set
import qualified System.Console.Haskeline as HL
import qualified System.Environment       as Environment
import qualified System.IO                as SystemIO

data Var = Text String | Number Int deriving (Eq, Ord)

instance Show Var where
  show :: Var -> String
  show (Text s)   = s
  show (Number i) = "#" ++ show i

data Term = Var Var | Abs Var Term | App Term Term deriving (Eq)

instance Show Term where
  show :: Term -> String
  show (Var var) = show var
  show (Abs x m) = "(λ" ++ show x ++ "." ++ show m ++ ")"
  show (App m n) = "(" ++ show m ++ " " ++ show n ++ ")"

data Expr = Let Var Term | Eval Term

instance Show Expr where
  show :: Expr -> String
  show (Let var term) = show var ++ " = " ++ show term
  show (Eval term)    = show term

data Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> do
    (s', a) <- runParser p s
    pure (s', f a)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (s, a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \s -> do
    (s', r1) <- runParser p1 s
    (s'', r2) <- runParser p2 s'
    pure (s'', r1 r2)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \s -> do
    case runParser p1 s of
      Nothing -> runParser p2 s
      r       -> r

parsePred p = Parser $ \s ->
  case s of
    (a:s') | p a -> Just (s', a)
    _            -> Nothing

parseWord = some (parsePred Char.isAlpha)
parseInt = read <$> some (parsePred Char.isDigit)
parseSingle c = parsePred (== c)

parseText = Text <$> parseWord
parseNumber = parseSingle '#' *> (Number <$> parseInt)
parseVar = parseText <|> parseNumber

parseSpace = many $ parsePred Char.isSpace

parseParenthesized p = parseSingle '(' *> parseSpace *> p <* parseSpace <* parseSingle ')'

parseVarTerm = Var <$> parseVar
parseAbsTerm = parseParenthesized (Abs <$> ((parseSingle 'λ' <|> parseSingle '\\' <|> parseSingle '/') *> parseSpace *> parseVar) <*> (parseSpace *> parseSingle '.' *> parseSpace *> parseTerm))
parseAppTerm = parseParenthesized (App <$> parseTerm <*> (parseSpace *> parseTerm))
parseTerm = parseVarTerm <|> parseAbsTerm <|> parseAppTerm

parseTermOrDie :: String -> Term
parseTermOrDie s = case runParser parseTerm s of
  Just ([], e) -> e

parseExprLet = Let <$> parseVar <*> (parseSpace *> parseSingle '=' *> parseSpace *> parseTerm)
parseExprEval = Eval <$> parseTerm
parseExpr = parseExprLet <|> parseExprEval

parseFullExpr :: String -> Either String Expr
parseFullExpr s =
  let input = (unwords $ lines s) in
  case runParser (parseSpace *> (parseExpr <* parseSpace)) input of
    Just ([], t)   -> Right t
    Just (left, _) -> Left $ "Could not parse whole input\n\n" ++ input ++ "\n" ++ replicate (length input - length left) ' ' ++ "^" ++ "Stopped here"
    _              -> Left "Could not parse expression"

freeVariables :: Term -> Set.Set Var
freeVariables = fv Set.empty
  where
    fv :: Set.Set Var -> Term -> Set.Set Var
    fv bound (Var var)
      | var `Set.member` bound = Set.empty
      | otherwise = Set.singleton var
    fv bound ((Abs x m)) = fv (Set.insert x bound) m
    fv bound ((App m n)) = fv bound m `Set.union` fv bound n

rename :: Term -> Var -> Var -> Term
rename t@(Var var) from to
  | var == from = Var to
  | otherwise = t
rename t@((Abs x m)) from to
  | x == from = t
  | otherwise = Abs x (rename m from to)
rename ((App m n)) from to =
  App (rename m from to) (rename n from to)

newVar :: State Int Var
newVar = do
  n <- State.get
  State.put $ n + 1
  pure $ Number n

renameBound :: Set.Set Var -> Term -> State Int Term
renameBound free t@(Var _) = pure t
renameBound free ((Abs x m)) = do
  (x', m') <- if x `Set.member` free
      then do
        to <- newVar
        pure (to, rename m x to)
      else pure (x, m)
  m'' <- renameBound free m'
  pure $ Abs x' m''
renameBound free t@(App _ _) = pure t

replaceVarWithTerm :: Term -> Var -> Term -> Term
replaceVarWithTerm t@(Var var) from to
  | var == from = to
  | otherwise = t
replaceVarWithTerm t@((Abs x m)) from to
  | x == from = t
  | otherwise = Abs x $ replaceVarWithTerm m from to
replaceVarWithTerm ((App m n)) from to =
  App (replaceVarWithTerm m from to) (replaceVarWithTerm n from to)

betaReduction :: (Var, Term) -> Term -> State Int Term
betaReduction (x, m) n = do
  let free = freeVariables n
  m' <- renameBound free m
  pure $ replaceVarWithTerm m' x n

reduce :: Term -> State Int Term
reduce (Var var) = pure $ Var var
reduce (Abs x m) = do
  m' <- reduce m
  pure $ Abs x m'
reduce (App (Abs x m) n) = betaReduction (x, m) n
reduce (App m n) = do
  m' <- reduce m
  if m' /= m
     then pure $ App m' n
     else do
       n' <- reduce n
       pure $ App m n'

reduceIO :: MonadIO m => Maybe Int -> Term -> m Term
reduceIO maxDepth t = reduceIO' 0 (t, 1)
  where
    reduceIO' n (t, i) = do
      let (t', i') = State.runState (reduce t) i
       in if t' == t || maybe False (n >=) maxDepth
         then pure t
         else do
           MonadIO.liftIO $ print t
           reduceIO' (n + 1) (t', i')

replaceAllGlobals :: Map Var Term -> Term -> Term
replaceAllGlobals globalVars t@(Var var) =
  Maybe.fromMaybe t $ Map.lookup var globalVars
replaceAllGlobals globalVars (Abs x m)
  | x `Map.member` globalVars = Abs x $ replaceAllGlobals (Map.delete x globalVars) m
  | otherwise = Abs x $ replaceAllGlobals globalVars m
replaceAllGlobals globalVars (App m n) =
  App (replaceAllGlobals globalVars m) (replaceAllGlobals globalVars n)

evalString :: MonadIO m => Map Var Term -> Maybe Int -> String -> m (Map Var Term)
evalString globalVars maxDepth s =
  case parseFullExpr s of
    Left e  -> do
      MonadIO.liftIO $ putStrLn $ "Error: " ++ e
      pure globalVars
    Right (Eval t) -> do
      t' <- reduceIO maxDepth (replaceAllGlobals globalVars t)
      MonadIO.liftIO $ print t'
      pure globalVars
    Right (Let var t) -> do
      t' <- reduceIO maxDepth (replaceAllGlobals globalVars t)
      let free = freeVariables t'
      if null free
          then do
            MonadIO.liftIO $ print (Let var t')
            pure $ Map.insert var t' globalVars
          else do
            MonadIO.liftIO $ putStrLn $ "Error: Cannot bind global variable to term with free variables: " ++ List.intercalate ", " (fmap show (Set.toList free))
            pure globalVars

exampleHelp :: String
exampleHelp =
  "\n\
  \  A lambda expression is on one of the following forms:\n\
  \\n\
  \  1. x       A variable.\n\
  \  2. (λx.M)  A lambda abstraction (function definition), taking a parameter x, returning the lambda expression M.\n\
  \  3. (M N)   An application of an expression M to the expression N.\n\
  \\n\
  \  \\ or / can be used instead of λ\n\
  \\n\
  \  An expression is evaluated by repeated β-reduction (function application) until it is irreducible.\n\
  \\n\
  \Example\n\
  \  > ((\\z.(z (o i))) (\\g.g))\n\
  \  ((λz.(z (o i))) (λg.g))\n\
  \  ((λg.g) (o i))\n\
  \  (o i)\n\
  \\n\
  \  The following variables are bound globally: " ++ List.intercalate ", " (fmap (show . fst) builtin) ++ ".\n" ++ "\
  \  Disjunction and multiplaction is left out as an exercise to the reader.\n\
  \\n\
  \  A global name can be bound with NAME = M and cannot contain any free variables.\n\
  \\n\
  \Example\n\
  \  > ONE = (SUCC ZERO)\n\
  \  ((λn.(λf.(λx.(f ((n f) x))))) (λf.(λx.x)))\n\
  \  (λf.(λx.(f (((λf.(λx.x)) f) x))))\n\
  \  (λf.(λx.(f ((λx.x) x))))\n\
  \  ONE = (λf.(λx.(f x)))\n\
  \  > TWO = ((ADD ONE) ONE)\n\
  \  (((λm.(λn.(λf.(λx.((m f) ((n f) x)))))) (λf.(λx.(f x)))) (λf.(λx.(f x))))\n\
  \  ((λn.(λf.(λx.(((λf.(λx.(f x))) f) ((n f) x))))) (λf.(λx.(f x))))\n\
  \  (λf.(λx.(((λf.(λx.(f x))) f) (((λf.(λx.(f x))) f) x))))\n\
  \  (λf.(λx.((λx.(f x)) (((λf.(λx.(f x))) f) x))))\n\
  \  (λf.(λx.(f (((λf.(λx.(f x))) f) x))))\n\
  \  (λf.(λx.(f ((λx.(f x)) x))))\n\
  \  TWO = (λf.(λx.(f (f x))))\n\
  \"

repl :: Map Var Term -> HL.InputT IO ()
repl globalVars = do
  input <- HL.handleInterrupt (pure Nothing) (HL.getInputLine "> ")
  case input of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just ":h" -> do
      HL.outputStrLn exampleHelp
      repl globalVars
    Just [] -> repl globalVars
    Just s -> do
      globalVars' <- evalString globalVars Nothing s
      repl globalVars'

cliHelp :: String
cliHelp =
  "Evaluator of lambda expressions.\n\
  \\n\
  \Usage  lc              Enter into a repl.\n\
  \       lc [EXPR]       Evaluate a lambda expression.\n\
  \       ... | lc        Evaluate a lambda expression from stdin.\n\
  \       lc -y           Evaluate the y combinator applied to g:\n\
  \                         ((λf.((λx.(f (x x))) (λx.(f (x x))))) g)\n\
  \                       and exit after 20 reductions.\n\
  \\n" ++ exampleHelp

builtin :: [(Var, Term)]
builtin =
  [ (Text "ID", parseTermOrDie "(λx.x)")
  , (Text "CONST", parseTermOrDie "(λx.(λy.x))")
  , (Text "TRUE", parseTermOrDie "(λt.(λf.t))")
  , (Text "FALSE", parseTermOrDie "(λt.(λf.f))")
  , (Text "AND", parseTermOrDie "(λp.(λq.((p q) (λt.(λf.f)))))")
  , (Text "ZERO", parseTermOrDie "(λf.(λx.x))")
  , (Text "SUCC", parseTermOrDie "(λn.(λf.(λx.(f ((n f) x)))))")
  , (Text "ADD", parseTermOrDie "(λm.(λn.(λf.(λx.((m f) ((n f) x))))))")
  , (Text "Y", parseTermOrDie "(λf.((λx.(f (x x))) (λx.(f (x x)))))")
  ]

builtinMap :: Map Var Term
builtinMap = Map.fromList builtin

main = do
  isInputTerminal <- SystemIO.hIsTerminalDevice SystemIO.stdin
  args <- Environment.getArgs
  case () of
    _ | "-h" `elem` args || "-help" `elem` args || "--help" `elem` args -> putStr cliHelp
    _ | "-y" `elem` args -> Monad.void $ evalString Map.empty (Just 20) "((λf.((λx.(f (x x))) (λx.(f (x x))))) g)"
    _ | isInputTerminal && null args -> if null args
          then do
            putStrLn "Entering repl. :h for help, :q to exit"
            HL.runInputT HL.defaultSettings $ HL.withInterrupt (repl builtinMap)
          else Monad.void $ evalString Map.empty Nothing (unwords args)
    _ -> do
      s <- SystemIO.getContents
      Monad.void $ evalString Map.empty Nothing s

