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
import qualified Data.Set                 as Set
import qualified System.Environment       as Environment
import qualified System.IO                as SystemIO

import qualified System.Console.Haskeline as HL

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

parseVarTerm = Var <$> parseVar
parseAbsBody = Abs <$> ((parseSingle 'λ' <|> parseSingle '\\' <|> parseSingle '/') *> parseSpace *> parseVar) <*> (parseSpace *> parseSingle '.' *> parseSpace *> parseTerm)
parseAppBody = App <$> parseTerm <*> (parseSpace *> parseTerm)
parseAbsOrApp = parseSingle '(' *> parseSpace *> (parseAbsBody <|> parseAppBody) <* parseSpace <* parseSingle ')'
parseTerm = parseSpace *> (parseVarTerm <|> parseAbsOrApp)

parseFullTerm :: String -> Either String Term
parseFullTerm s =
  case runParser (parseTerm <* parseSpace) s of
    Just ([], t)   -> Right t
    Just (left, _) -> Left $ "Could not parse whole input\n\n" ++ s ++ "\n" ++ replicate (length s - length left) ' ' ++ "^" ++ "Stopped here"
    _              -> Left "Could not parse term"

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
renameBound free t@((Abs x m)) = do
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
reduce t@(Var _) = pure t
reduce t@((Abs x m)) = do
  m' <- reduce m
  pure $ Abs x m'
reduce t@((App (Abs x m) n)) = do
  n' <- reduce n
  betaReduction (x, m) n'
reduce t@((App m n)) = do
  n' <- reduce n
  m' <- reduce m
  pure (App m' n')

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

eval :: MonadIO m => Maybe Int -> String -> m ()
eval maxDepth s =
  case parseFullTerm s of
    Left e  -> MonadIO.liftIO $ putStrLn $ "Error: " ++ e
    Right e -> do
      t <- reduceIO maxDepth e
      MonadIO.liftIO $ print t

exampleHelp :: String
exampleHelp =
  "\n\
  \  A lambda expression is on one of the following forms:\n\
  \\n\
  \  1. x       A variable\n\
  \  2. (λx.M)  A lambda abstraction (function definition), taking a parameter x, returning the lambda expression M\n\
  \  3. (M N)   An application of an expression M to the expression N\n\
  \\n\
  \  \\ or / can be used instead of λ\n\
  \\n\
  \  An expression is evaluated by repeated β-reduction (function application) until it is irreducible\n\
  \\n\
  \Example\n\
  \  > ((\\z.(z (o i))) (\\g.g))\n\
  \  ((λz.(z (o i))) (λg.g))\n\
  \  ((λg.g) (o i))\n\
  \  (o i)\n"

repl :: HL.InputT IO ()
repl = do
  input <- HL.handleInterrupt (pure Nothing) (HL.getInputLine "> ")
  case input of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just ":h" -> do
      HL.outputStrLn exampleHelp
      repl
    Just [] -> repl
    Just s -> do
      eval Nothing s
      repl

cliHelp :: String
cliHelp =
  "Evaluator of lambda calculus expressions\n\
  \\n\
  \Usage  lc              Enter into a repl\n\
  \       lc [EXPR]       Evaluate a lambda expression\n\
  \       ... | lc        Evaluate a lambda expression from stdin\n\
  \       lc -y           Evaluate the y combinator applied to g: ((λf.((λx.(f (x x))) (λx.(f (x x))))) g)\n\
  \                       and exit after 20 reductions\n\
  \\n" ++ exampleHelp

main = do
  isInputTerminal <- SystemIO.hIsTerminalDevice SystemIO.stdin
  args <- Environment.getArgs
  case () of
    _ | "-h" `elem` args || "-help" `elem` args || "--help" `elem` args -> putStr cliHelp
    _ | "-y" `elem` args -> eval (Just 20) "((λf.((λx.(f (x x))) (λx.(f (x x))))) g)"
    _ | isInputTerminal && null args -> if null args
          then do
            putStrLn "Entering repl. :h for help, :q to exit"
            HL.runInputT HL.defaultSettings $ HL.withInterrupt repl
          else eval Nothing (unwords args)
    _ -> do
      s <- SystemIO.getContents
      eval Nothing s

