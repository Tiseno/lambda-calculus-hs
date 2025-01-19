{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use lambda-case" #-}
import           Control.Applicative (Alternative (..))
import qualified Control.Monad.State as State
import qualified Data.Char           as Char
import qualified Data.Set            as Set

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
parseAbsBody = Abs <$> (parseSingle 'λ' *> parseSpace *> parseVar) <*> (parseSpace *> parseSingle '.' *> parseSpace *> parseTerm)
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

newVar :: State.State Int Var
newVar = do
  n <- State.get
  State.put $ n + 1
  pure $ Number n

renameBound :: Set.Set Var -> Term -> State.State Int Term
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

betaReduction :: (Var, Term) -> Term -> State.State Int Term
betaReduction (x, m) n = do
  let free = freeVariables n
  m' <- renameBound free m
  pure $ replaceVarWithTerm m' x n

reduce :: Term -> State.State Int Term
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
  if m' == m
     then pure (App m' n')
     else reduce (App m' n')

reduceIO :: Term -> IO Term
reduceIO = reduceIOr 1 0
  where
    reduceIOr :: Int -> Int -> Term -> IO Term
    reduceIOr i n t = do
      let (t', i') = State.runState (reduce t) i
      if t' == t
         then pure t
         else do
           putStrLn $ "  -> " ++ show t'
           reduceIOr i' (n + 1) t'

main :: IO ()
main = case parseFullTerm "((λf.((λx.(f (x x))) (λx.(f (x x))))) g)" of
  Left e -> putStrLn $ "Error: " ++ e
  Right e -> do
    print e
    r <- reduceIO e
    print r
    pure ()
