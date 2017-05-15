{-# LANGUAGE GADTs #-}

-- | A finite domain constraint solver.
module FDSolver
  ( FD
  , Var (..)
  , E (..)
  , reduce
  -- , solve'
  , newVar
  , assert
  ) where

import Data.List
import MonadLib

-- | The FD monad.
type FD a = StateT (FDDB a) Id

data FDDB a = FDDB
  { nextId      :: Int
  , variables   :: [[a]]  -- Vars index into list.
  , constraints :: [E]
  }

-- | Variable.
data Var = Var Int deriving Eq
instance Show Var where show (Var i) = show i

-- | Constraint expressions.
data E where
  (:&&) :: E -> E -> E
  (:||) :: E -> E -> E
  (:->) :: E -> E -> E
  (:==) :: Var -> Var -> E
  (:/=) :: Var -> Var -> E
  (:$=) :: [Var] -> [Var] -> E
  deriving Show

infix  4 :==, :/=, :$=
infixl 3 :&&
infixl 2 :||
infixr 1 :->

-- | Reduce a set of constraints and update a structure with solved domain values.
--   Returns Nothing if overconstrained.
reduce :: Ord a => FD a b -> Maybe (b, Var -> [a])
reduce fd = do
  solvedVars <- applyConstraints (constraints db) (variables db)
  return (b, \ (Var i) -> solvedVars !! i)
  where
  (b, db) = runId $ runStateT (FDDB 0 [] []) fd

{-
solve' :: Ord a => FD a b -> (b, Var -> (Var, [a]), [E])
solve' fd =  (b, \ (Var i) -> (Var i, solvedVars !! i), constraints db)
  where
  (b, db) = runId $ runStateT (FDDB 0 [] []) fd
  solvedVars = applyConstraints (constraints db) $ variables db
  -}

applyConstraints :: Ord a => [E] -> [[a]] -> Maybe [[a]]
applyConstraints constraints vars
  | any null vars = Nothing
  | vars == vars' = Just vars
  | otherwise     = applyConstraints constraints vars'
  where
  vars' = foldr applyConstraint vars constraints

applyConstraint :: Ord a => E -> [[a]] -> [[a]]
applyConstraint a vars = case a of
  a :$= b -> filterPossibleVars vars
    where
    possibleSetValues = (nub $ concat [ vars !! a | Var a <- a ]) `intersect` (nub $ concat [ vars !! b | Var b <- b ])
    filterPossibleVars vars = foldr (\ (Var a) vars -> replace ((vars !! a) `intersect` possibleSetValues) a vars) vars (a ++ b)
    knownSetValues vars = [ head $ vars !! i | Var i <- a ++ b, length (vars !! i) == 1 ]
    filterKnownSetValues a vars = :: [a] -> 
      where
      known = knownSetValues vars
      a' = map (\ (Var i) -> vars !! i) a

  Var a :== Var b -> replace n a $ replace n b vars where n = (vars !! a) `intersect` (vars !! b)
  Var a :/= Var b
    | length (vars !! a) == 1 -> replace ((vars !! b) \\ (vars !! a)) b vars
    | length (vars !! b) == 1 -> replace ((vars !! a) \\ (vars !! b)) a vars
    | otherwise -> vars
  a :-> b -> case eval vars a of
    Nothing    -> vars
    Just False -> vars
    Just True  -> applyConstraint b vars
  a :&& b -> applyConstraint a $ applyConstraint b vars
  _ :|| _
    | length terms == length falses + 1 -> applyConstraint term vars
    | otherwise -> vars
    where
    terms  = [ (t, eval vars t) | t <- orTerms a ]
    falses = [ () | (_, Just False) <- terms ]
    term   = head [ a | (a, v) <- terms, v /= Just False ]
  where
  replace a i l = take i l ++ [a] ++ drop (i + 1) l
  orTerms a = case a of
    a :|| b -> orTerms a ++ orTerms b
    a       -> [a]

eval :: Ord a => [[a]] -> E -> Maybe Bool
eval vars a = case a of
  a :&& b -> case (eval vars a, eval vars b) of
    (Just True, Just True) -> Just True
    (Just False, _) -> Just False
    (_, Just False) -> Just False
    _ -> Nothing
  a :|| b -> case (eval vars a, eval vars b) of
    (Just False, Just False) -> Just False
    (Just True, _) -> Just True
    (_, Just True) -> Just True
    _ -> Nothing
  a :-> b -> case eval vars a of
    Nothing -> Nothing
    Just True -> eval vars b
    Just False -> Just True
  Var a :== Var b
    | length (vars !! a) == 1 && length (vars !! b) == 1 -> Just (vars !! a == vars !! b)
    | null ((vars !! a) `intersect` (vars !! b)) -> Just False
    | otherwise -> Nothing
  Var a :/= Var b
    | length (vars !! a) == 1 && length (vars !! b) == 1 -> Just (vars !! a /= vars !! b)
    | null ((vars !! a) `intersect` (vars !! b)) -> Just True
    | otherwise -> Nothing
  a :$= b
    | all (\ a -> length a == 1) (a' ++ b') && length a == length b -> Just (sort (concat a') == sort (concat b'))
    | otherwise -> Nothing  -- XXX Can strengthen this by checking if any values are known and not in the other set.
    where
    a' = [ vars !! a | Var a <- a]
    b' = [ vars !! b | Var b <- b]

-- | Create a new variables with an initial domain.
newVar :: [a] -> FD a Var
newVar domain = do
  if null domain then error "Variables can't have empty domain." else return ()
  db <- get
  set db { nextId = nextId db + 1, variables = variables db ++ [domain] }
  return $ Var $ nextId db

-- | Add a constraint.
assert :: E -> FD a ()
assert a = do
  db <- get
  set db { constraints = constraints db ++ [a] }

