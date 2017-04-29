{-# LANGUAGE

GADTs,
RankNTypes


#-}
module Main where



data KeyWord =
  Let |
  LetRec |
  IF |
  WHILE |
  
  AND |
  OR

{- data Symbol = -}
type Symbol = String

data ExprC a where
  NumC :: Int -> ExprC Int
  PlusC :: ExprC Int -> ExprC Int -> ExprC Int
  MultC :: ExprC Int -> ExprC Int -> ExprC Int
  VarC :: Symbol -> ExprC a
  -- func :: name -> param -> body
  FunC :: Symbol -> Symbol -> ExprC a
  -- apply :: arg (as expr)-> body
  AppC :: ExprC a -> ExprC a
  -- lambda expression :: arg (as expr) -> body
  LamC :: Symbol -> ExprC a
  SetC :: Symbol -> ExprC a
  SeqC :: ExprC a -> ExprC a
  ObjC :: [Symbol] -> [ExprC a] -> ExprC a

data Value a where
  NumV :: Int -> Value Int


numPlus :: Value a -> Value a -> Value a
numPlus (NumV a) (NumV b) = NumV (a+b)

numMult :: Value a -> Value a -> Value a
numMult (NumV a) (NumV b) = NumV (a*b)

type Location = Int
type Binding = (Symbol, Location)
type Env = [Binding]
{-
data Binding a where
  Bind :: [(Symbol, Location)] -> Binding a
data Env a = Env [Binding a]
-}
data Cell a = Cell (Location, Value a)
data Store a = Store [Cell a]

data Result v s = Result (Value v, Store s)

lookupLoc :: Symbol -> Env -> Maybe Location
lookupLoc sym [] = Nothing
lookupLoc sym ((sym_, loc_): binds) =
  if sym == sym_ then (Just loc_)
  else lookupLoc sym binds

lookupVal :: Location -> Store a -> Maybe (Value a)
lookupVal loc (Store []) = Nothing
lookupVal loc (Store ( (Cell(loc_, val_)):cells )) =
  if loc == loc_ then (Just val_)
  else lookupVal loc (Store cells)



interp :: ExprC a -> Env -> Store a -> Maybe(Result a a)
interp expr env store =
  case expr of
    NumC num -> Just $ Result ((NumV num), store)
    _ -> Nothing

-- interp :: [ExprC a] -> 


main :: IO ()
main = putStrLn "Hello, Haskell!"
