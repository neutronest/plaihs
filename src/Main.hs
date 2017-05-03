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

data ExprC a  where
  NumC :: Int -> ExprC Int
  PlusC :: ExprC a -> ExprC a -> ExprC a
  MultC :: ExprC a -> ExprC a -> ExprC a
  VarC :: Symbol -> ExprC a
  -- func :: name -> param -> body -> end
  FunC :: Symbol -> Symbol -> ExprC a -> ExprC a
  -- apply :: arg (as expr)-> body -> end
  AppC :: ExprC a -> ExprC a -> ExprC a
  -- lambda expression :: arg (as expr) -> body -> end
  LamC :: Symbol -> ExprC a -> ExprC a
  -- setC :: name -> expr -> end
  SetC :: Symbol -> ExprC a -> ExprC a
  -- seqC :: seq1 -> seq2 -> end
  SeqC :: ExprC a -> ExprC a -> ExprC a
  -- objC :: listof symbol -> listof expr -> end
  ObjC :: [Symbol] -> [ExprC a] -> ExprC a

instance Show (ExprC a) where

  showsPrec _ (NumC n) =
    showString "NumC " .
    (shows n)

  showsPrec d (PlusC e1 e2) =
    showString "PlusC (" .
    (showsPrec d e1) .
    (showsPrec d e2) .
    showString " )"

  showsPrec d (MultC e1 e2) =
    showString "MultC (" .
    (showsPrec d e1) .
    (showsPrec d e2) .
    showString " )"

  showsPrec d (VarC sym) =
    showString "VarC (" .
    (showString sym) .
    showString " )"

  showsPrec d (FunC name param body) =
    showString "FunC (" .
    (showsPrec d name) .
    (showsPrec d param) .
    (showsPrec d body) .
    showString " )"

  showsPrec _ _ = showString "Nothing"

data Value a where
  NumV :: Int -> Value Int
  ClosV :: Symbol -> ExprC a -> Env -> Value a
  ObjV :: [Symbol] -> [Value a] -> Value a

instance Show (Value a) where
  showsPrec _ (NumV i) =
    showString "NumV "
    . (shows i)
  showsPrec _ _ = shows "Undefined"



numPlus :: Value a -> Value a -> Maybe (Value a)
numPlus (NumV a) (NumV b) = Just $ NumV (a+b)
numPlus _ _ = Nothing

numMult :: Value a -> Value a -> Maybe (Value a)
numMult (NumV a) (NumV b) = Just $ NumV (a*b)
numMult _ _ = Nothing

{-

numPlus :: Result a b -> Result a b -> Maybe (Result a b)
numPlus (NumV a) (NumV b) = Just $ NumV (a+b)
numPlus _ _ = Nothing

numMult :: Value a -> Value a -> Maybe (Value a)
numMult (NumV a) (NumV b) = Just $ NumV (a*b)
numMult _ _ = Nothing


-}

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



interp :: ExprC a -> Env -> Store a -> Maybe (Result a a)
interp expr env store =
  case expr of
    NumC num -> Just $ Result ((NumV num), store)
    PlusC e1 e2 ->
      do
      Result (v1, _) <- interp e1 env store
      Result (v2, s2) <- interp e2 env store
      case numPlus v1 v2 of
        Just v -> Just $ Result(v, s2)
        _ -> Nothing

    MultC e1 e2 ->
      do
        Result(v1, _) <- interp e1 env store
        Result(v2, s2) <- interp e2 env store
        case numMult v1 v2 of
          Just v -> Just $ Result(v, s2)
          _ -> Nothing
    _ -> Nothing

-- interp :: [ExprC a] -> 


main :: IO ()
main = do
  putStrLn "well..."
  let res = interp (PlusC (NumC 10) (NumC 5)) [] (Store []) in
    putStrLn (show (PlusC (NumC 10) (NumC 5))) >>
    case res of
      Just (Result (val, _)) ->
        putStrLn $ show val
      _ -> putStrLn "fuck"
