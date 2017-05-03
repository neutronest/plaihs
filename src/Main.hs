{-# LANGUAGE

GADTs,
StandaloneDeriving


#-}

module Main where
import Control.Monad



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
  NumC :: Int -> ExprC Int;
  PlusC :: ExprC a -> ExprC a -> ExprC a;
  MultC :: ExprC a -> ExprC a -> ExprC a;
  VarC ::  Symbol -> ExprC a;
  -- func :: name -> param -> body -> end
  FunC ::  Symbol -> Symbol -> ExprC a -> ExprC a;
  -- apply :: arg (as expr)-> body -> end
  AppC ::  ExprC a -> ExprC a -> ExprC a;
  -- lambda expression :: arg (as expr) -> body -> end
  LamC ::  Symbol -> ExprC a -> ExprC a;
  -- setC :: name -> expr -> end
  SetC ::  Symbol -> ExprC a -> ExprC a;
  -- seqC :: seq1 -> seq2 -> end
  SeqC ::  ExprC a -> ExprC a -> ExprC a;
  -- objC :: listof symbol -> listof expr -> end
  ObjC ::  [Symbol] -> [ExprC a] -> ExprC a

deriving instance Eq (ExprC a)

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

deriving instance Eq (Value a)

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
data Cell a = Cell (Location, Value a) deriving Eq
data Store a = Store [Cell a] deriving Eq

data Result va sa = Result (Value va, Store sa) deriving Eq

valFromResult :: Result va sa -> Value va
valFromResult (Result (v, s)) = v


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


hasNothing :: Eq a => [Maybe a] -> Bool
hasNothing [] = True
hasNothing (x:xs) =
  if x == Nothing then True
  else hasNothing xs


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
    ObjC syms exprs ->
      -- mrs :: [Maybe Result]
      let mayResList = map (\expr -> interp expr env store) exprs in
        if (hasNothing mayResList) == True then Nothing
        else
          -- mayRes :: Maybe Result
          -- r :: Result
          -- liftM2 (Result -> Value) -> Maybe Result -> Maybe Value
          let mayVs = map (\mayRes ->
                          (liftM
                           (\r -> valFromResult r)
                           mayRes)
                       ) mayResList
          in
            let go [] rs = rs
                go (x:xs) rs =
                  case x of
                    Just v -> go xs (rs++[v])
                    Nothing -> []
            in
              let valList = go mayVs [] in
              if valList == [] then Nothing
              else Just $ Result ((ObjV syms valList), store)
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
  
