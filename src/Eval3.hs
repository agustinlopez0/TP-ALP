module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, t) = case M.lookup v s of
                    Just n  -> Right n
                    Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v n s = addTrace ("Let " ++ v ++ " " ++ show n) (M.insert v n (Prelude.fst s), Prelude.snd s)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace a (s, t) = (s, t ++ " " ++ a)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)
stepComm (Let v e) s =
  case evalExp e s of
    Right (n :!:  s') -> Right (Skip :!: update v n s')
    Left err         -> Left err
stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s =
  case stepComm c1 s of
    Right (c1' :!: s') -> Right (Seq c1' c2 :!: s')
    Left err           -> Left err
stepComm (IfThenElse b c1 c2) s =
  case evalExp b s of
    Right (cond :!: s') -> if cond then Right (c1 :!: s') else Right (c2 :!: s')
    Left err            -> Left err
stepComm (RepeatUntil c b) s =
  Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var v) s =
  case lookfor v s of
    Right val -> Right (val :!: s)
    Left err  -> Left err

evalExp (UMinus e) s =
  case evalExp e s of
    Right (v :!: s1) -> Right ((-v) :!: s1)
    Left err         -> Left err

evalExp (Plus e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 + v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (VarInc v) s =
  case lookfor v s of
    Right val -> Right (val + 1 :!: s)
    Left err  -> Left err

evalExp (Minus e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 - v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (Times e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 * v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (Div e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) ->
          if v2 == 0
            then Left DivByZero
            else Right (div v1 v2 :!: s2)
        Left err -> Left err
    Left err -> Left err

evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)

evalExp (Lt e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 < v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (Gt e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 > v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (And e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 && v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (Or e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right ((v1 || v2) :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (Not e1) s =
  case evalExp e1 s of
    Right (v :!: s1) -> Right (not v :!: s1)
    Left err         -> Left err

evalExp (Eq e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 == v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err

evalExp (NEq e1 e2) s =
  case evalExp e1 s of
    Right (v1 :!: s1) ->
      case evalExp e2 s1 of
        Right (v2 :!: s2) -> Right (v1 /= v2 :!: s2)
        Left err          -> Left err
    Left err -> Left err
