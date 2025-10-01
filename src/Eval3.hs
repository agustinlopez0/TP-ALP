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
lookfor v (s, _) = maybe (Left UndefVar) Right (M.lookup v s)

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

stepComm (Let v e) s = do
  (n :!: s') <- evalExp e s
  pure (Skip :!: update v n s')

stepComm (Seq Skip c2) s = Right (c2 :!: s)
stepComm (Seq c1 c2) s = do
  (c1' :!: s') <- stepComm c1 s
  pure (Seq c1' c2 :!: s')

stepComm (IfThenElse b c1 c2) s = do
  (cond :!: s') <- evalExp b s
  pure $ if cond then (c1 :!: s') else (c2 :!: s')

stepComm (RepeatUntil c b) s =
  Right (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Helpers para simplificar evalExp
binOp :: (a -> b -> c) -> Exp a -> Exp b -> State -> Either Error (Pair c State)
binOp op e1 e2 s = do
  (v1 :!: s1) <- evalExp e1 s
  (v2 :!: s2) <- evalExp e2 s1
  pure (op v1 v2 :!: s2)

unOp :: (a -> b) -> Exp a -> State -> Either Error (Pair b State)
unOp op e s = do
  (v :!: s1) <- evalExp e s
  pure (op v :!: s1)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s    = Right (n :!: s)
evalExp (Var v) s      = (:!: s) <$> lookfor v s
evalExp (UMinus e) s   = unOp negate e s
evalExp (Plus e1 e2) s = binOp (+) e1 e2 s
evalExp (Minus e1 e2) s= binOp (-) e1 e2 s
evalExp (Times e1 e2) s= binOp (*) e1 e2 s

evalExp (Div e1 e2) s = do
  (v1 :!: s1) <- evalExp e1 s
  (v2 :!: s2) <- evalExp e2 s1
  if v2 == 0 then Left DivByZero else Right (div v1 v2 :!: s2)

evalExp (VarInc v) s = do
  val <- lookfor v s
  let s' = update v (val + 1) s
  Right (val + 1 :!: s')

evalExp BTrue  s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)

evalExp (Lt e1 e2) s  = binOp (<) e1 e2 s
evalExp (Gt e1 e2) s  = binOp (>) e1 e2 s
evalExp (Eq e1 e2) s  = binOp (==) e1 e2 s
evalExp (NEq e1 e2) s = binOp (/=) e1 e2 s
evalExp (And e1 e2) s = binOp (&&) e1 e2 s
evalExp (Or e1 e2) s  = binOp (||) e1 e2 s
evalExp (Not e) s     = unOp not e s