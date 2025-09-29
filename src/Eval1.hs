module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                Just n  -> n
                Nothing -> error "UndefVar"

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s)
stepComm (Let v e) s =
  let (n :!: s') = evalExp e s
  in (Skip :!: update v n s')
stepComm (Seq Skip c2) s = (c2 :!: s)
stepComm (Seq c1 c2) s =
  let (c1' :!: s') = stepComm c1 s
  in (Seq c1' c2 :!: s')
stepComm (IfThenElse b c1 c2) s =
  let (cond :!: s') = evalExp b s
  in if cond then (c1 :!: s') else (c2 :!: s')
stepComm (RepeatUntil c b) s =
  (Seq c (IfThenElse b Skip (RepeatUntil c b)) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var v) s   = (lookfor v s :!: s)
evalExp (UMinus e) s = 
  let (v :!: s1) = evalExp e s 
  in ((-v) :!: s1)
evalExp (Plus e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 + v2 :!: s2)
evalExp (VarInc v) s = 
  let n = lookfor v s + 1
  in (n :!: update v n s)
evalExp (Minus e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 - v2 :!: s2)
evalExp (Times e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 * v2 :!: s2)
evalExp (Div e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in if v2 == 0 
      then error "DivByZero"
      else (div v1 v2 :!: s2)
evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Lt e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 < v2 :!: s2)
evalExp (Gt e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 > v2 :!: s2)
evalExp (And e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 && v2 :!: s2)
evalExp (Or e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in ((v1 || v2) :!: s2)
evalExp (Not e1) s = 
  let (v :!: s1) = evalExp e1 s
  in (not v :!: s1)
evalExp (Eq e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 == v2 :!: s2)
evalExp (NEq e1 e2) s =
  let (v1 :!: s1) = evalExp e1 s
      (v2 :!: s2) = evalExp e2 s1
  in (v1 /= v2 :!: s2)