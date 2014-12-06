import SIMPsyntax

import SIMPexamples

--type State = String -> Int
data State = E [(String,Int)] deriving Show

state :: String -> State -> Int
state _ (E []) = 0
state x (E ((y,vy):ys)) = if x == y then vy else state x (E ys)

--type Conf a b = (a,b)
data Conf a b = Conf a b deriving Show

-- evalArith :: Conf ArithSIMP State -> Int
evalArith (C n,e) = n
evalArith (V x,e) = state x e
evalArith (a :+ a',e) = (evalArith (a,e)) + (evalArith (a',e))
evalArith (a :- a',e) = (evalArith (a,e)) - (evalArith (a',e))
evalArith (a :* a',e) = (evalArith (a,e)) * (evalArith (a',e))

-- evalBool :: Conf BoolSIMP State -> Bool
evalBool (B b,e) = b
evalBool (a :== a',e) = (evalArith (a,e)) == (evalArith (a',e))
evalBool (a :<= a',e) = (evalArith (a,e)) <= (evalArith (a',e))
evalBool (Not b,e) = not (evalBool (b,e))
evalBool (b :\/ b',e) = (evalBool (b,e)) || (evalBool (b',e))

-- sos_small_step :: Conf StatementSIMP State -> [Conf StatementSIMP State]
sos_small_step (Skip,e) = [(Skip,e)]
sos_small_step c@(Skip :# s',e) = c:sos_small_step (s',e)
sos_small_step c@(s :# s',e) = c:sos_small_step (s'' :# s',e')
  where (s'',e') = last (sos_small_step (s,e))

sos_small_step c@(x := a, e) = c:[(Skip,e')]
  where e' y = if x == y then (evalArith (a,e)) else (state y e)

sos_small_step c@(If b s s',e) 
  | evalBool (b,e) = c:sos_small_step (s,e)
  | otherwise = c:sos_small_step (s',e)

sos_small_step c@(While b s,e) 
  | evalBool (b,e)  = c:sos_small_step (s:#(While b s),e)
  | otherwise = c:sos_small_step (Skip,e)

-- compute_sos_small_step :: StatementSIMP -> [Conf StatementSIMP State]
compute_sos_small_step p = sos_small_step (p,\_->0) -- All variables initialized to 0.

showState vars state = [x ++ " -> " ++ show (state x) ++"\n " | x <- vars]

see_sos_small_step p = showState (var p) e
   where   (_,e) = last (sos_small_step (p,\_->0)) -- All variables initialized to 0.



-- sos_big_step :: Conf StatementSIMP State -> State
sos_big_step (Skip,e) = e

sos_big_step c@(s :# s',e) = sos_big_step (s',sos_big_step (s,e))

sos_big_step c@(x := a, e) = e'
  where e' y = if x == y then (evalArith (a,e)) else (state y e)

sos_big_step c@(If b s s',e) 
  | evalBool (b,e) = sos_big_step (s,e)
  | otherwise = sos_big_step (s',e)

sos_big_step c@(While b s,e) 
  | evalBool (b,e)  = sos_big_step (While b s,sos_big_step (s,e))
  | otherwise = e

compute_sos_big_step p = sos_big_step (p,\_->0) -- All variables initialized to 0.

see_sos_big_step p = showState (var p) (compute_sos_big_step p)

--instace Show(State) where
--show (\x -> v) =  x++" -> "++ show v
