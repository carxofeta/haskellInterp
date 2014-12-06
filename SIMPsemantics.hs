import SIMPsyntax

import SIMPexamples

type State = String -> Int

type Conf a b c = (a,b,c)

-- evalArith :: Conf ArithSIMP State -> Int
evalArith (C n,state,cont) = n
evalArith (V x,state,cont) = state x
evalArith (a :+ a',state,cont) = (evalArith (a,state,cont)) + (evalArith (a',state,cont))
evalArith (a :- a',state,cont) = (evalArith (a,state,cont)) - (evalArith (a',state,cont))
evalArith (a :* a',state,cont) = (evalArith (a,state,cont)) * (evalArith (a',state,cont))

-- evalBool :: Conf BoolSIMP State -> Bool
evalBool (B b,state,cont) = b
evalBool (a :== a',state,cont) = (evalArith (a,state,cont)) == (evalArith (a',state,cont))
evalBool (a :<= a',state,cont) = (evalArith (a,state,cont)) <= (evalArith (a',state,cont))
evalBool (Not b,state,cont) = not (evalBool (b,state,cont))
evalBool (b :\/ b',state,cont) = (evalBool (b,state,cont)) || (evalBool (b',state,cont))

-- sos_small_step :: Conf StatementSIMP State -> [Conf StatementSIMP State]
sos_small_step (Skip,state,cont) = [(Skip,state,cont)]
sos_small_step c@(Skip :# s',state,cont) = c:sos_small_step (s',state,cont)
sos_small_step c@(s :# s',state,cont) = c:sos_small_step (s'' :# s',state',cont')
  where (s'',state',cont') = last (sos_small_step (s,state,cont))

sos_small_step c@(x := a, state, cont) = c:[(Skip,state',cont)]
  where state' y = if x == y then (evalArith (a,state,cont)) else (state y)

sos_small_step c@(If b s s',state,cont) 
  | evalBool (b,state,cont) = c:sos_small_step (s,state,cont)
  | otherwise = c:sos_small_step (s',state,cont)

sos_small_step c@(While b s,state,cont) 
  | evalBool (b,state,cont)  = c:sos_small_step (s:#(While b s),state,cont)
  | otherwise = c:sos_small_step (Skip,state,cont)

-- compute_sos_small_step :: StatementSIMP -> [Conf StatementSIMP State]
compute_sos_small_step p cont= sos_small_step (p,cont,\_->0) -- All variables initialized to 0.

showState vars state cont = [x ++ " -> " ++ show (state x) ++ ", " ++ cont ++ "\n " | x <- vars]

see_sos_small_step p = showState (var p) state
   where   (_,state,cont) = last (sos_small_step (p,cont,\_->0)) -- All variables initialized to 0.



-- sos_big_step :: Conf StatementSIMP State -> State
sos_big_step (Skip,state,cont) = state

sos_big_step c@(s :# s',state,cont) = sos_big_step (s',sos_big_step (s,state,cont),cont)

sos_big_step c@(x := a, state,cont) = state'
  where state' y = if x == y then (evalArith (a,state,cont)) else (state y)

sos_big_step c@(If b s s',state,cont) 
  | evalBool (b,state,cont) = sos_big_step (s,state,cont)
  | otherwise = sos_big_step (s',state,cont)

sos_big_step c@(While b s,state,cont) 
  | evalBool (b,state,cont)  = sos_big_step (While b s,sos_big_step (s,state,cont),cont)
  | otherwise = state

compute_sos_big_step p cont = sos_big_step (p,cont,\_->0) -- All variables initialized to 0.

--see_sos_big_step p = showState (var p) (compute_sos_big_step p)

--instace Show(State) where
--show (\x -> v) =  x++" -> "++ show v
