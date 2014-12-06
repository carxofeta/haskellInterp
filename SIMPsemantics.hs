import SIMPsyntax

import SIMPexamples

type State = String -> Int

type Conf a b = (a,b)

-- evalArith :: Conf ArithSIMP State -> Int
evalArith (C n,state) = n
evalArith (V x,state) = state x
evalArith (a :+ a',state) = (evalArith (a,state)) + (evalArith (a',state))
evalArith (a :- a',state) = (evalArith (a,state)) - (evalArith (a',state))
evalArith (a :* a',state) = (evalArith (a,state)) * (evalArith (a',state))

-- evalBool :: Conf BoolSIMP State -> Bool
evalBool (B b,state) = b
evalBool (a :== a',state) = (evalArith (a,state)) == (evalArith (a',state))
evalBool (a :<= a',state) = (evalArith (a,state)) <= (evalArith (a',state))
evalBool (Not b,state) = not (evalBool (b,state))
evalBool (b :\/ b',state) = (evalBool (b,state)) || (evalBool (b',state))

-- sos_small_step :: Conf StatementSIMP State -> [Conf StatementSIMP State]
sos_small_step (Skip,state) = [(Skip,state)]
sos_small_step c@(Skip :# s',state) = c:sos_small_step (s',state)
sos_small_step c@(s :# s',state) = c:sos_small_step (s'' :# s',state')
  where (s'',state') = last (sos_small_step (s,state))

sos_small_step c@(x := a, state) = c:[(Skip,state')]
  where state' y = if x == y then (evalArith (a,state)) else (state y)

sos_small_step c@(If b s s',state) 
  | evalBool (b,state) = c:sos_small_step (s,state)
  | otherwise = c:sos_small_step (s',state)

sos_small_step c@(While b s,state) 
  | evalBool (b,state)  = c:sos_small_step (s:#(While b s),state)
  | otherwise = c:sos_small_step (Skip,state)

-- compute_sos_small_step :: StatementSIMP -> [Conf StatementSIMP State]
compute_sos_small_step p = sos_small_step (p,\_->0) -- All variables initialized to 0.

showState vars state = [x ++ " -> " ++ show (state x) ++"\n " | x <- vars]

see_sos_small_step p = showState (var p) state
   where   (_,state) = last (sos_small_step (p,\_->0)) -- All variables initialized to 0.



-- sos_big_step :: Conf StatementSIMP State -> State
sos_big_step (Skip,state) = state

sos_big_step c@(s :# s',state) = sos_big_step (s',sos_big_step (s,state))

sos_big_step c@(x := a, state) = state'
  where state' y = if x == y then (evalArith (a,state)) else (state y)

sos_big_step c@(If b s s',state) 
  | evalBool (b,state) = sos_big_step (s,state)
  | otherwise = sos_big_step (s',state)

sos_big_step c@(While b s,state) 
  | evalBool (b,state)  = sos_big_step (While b s,sos_big_step (s,state))
  | otherwise = state

compute_sos_big_step p = sos_big_step (p,\_->0) -- All variables initialized to 0.

see_sos_big_step p = showState (var p) (compute_sos_big_step p)

--instace Show(State) where
--show (\x -> v) =  x++" -> "++ show v
