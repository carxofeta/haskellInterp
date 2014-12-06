module SIMPexamples where

import SIMPsyntax

---
--- fact
---

fact = 
     ("n" := C 4) -- write here the argument to fact
   :# (("c" := C 2)
   :# (("r" := C 1)
   :# ((While ((V "c") :<= (V "n"))
                    (("r" := ((V "r") :* (V "c")))
                      :# ("c" := (V "c" :+ C 1))
                    )
        )
    :# ("f" := V "r")
       )
       )
    )
 
{- 

show fact 
    
   yields
    
"n := 4;
c := 2;
r := 1;
while c <= n do 
  r := r * c;
  c := c + 1
;
f := r"

-}

---
--- program1
---

program1 = 
  ("n" := C 4) -- write here the upper limit
  :# (
  ("s" := C 0)
  :# ((While ((C 1) :<= (V "n")) 
       ("s" := ((V "s") :+ (V "n"))
       :# ("n" := ((V "n") :- (C 1)))
       )
  ) 
  :# 
  ("f" := (V "s"))
  )
  )
  
 {- 

show program1 
    
   yields
    
"n := 4;
s := 0;
while 1 <= n do 
  s := s + n;
  n := n - 1
;
f := s"

-}

---
--- program2
---

 
program2 =
  ("n" := C 4) -- write here the upper limit
  :# (
  ("q" := C 0)
  :# (
  ("r" := V "n")
  :# (
  (While ((C 2) :<= (V "r")) ( 
       ("r" := ((V "r") :- (C 2))) 
       :#
       ("q" := ((V "q") :+ (C 1)))
       )
  )
  :# ( If ((V "r") :== (C 0)) 
     ("s" := ((V "q") :* ((V "n") :+ (C 1))))
     ("s" := (((V "q") :+ (C 1)) :* (V "n")))
  :# ("f" := (V "s"))
  )
  )
  )
  )

 {- 

show program2 
    
   yields
    
"n := 4;
q := 0;
r := n;
while 2 <= r do 
  r := r - 2;
  q := q + 1
;
if r = 0 then 
  s := q * (n + 1)
else 
  s := (q + 1) * n
;
f := s"

-}

---
--- nestedWhile
---


nestedWhile =
  (While ((C 0) :<= (V "x")) (
       ("y" := C 2)
       :# (
       ((While ((V "y") :<= ((V "x") :+ (C 1)))
           ("y" := ((V "y") :+ (C 2)))
       )
       )
       :#
       ("x" := ((V "x") :- (C 1)))
       )
       )
 )
 
 {- 

show nestedWhile 
    
   yields
    
"while 0 <= x do 
  y := 2;
  while y <= x + 1 do 
    y := y + 2
  ;
  x := x - 1"

-}
