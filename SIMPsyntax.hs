--
-- Module SIMPsyntax
-- 
-- 11 October 2012
-- 

module SIMPsyntax where

import Data.Char
import Data.List

-- Basic Types

type Identifier = String
type Variable = Identifier

class SyntacticObject a  where
  var :: a -> [Variable]  -- Returns the list of variables in a syntactic object

--
-- Arithmetic expressions
--

data ArithSIMP = 
   C Int 
 | V Variable 
 | ArithSIMP :+ ArithSIMP 
 | ArithSIMP :- ArithSIMP 
 | ArithSIMP :* ArithSIMP

instance SyntacticObject ArithSIMP where
   var (C c) = []
   var (V x) = [x]
   var (a :+ a') = nub $ (var a) ++ (var a')
   var (a :- a') = nub $ (var a) ++ (var a')
   var (a :* a') = nub $ (var a) ++ (var a')
   
someNoDigits = not . and . (map isAlphaNum)

enclosingParenthesis xs = if someNoDigits xs then '(':xs++")" else xs

instance Show ArithSIMP where
  show (C n) = show n
  show (V x) = x
  show (a :+ a') = enclosingParenthesis ashow ++" + "++ enclosingParenthesis ashow'
    where 
       (ashow,ashow') = (show a,show a')
  show (a :- a') = enclosingParenthesis ashow ++" - "++ enclosingParenthesis ashow'
    where 
       (ashow,ashow') = (show a,show a')
  show (a :* a') = enclosingParenthesis ashow ++" * "++ enclosingParenthesis ashow'
    where 
       (ashow,ashow') = (show a,show a')
  

--
-- Boolean expressions
--

data BoolSIMP = 
   B Bool 
 | ArithSIMP :== ArithSIMP 
 | ArithSIMP :<= ArithSIMP 
 | Not BoolSIMP
 | BoolSIMP :\/ BoolSIMP 

instance SyntacticObject BoolSIMP where
   var (B b) = []
   var (a :== a') = nub $ (var a) ++ (var a')
   var (a :<= a') = nub $ (var a) ++ (var a')
   var (Not b) = var b
   var (b :\/ b') = nub $ (var b) ++ (var b')
   
symbNot = '~'

instance Show BoolSIMP where
  show (B b) = show b
  show (a :== a') = show a ++ " = " ++ show a'
  show (a :<= a') = show a ++ " <= " ++ show a'
  show (Not b) = symbNot:enclosingParenthesis (show b)
  show (b :\/ b') = enclosingParenthesis bshow ++" * "++ enclosingParenthesis bshow'
    where 
       (bshow,bshow') = (show b,show b')
  
--
-- Statements
--

data StatementSIMP = 
   Skip 
 | StatementSIMP :# StatementSIMP
 | String := ArithSIMP
 | If BoolSIMP StatementSIMP StatementSIMP
 | While BoolSIMP StatementSIMP

instance SyntacticObject StatementSIMP where
  var (Skip) = []
  var (s :# s') = nub $ (var s) ++ var s'
  var (x := s) = nub $ (x:var s)
  var (If b s s') = nub $ (var b) ++ (var s) ++ (var s')
  var (While b s) = nub  $ (var b) ++ (var s)

tab = "  "
insertTabs xs = concat (map (\ x -> tab++x++"\n") ls) 
   where ls = lines xs
   
instance Show StatementSIMP where
  show Skip = "Skip"
  show (s :# s') = show s ++ ';':'\n':show s'
  show (x := s) = x ++ " := "++show s
  show (If b s s') = "if "++show b++" then \n"++insertTabs (show s)++"else \n"++insertTabs (show s')
  show (While b s) = "while "++(show b)++" do \n"++insertTabs (show s)

