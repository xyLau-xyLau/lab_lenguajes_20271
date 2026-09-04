module Interp where

import Grammars
import Data.List

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And ls) = foldl (++) [] (map freeVars ls)
freeVars (Or ls) = foldl (++) [] (map freeVars ls)
freeVars (Add ls) = foldl (++) [] (map freeVars ls)
freeVars (Sub ls) = foldl (++) [] (map freeVars ls)
freeVars (Mul ls) = foldl (++) [] (map freeVars ls)
freeVars (Div ls) = foldl (++) [] (map freeVars ls)
freeVars (Lt ls) = foldl (++) [] (map freeVars ls)
freeVars (Gt ls) = foldl (++) [] (map freeVars ls)
freeVars (Le ls) = foldl (++) [] (map freeVars ls)
freeVars (Ge ls) = foldl (++) [] (map freeVars ls)
freeVars (Expt e1 e2) = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2) = freeVars e1 ++ freeVars e2
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e 
freeVars (ZeroP e) = freeVars e
freeVars (Let bindings e) = foldl (++) [] (map freeVars [x | (_, x) <- bindings])
                            ++ ((freeVars e) \\ [x | (x, _) <- bindings]) 
--LetStar bindings e


names :: ASA -> [String]
names t = []

freshName :: [String] -> String
freshName u = ""

sust :: ASA -> String -> ASA -> ASA
sust a "" b = (Num 3)

sustMany :: ASA -> [Binding] -> ASA
sustMany c d = (Num 3)
-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep k = Nothing
