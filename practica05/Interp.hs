module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

---------------
-- CURRY FUN --
---------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun ls asa = curryFunAux (reverse ls) [] asa

-- Función auxiliar para detectar ocurrencias repetidas de variables y efectúar la currificación
curryFunAux :: [Nombre] -> [Nombre] -> ASA -> Maybe ASA
curryFunAux [] _ asa = Just asa
curryFunAux (x:xs) ls asa 
    | elem x ls = Nothing
    | otherwise = curryFunAux xs (x:ls) (Fun x asa)

---------------
-- CURRY APP --
---------------

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp asa ls = curryAppAux asa ls

-- Función auxiliar para currificar
curryAppAux :: ASA -> [ASA] -> Maybe ASA
curryAppAux asa [] = Just asa
curryAppAux asa (x:xs) = curryAppAux (App asa x) xs

---------------
-- BINARY OP --
---------------
-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ [] = Nothing
binaryOp _ [x] = Nothing
binaryOp f (x:xs) = Just (foldl f x xs)

-------------
-- DESUGAR --
-------------

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond conds _else = if (desugaredElse == Nothing || foldr (&&) True (map checkForNothingInTuple desugaredConds)) 
                          then Nothing
                          else desugarCondAux (reverse(desugaredConds)) desugaredElse
                          where desugaredElse = desugar _else
                                desugaredConds = [(desugar a, desugar b) | (a,b) <- conds]

-- Verificamos si algún Maybe ASA es Nothing
checkForNothingInTuple :: ((Maybe ASA), (Maybe ASA)) -> Bool
checkForNothingInTuple (a, b) = (a == Nothing) || (b == Nothing)

-- Auxiliar para construcción de If
desugarCondAux :: [(Maybe ASA, Maybe ASA)] -> Maybe ASA -> Maybe ASA
desugarCondAux [] a@(Just asa) = a
desugarCondAux ((Just a, Just b) : xs) (Just asa) = desugarCondAux xs (Just (If a b asa))
desugarCondAux _ _ = Nothing

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS args) = desugarBinaryOp Add args
desugar (SubS args) = desugarBinaryOp Sub args
desugar (NotS arg) = let desugaredArg = desugar arg
                     in if (desugaredArg == Nothing)
                        then Nothing
                        else Just (Not (getASA desugaredArg))
desugar (LetS var arg body) = if checkForNothingInTuple(desugaredArg, desugaredBody)
                              then Nothing
                              else Just (App (Fun var (getASA (desugaredBody))) (getASA (desugaredArg)))
                              where desugaredArg = desugar arg
                                    desugaredBody = desugar body
desugar (LetStarS [] body) = if desugaredBody == Nothing
                             then Nothing
                             else Just (getASA (desugaredBody))
                             where desugaredBody = (desugar body)
desugar (LetStarS bindings body) = desugar(desugarLetStarSAux (reverse(bindings)) body)
desugar (FunS params body) = if desugaredBody == Nothing
                             then Nothing
                             else curryFun params (getASA(desugaredBody))
                             where desugaredBody = desugar body
desugar (AppS exp args) = if (desugaredExp == Nothing || nothingInArgs)
                          then Nothing
                          else curryApp (getASA(desugaredExp)) asaArgs
                          where desugaredExp = desugar exp
                                desugaredArgs = map desugar args
                                nothingInArgs = elem Nothing desugaredArgs
                                asaArgs = map (getASA) desugaredArgs
desugar (IfS cond cons alt) = if (desugaredCond == Nothing || desugaredCons == Nothing || desugaredAlt == Nothing)
                              then Nothing
                              else Just(If (getASA(desugaredCond)) (getASA(desugaredCons)) (getASA(desugaredAlt)))
                              where desugaredCond = desugar cond
                                    desugaredCons = desugar cons
                                    desugaredAlt = desugar alt
desugar (CondS conds alt) = desugarCond conds alt
desugar (LetRecS var arg body) = desugar (LetS var (AppS (IdS "Y") [(FunS [var] arg)]) body)

-- Auxiliar para obtener el ASA de un Maybe ASA
getASA :: Maybe ASA -> ASA
getASA (Just asa) = asa

-- Auxiliar para desazucarar operaciones narias
desugarBinaryOp :: (ASA -> ASA -> ASA) -> [SASA] -> Maybe ASA
desugarBinaryOp f args = if (elem Nothing desugaredArgs)
                         then Nothing
                         else (binaryOp f (map getASA desugaredArgs))
                         where desugaredArgs = map desugar args

-- Auxiliar para transformar LetStarS en LetS 
desugarLetStarSAux :: [(Nombre, SASA)] -> SASA -> SASA
desugarLetStarSAux [] y = y
desugarLetStarSAux ((name, x):xs) y = desugarLetStarSAux xs (LetS name x y)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv x [] = Nothing
lookupEnv x ((name, expr):xs) = if x == name
                                  then Just expr
                                  else lookupEnv x xs

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (ExprV asa env) = strictAux (bigStep env asa)
strict e = Just e

-- Auxiliar para resolver el caso donde al aplicar strict obtenemos otra expr
strictAux :: Maybe Value -> Maybe Value
strictAux (Just e@(ExprV x expr)) = strict e
strictAux (Just(value)) = Just value
strictAux Nothing = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Id x) = lookupEnv x env
bigStep env (Add n m) = addOp (strictApp(bigStep env n)) (strictApp(bigStep env m))
bigStep env (Sub n m) = subOp (strictApp(bigStep env n)) (strictApp(bigStep env m))
bigStep env (Not b) = notOp (strictApp(bigStep env b))
bigStep env (Fun x f) = Just (ClosureV x f env)
bigStep env (App f arg) = appOp env (strictApp(bigStep env f)) arg
bigStep env (If cond cons alt) = ifOp env (strictApp(bigStep env cond)) cons alt

-- Función para aplicar el punto estricto al resultado de la evaluación con bigStep
strictApp :: Maybe Value -> Maybe Value
strictApp (Just v) = strict v
strictApp Nothing = Nothing

-- Función auxiliar para efectuar suma de dos NumV
addOp :: Maybe Value -> Maybe Value -> Maybe Value
addOp (Just (NumV n)) (Just (NumV m)) = Just (NumV (n + m))
addOp _ _ = Nothing

-- Función auxiliar para efectuar resta truncada de dos NumV
subOp :: Maybe Value -> Maybe Value -> Maybe Value
subOp (Just (NumV n)) (Just (NumV m)) = Just (NumV (max 0 (n - m)))
subOp _ _ = Nothing

-- Función auxiliar para efectuar Not sobre un BooleanV
notOp :: Maybe Value -> Maybe Value
notOp (Just (BooleanV False)) = Just (BooleanV True)
notOp (Just (BooleanV True)) = Just (BooleanV False)
notOp (Just (NumV _)) = Just (BooleanV False)
notOp _ = Nothing

-- Función auxiliar para la aplicación
appOp :: Env -> Maybe Value -> ASA -> Maybe Value
appOp env (Just (ClosureV param body env')) arg = bigStep ((param, (ExprV arg env)): env') body
appOp _ _ _ = Nothing

-- Función auxiliar para la evaluación de if
ifOp :: Env -> Maybe Value -> ASA -> ASA -> Maybe Value
ifOp env (Just (BooleanV True)) cons _ = bigStep env cons
ifOp env (Just (BooleanV False)) _ alt = bigStep env alt
ifOp _ _ _ _ = Nothing