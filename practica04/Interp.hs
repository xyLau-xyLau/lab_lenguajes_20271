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
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

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

---------------
-- DESUGAR   --
---------------

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS args) = desugarBinaryOp Add args
desugar (SubS args) = desugarBinaryOp Sub args
desugar (NotS arg) = case desugaredArg of
                     Nothing -> Nothing
                     _ -> Just (Not (getASA desugaredArg))
                     where desugaredArg = desugar arg
desugar (LetS var arg body) = case (desugaredArg, desugaredBody) of
                              (Nothing, _) -> Nothing
                              (_, Nothing) -> Nothing
                              _ -> Just (App (Fun var (getASA (desugaredBody))) (getASA (desugaredArg)))
                              where desugaredArg = desugar arg
                                    desugaredBody = desugar body
desugar (LetStarS [] body) = case desugaredBody of
                             Nothing -> Nothing
                             _ -> Just (getASA (desugaredBody))
                             where desugaredBody = (desugar body)
desugar (LetStarS bindings body) = desugar(desugarLetStarSAux (reverse(bindings)) body)
desugar (FunS params body) = case desugaredBody of
                             Nothing -> Nothing
                             _ -> curryFun params (getASA(desugaredBody))
                             where desugaredBody = desugar body
desugar (AppS exp args) = case (desugaredExp, nothingInArgs) of
                          (Nothing, _) -> Nothing
                          (_, True) -> Nothing
                          _ -> curryApp (getASA(desugaredExp)) asaArgs
                          where desugaredExp = desugar exp
                                desugaredArgs = map desugar args
                                nothingInArgs = elem Nothing desugaredArgs
                                asaArgs = map (getASA) desugaredArgs

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
desugarLetStarSAux ((nombre, x):xs) y = desugarLetStarSAux xs (LetS nombre x y)


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((nombre, valor):xs) = if (x == nombre)
                                   then Just valor
                                   else lookupEnv x xs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
bigStep env (Id x) = lookupEnv x env
bigStep env (Add n m) = numOp (+) (bigStep env n) (bigStep env m)
bigStep env (Sub n m) = numOp (-) (bigStep env n) (bigStep env m)
bigStep env (Not b) = notOp (bigStep env b)
-- Verificar caso límite, x = ""
bigStep env (Fun x f) = Just (ClosureV x f env)
bigStep env (App f arg) = seq parameter
                            (seq argumentValue
                              (bigStep body
                                ((parameter, argumentValue) : definitionEnv)))
                          where closureValue = bigStep f env
                                parameter = closureP closureValue
                                body = closureC closureValue
                                definitionEnv = closureE closureValue
                                argumentValue = bigStep arg env
bigStep _ _ = Nothing

closure
closureCPE :: Maybe Value -> (String, Maybe ASA, Env)
closureCPE (Closure V parameter exp env) = (parameter, exp, env)
closureCPE _ = ("", Nothing, [])

closureP :: Maybe Value -> Maybe ASA
closure



-- Función auxiliar para la regla de evaluación de Add y Sub
numOp :: (Int -> Int -> Int) -> Maybe Value -> Maybe Value -> Maybe Value
numOp f (Just (NumV n)) (Just (NumV m)) = Just (NumV (max (f n m) 0))
numOP _ _ _ = Nothing

-- Función auxiliar para la regla de evaluación Not
notOp :: Maybe Value -> Maybe Value
notOp Nothing = Nothing
notOp (Just (BooleanV False)) = Just (BooleanV True)
notOp _ = Just (BooleanV False)