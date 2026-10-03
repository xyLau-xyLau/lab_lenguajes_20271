module MiniLispPlusPlus where

import Control.Monad.IO.Class (liftIO)
import Grammars
import Interp
import Lexer
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)

-- RETO 5: integrar el combinador Y ----------------------------------------

combinadorY :: ASA
combinadorY = 
  unwrapper(desugar 
    $ parsea "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f(x x)))))")

-- Evalua combinadorY en el ambiente vacio y asocia su valor con el nombre Y.
prelude :: Env
prelude = [("Y", unwrapper(bigStep [] combinadorY))]

-- Integra el analisis, el desazucarado y la evaluacion desde prelude.
-- El resultado final debe pasar por strict antes de devolverse.
evalua :: String -> Maybe Value
evalua x = pasosIntermedios (desugar $ parsea x)

-- Función para integrar la aplicación de strict al resultado de bigstep con una expresión
-- correctamente desazucarada 
pasosIntermedios :: Maybe ASA -> Maybe Value
pasosIntermedios (Just desazucarado) = aplicaStrict (bigStep prelude desazucarado)
pasosIntermedios _ = Nothing

-- Función para aplicar strict al valor final
aplicaStrict :: Maybe Value -> Maybe Value
aplicaStrict (Just value) = strict value
aplicaStrict Nothing = Nothing

-- Infraestructura provista. No forma parte de los retos.
repl :: IO ()
repl = runInputT defaultSettings loop

parsea :: String -> SASA
parsea = parse . lexer

loop :: InputT IO ()
loop =
  getInputLine "MiniLisp++> " >>= maybe (pure ()) procesaEntrada

procesaEntrada :: String -> InputT IO ()
procesaEntrada ":q" = pure ()
procesaEntrada entrada =
  liftIO (maybe muestraBloqueo print (evalua entrada)) >> loop

muestraBloqueo :: IO ()
muestraBloqueo =
  putStrLn "Error: evaluacion bloqueada"

main :: IO ()
main = repl
