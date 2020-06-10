module Lib where
import Text.Show.Functions
laVerdad = True

--------------------------------------------------- Parte 1 ---------------------------------------------------
--------------------------------------------------- Punto 1 ---------------------------------------------------
data Persona = UnaPersona{
    edad :: Int,
    items :: Items,
    xp :: Int
}deriving (Show,Eq)

type Items = [String]
type Criterio = Persona->Bool

data Criatura = UnaCriatura{
    peligrosidad :: Int,
    criterio :: Criterio
}deriving (Show)

---------------------------------- Ejemplos criaturas ----------------------------------
siempreDetras :: Criatura
siempreDetras = UnaCriatura 0 puedeDeshacerseSiempreDetras -- el 0 es la peligrosidad que le paso a la funcion nivelPeligrosidad

gnomos :: Int->Criatura
gnomos cantidad = UnaCriatura (2^cantidad) puedeDeshacerseGnomos

fantasma :: Int->(Persona->Bool)->Criatura
fantasma poder asuntoPendiente = UnaCriatura (2*poder) (puedeDeshacerseFantasma asuntoPendiente)
--La desventaja esta en que si en el futuro agrego mas criaturas tengo que agregar un data nuevo y las nuevas condiciones a esta
--funcion para que el programa ande correctamente.

puedeDeshacerseSiempreDetras :: Criterio
puedeDeshacerseSiempreDetras _ = False
puedeDeshacerseGnomos :: Criterio
puedeDeshacerseGnomos persona = elem "soplador de hojas" (items persona)
puedeDeshacerseFantasma :: (Persona->Bool)->Criterio
puedeDeshacerseFantasma asuntoPendiente persona = asuntoPendiente persona

--------------------------------------------------- Punto 2 ---------------------------------------------------
enfrentamiento :: Persona->Criatura->Persona
enfrentamiento persona criatura |tieneElementoNecesario persona criatura = ganaExp persona (peligrosidad criatura)
                                |otherwise = ganaExp persona 1

tieneElementoNecesario :: Persona->Criatura->Bool
tieneElementoNecesario persona criatura = (criterio criatura) persona  

ganaExp :: Persona->Int->Persona
ganaExp persona xpGanada = persona{xp = (xp persona) + xpGanada}
--------------------------------------------------- Punto 3 ---------------------------------------------------
grupoDeCriaturas = [siempreDetras,gnomos 10,fantasma 3 (((<10).edad) && ((elem "disfraz de oveja").items))]

enfrentamientoSucesivo :: Persona->[Criatura]->Int
enfrentamientoSucesivo persona listaCriaturas = (diferenciaXp persona.foldl enfrentamiento persona) listaCriaturas
-- le aplico la funcion enfrentamiento y persona a cada criatura y al resultado le calculo la diferencia con el del principio
diferenciaXp :: Persona->Persona->Int
diferenciaXp persona personaFinal = (xp personaFinal) - (xp persona)

--------------------------------------------------- Parte 2 ---------------------------------------------------
--------------------------------------------------- Punto 1 ---------------------------------------------------
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ _ [] = agregoALista 
zipWithIf funcion1 condicion (x:xs) (y:ys) |(not.condicion) y = (zipWithIf funcion1 condicion (x:xs) ys.)( y:[])
                                           |otherwise = (zipWithIf funcion1 condicion xs ys).((x*y):[])