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
type Criterio = [String]

data Criatura = UnSiempreDetras{criterios :: Criterio} -- los pienso separado pq cada criatura tiene consideraciones distintas
               |UnosGnomos{cantidad:: Int,criterios :: Criterio}
               |UnFantasma{categoria :: Int,criterios :: Criterio}deriving (Show)

---------------------------------- Ejemplos criaturas ----------------------------------

siempreDetras = UnSiempreDetras [] -- el 0 es la peligrosidad que le paso a la funcion nivelPeligrosidad
grupoDeGnomos = UnosGnomos 20 ["soplador de hojas"]
fantasma = UnFantasma 7 ["esparcir restos en el mar"]

nivelPeligrosidad :: Criatura->Int --uso pattern matching para los distintos tipos de criatura
nivelPeligrosidad (UnSiempreDetras _) = 0
nivelPeligrosidad (UnosGnomos 1 _) = 0 -- si esta solo es inofensivo
nivelPeligrosidad (UnosGnomos cantidad _) = 2^cantidad
nivelPeligrosidad (UnFantasma categoria  _) = 20 * categoria
--La desventaja esta en que si en el futuro agrego mas criaturas tengo que agregar un data nuevo y las nuevas condiciones a esta
--funcion para que el programa ande correctamente.

--------------------------------------------------- Punto 2 ---------------------------------------------------
enfrentamiento :: Persona->Criatura->Persona
enfrentamiento persona criatura |tieneElementoNecesario (items persona) criatura = ganaExp persona (nivelPeligrosidad criatura)
                                |otherwise = ganaExp persona 1

tieneElementoNecesario :: Items->Criatura->Bool -- uso recursividad para no armar otra funcion para evitar usar otra funcion 
tieneElementoNecesario [] _ = False
tieneElementoNecesario (x:xs) criatura = any ( == x ) (criterios criatura) || tieneElementoNecesario xs criatura
-- si el primer item no cumple toma el segundo y asi hasta que se acaba la lista. si esta vacia es porque no lo tiene

ganaExp :: Persona->Int->Persona
ganaExp persona xpGanada = persona{xp = (xp persona) + xpGanada}
--------------------------------------------------- Punto 3 ---------------------------------------------------
grupoDeCriaturas = [siempreDetras,grupoDeGnomos,fantasma]

enfrentamientoSucesivo :: Persona->[Criatura]->Int
enfrentamientoSucesivo persona listaCriaturas = (diferenciaXp persona.foldl enfrentamiento persona) listaCriaturas
-- le aplico la funcion enfrentamiento y persona a cada criatura y al resultado le calculo la diferencia con el del principio
diferenciaXp :: Persona->Persona->Int
diferenciaXp persona personaFinal = (xp personaFinal) - (xp persona)