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

data Criatura = UnSiempreDetras{criterios :: Criterio}
               |UnosGnomos{cantidad:: Int,criterios :: Criterio}
               |UnFantasma{categoria :: Int,criterios :: Criterio}deriving (Show)

---------------------------------- Ejemplos criaturas ----------------------------------

siempreDetras = UnSiempreDetras [] -- el 0 es la peligrosidad que le paso a la funcion nivelPeligrosidad
grupoDeGnomos = UnosGnomos 20 ["soplador de hojas"]
fantasma = UnFantasma 

nivelPeligrosidad :: Criatura->Int
nivelPeligrosidad (UnSiempreDetras _) = 0
nivelPeligrosidad (UnosGnomos cantidad _) = 2^cantidad
nivelPeligrosidad (UnFantasma categoria  _) = 20* categoria

--------------------------------------------------- Punto 2 ---------------------------------------------------
type Enfrentamiento = Persona->Criatura->Persona
enfrentamiento :: Enfrentamiento
enfrentamiento persona criatura |tieneElementoNecesario (items persona) criatura = ganaExp persona (nivelPeligrosidad criatura)
                                |otherwise = ganaExp persona 1

tieneElementoNecesario :: Items->Criatura->Bool -- uso recursividad para no armar otra funcion para evitar usar otra funcion 
tieneElementoNecesario [] _ = False
tieneElementoNecesario (x:xs) criatura = any ( == x ) (criterios criatura) || tieneElementoNecesario xs criatura
-- si el primer item no cumple toma el segundo y asi hasta que se acaba la lista. si esta vacia es porque no lo tiene

ganaExp :: Persona->Int->Persona
ganaExp persona xpGanada = persona{xp = (xp persona) + xpGanada}