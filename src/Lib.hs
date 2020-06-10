module Lib where
import Text.Show.Functions
laVerdad = True

--------------------------------------------------- Parte 1 ---------------------------------------------------
data Persona = UnaPersona{
    edad :: Int,
    items :: Items,
    exp :: Int
}deriving (Show,Eq)

type Items = [String]
type Criterio = [String]

data Criatura = UnSiempreDetras{ criterios :: Criterio}
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