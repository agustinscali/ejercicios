import Text.Show.Functions 
import Distribution.Simple.Setup (trueArg)

data Participante = UnParticipante {
    nombre :: String,
    trucosDeCocina :: [Truco],
    platoEspecial :: Plato
} deriving Show

data Plato = UnPlato {
    dificultad :: Int,
    componentes :: [Componente]
} deriving Show

type Truco = Plato -> Plato 
type Componente = (Ingrediente,Gramos)
type Ingrediente = String
type Gramos = Int

-- Parte A

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes unaFuncion unPlato = unPlato {componentes = unaFuncion.componentes $ unPlato }

endulzar :: Gramos -> Truco
endulzar gramosDeAzucar unPlato = agregarIngrediente "Azucar" gramosDeAzucar unPlato

agregarIngrediente :: Ingrediente -> Gramos -> Plato -> Plato
agregarIngrediente nombreIngrediente gramos unPlato = modificarComponentes ((nombreIngrediente, gramos) :) unPlato

salar :: Gramos -> Truco
salar gramos unPlato = agregarIngrediente "Sal" gramos unPlato

darSabor :: Gramos -> Gramos -> Truco 
darSabor gramosDeSal gramosDeAzucar = salar gramosDeSal. endulzar gramosDeAzucar

duplicarPorcion :: Truco
duplicarPorcion unPlato = modificarComponentes (map duplicarCantidad) unPlato

duplicarCantidad :: Componente -> Componente 
duplicarCantidad (string, cantidad) = (string, cantidad * 2)

simplificar :: Truco
simplificar unPlato | esDificil unPlato = bajarDificultad unPlato
                    | otherwise = unPlato 

esDificil :: Plato -> Bool
esDificil unPlato = dificultad unPlato > 7 && length (componentes unPlato) > 5

bajarDificultad :: Truco
bajarDificultad unPlato = modificarDificultad (const 5).modificarComponentes (filter (not.llevaPoco)) $ unPlato 

modificarDificultad :: (Int -> Int) -> Plato -> Plato
modificarDificultad unaFuncion unPlato = unPlato {dificultad = unaFuncion.dificultad $ unPlato }

llevaPoco :: Componente -> Bool
llevaPoco (_,gramos) = gramos<10

esVegano :: Plato -> Bool
esVegano unPlato = not $ any (alimentosNoVeganos.fst) (componentes unPlato)

alimentosNoVeganos :: Ingrediente -> Bool
alimentosNoVeganos "carne" = True
alimentosNoVeganos "huevo" = True 
alimentosNoVeganos "lacteos" = True
alimentosNoVeganos _ = False

-- prueba 

fideos :: Plato
fideos = UnPlato 2 [("harina",5),("tuco",4)]

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not $ any ((== "harina").fst) (componentes unPlato)

esComplejo :: Plato -> Bool 
esComplejo  = esDificil

noAptoHipertension :: Plato -> Bool 
noAptoHipertension unPlato = algunoTieneMasDeDosGramos (filter ((=="sal").fst) (componentes unPlato))

algunoTieneMasDeDosGramos :: [Componente] -> Bool
algunoTieneMasDeDosGramos = any ((>2).snd)

-- Parte B

pepeRonccino :: Participante
pepeRonccino = UnParticipante "Pepe Ronccino" [darSabor 2 5,simplificar,duplicarPorcion] platoComplejo

platoComplejo :: Plato
platoComplejo = UnPlato 10 [("sal",100),("sal",100),("sal",100),("sal",100),("sal",100),("sal",100)]

-- Parte C

cocinar :: Participante -> Plato
cocinar unParticipante = foldr ($) (platoEspecial unParticipante) (trucosDeCocina unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = dificultad unPlato > dificultad otroPlato && sumaDePesos unPlato < sumaDePesos otroPlato

sumaDePesos :: Plato -> Int 
sumaDePesos unPlato = sum (map snd (componentes unPlato))

participanteEstrella :: [Participante] -> [Participante]
participanteEstrella [unParticipante] = [unParticipante] 
participanteEstrella (participante1:participante2:cola) 
    | esMejorQue (platoDe participante1) (platoDe participante2) = (participante1:cola)
    | otherwise = (participante2:cola)

platoDe :: Participante -> Plato
platoDe = platoEspecial

-- Parte D

platinum :: Plato
platinum = UnPlato {
    dificultad = 10,
    componentes = infinitosComponentes
} 

infinitosComponentes :: [Componente]
infinitosComponentes = map componentesPlatinum [1..]

componentesPlatinum :: Int -> Componente
componentesPlatinum unNumero = ("Ingrediente " ++ show unNumero, unNumero)

--- ¿Qué sucede si aplicamos cada uno de los trucos modelados en la Parte A al platinum?
{- en todas las funciones de la parte A sucede que 

-}