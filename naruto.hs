import Text.Show.Functions ()

data Ninja = UnNinja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [Poder],
    rango :: Int
}

type Poder = Mision -> Mision

data Herramienta = UnaHerramienta {
    nombreherramienta :: String,
    cantidad :: Int
} 

modificarHerramientas :: ([Herramienta]->[Herramienta]) -> Ninja -> Ninja
modificarHerramientas unaFuncion unNinja = unNinja {herramientas = (unaFuncion.herramientas) unNinja}

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta unaHerramienta unNinja | menorOIgualACienHerramientas unaHerramienta unNinja =  modificarHerramientas (++ [unaHerramienta]) unNinja
                                          | otherwise = modificarHerramientas (++ [adaptarCantidadDeHerramientas unaHerramienta unNinja]) unNinja 

menorOIgualACienHerramientas :: Herramienta -> Ninja -> Bool
menorOIgualACienHerramientas unaHerramienta unNinja = sumaDeHerramientas unNinja + cantidad unaHerramienta <= 100    

sumaDeHerramientas :: Ninja -> Int
sumaDeHerramientas unNinja = sum  (map cantidad (herramientas unNinja))    

adaptarCantidadDeHerramientas :: Herramienta -> Ninja -> Herramienta
adaptarCantidadDeHerramientas unaHerramienta unNinja = modificarCantidad (+(-(100-sumaDeHerramientas unNinja))) unaHerramienta

modificarCantidad :: (Int->Int) -> Herramienta -> Herramienta 
modificarCantidad unaFuncion unaHerramienta = unaHerramienta {cantidad = (unaFuncion.cantidad) unaHerramienta}

usarHerramienta :: Ninja -> Herramienta -> Ninja
usarHerramienta unNinja unaHerramienta = modificarHerramientas (filter((/= nombreherramienta unaHerramienta). nombreherramienta)) unNinja

--- PARTE B 

data Mision = UnaMision {
    cantidadDeNinjas :: Int,
    rangoRecomendable :: Int,
    enemigosADerrotar :: [Ninja],
    recompensa :: Herramienta
}

type Equipo = [Ninja]

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante unEquipo unaMision = any (rangoRecomendable unaMision<) (map rango unEquipo)


esCopada :: Mision -> Bool
esCopada unaMision = undefined

recompensasCopadas :: [Herramienta]
recompensasCopadas = [tresBombasDeHumo,cincoshurikens,catorcekunais]

tresBombasDeHumo :: Herramienta
tresBombasDeHumo = UnaHerramienta "Bomba De Humo" 3

cincoshurikens :: Herramienta 
cincoshurikens = UnaHerramienta "Shuriken" 5

catorcekunais :: Herramienta
catorcekunais = UnaHerramienta "kunai" 14


esFactible :: Equipo -> Mision -> Bool 
esFactible unEquipo unaMision = not (esDesafiante unEquipo unaMision) && (sumaDeHerramientasDelEquipo unEquipo > 500 || length unEquipo >= length (enemigosADerrotar unaMision))

sumaDeHerramientasDelEquipo :: Equipo -> Int
sumaDeHerramientasDelEquipo unEquipo = sum (map cantidad (foldr1 (++) (map herramientas unEquipo)))

fallarMision :: Equipo -> Mision -> Equipo 
falarMision [] _ = []
fallarMision (ninja1:cola) unaMision | rango ninja1 >= (rangoRecomendable unaMision) = (modificarRango (+(-2)) ninja1:cola)
                                            | otherwise = (cola)

modificarRango :: (Int->Int) -> Ninja -> Ninja
modificarRango unaFuncion unNinja = unNinja {rango = (unaFuncion.rango) unNinja}

cumplirMision :: Equipo -> Mision -> Equipo 
cumplirMision [] _ = []
cumplirMison (ninja1:cola) unaMision = ((aplicarRecompensa.aplicarRango) ninja1:cola)
                                     
aplicarRecompensa :: Ninja -> Ninja
aplicarRecompensa unNinja = undefined

aplicarRango :: Ninja -> Ninja
aplicarRango = modificarRango (+1)

ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo  = completarMision unEquipo. usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision | esFactible unEquipo unaMision || esCopada unaMision = cumplirMision unEquipo unaMision
                                   | otherwise = fallarMision unEquipo unaMision 

granGuerraNinja :: Mision
granGuerraNinja = UnaMision {
    cantidadDeNinjas = 100000,
    rangoRecomendable = 600,
    enemigosADerrotar = infinitosZetsus,
    recompensa = abanicodeMadaraUchiha
}  

abanicodeMadaraUchiha :: Herramienta
abanicodeMadaraUchiha = UnaHerramienta {
    nombreherramienta = "Abanico De Madara Uchiha",
    cantidad = 1 
}

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu unNumero = UnNinja {
    nombre = "Zetsu " ++ show unNumero,
    rango = 600,
    jutsus = [],
    herramientas = []
}


