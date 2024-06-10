import Text.Show.Functions
import Data.Char (toUpper)

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving Show

dave1 :: Barbaro
dave1 = UnBarbaro "Dave1" 100 ["tejer", "escribirPoesia"] [ardilla, varitasDefectuosas]

dave :: Barbaro
dave = UnBarbaro {
    nombre = "Dave",
    fuerza = 100,
    habilidades = ["tejer", "escribirPoesia"],
    objetos = [ardilla, varitasDefectuosas]
}

modificarFuerza :: (Int->Int) -> Barbaro -> Barbaro 
modificarFuerza unaFuncion unBarbaro = unBarbaro {fuerza = (unaFuncion.fuerza) unBarbaro}

modificarHabilidades :: ([String]->[String]) -> Barbaro -> Barbaro 
modificarHabilidades unaFuncion unBarbaro = unBarbaro {habilidades = (unaFuncion.habilidades) unBarbaro}

modificarObjetos ::([Objeto] -> [Objeto]) -> Barbaro -> Barbaro 
modificarObjetos unaFuncion unBarbaro = unBarbaro {objetos = (unaFuncion.objetos) unBarbaro}

modificarNombre :: (String -> String) -> Barbaro -> Barbaro
modificarNombre unaFuncion unBarbaro = unBarbaro {nombre = (unaFuncion.nombre) unBarbaro}

type Objeto = Barbaro -> Barbaro 
type Habilidad = String 

espadas :: Int -> Objeto 
espadas pesoDeLaEspada unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + pesoDeLaEspada * 2}

amuletosMisticos :: Habilidad -> Objeto 
amuletosMisticos unaHabilidad unBarbaro = unBarbaro {habilidades = habilidades unBarbaro ++ [unaHabilidad]}

varitasDefectuosas :: Objeto 
varitasDefectuosas unBarbaro = unBarbaro {habilidades = habilidades unBarbaro ++ ["hacerMagia"], objetos = []}

ardilla :: Objeto 
ardilla unBarbaro = unBarbaro

cuerda ::  Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto.otroObjeto

megafono :: Objeto 
megafono unBarbaro = unBarbaro {habilidades = [concatMap (map toUpper) (habilidades unBarbaro)]} 

megafonoBarbarico :: Objeto 
megafonoBarbarico = cuerda ardilla megafono 

type Aventura = [Evento] -> Barbaro -> Bool 
type Evento = Barbaro -> Bool 

invasionDeSuciosDuendes :: Evento 
invasionDeSuciosDuendes = tieneHabilidad "Escribir Poesia Atroz"

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = elem unaHabilidad (habilidades unBarbaro)

cremalleraDelTiempo :: Evento 
cremalleraDelTiempo unBarbaro = nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"

saqueo :: Evento 
saqueo unBarbaro = tieneHabilidad "robar" unBarbaro && fuerza unBarbaro > 80

gritoDeGuerra :: Evento 
gritoDeGuerra unBarbaro = poderGritoDeGuerra unBarbaro == cantidadDeLetrasDeHabilidades unBarbaro

poderGritoDeGuerra :: Barbaro -> Int 
poderGritoDeGuerra unBarbaro = length (objetos unBarbaro) * 4

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades = length.concat.habilidades 

caligrafia :: Evento 
caligrafia unBarbaro = ((3<).length.map esVocal.concat) (habilidades unBarbaro) && all empiezaConMayuscula (habilidades unBarbaro)

esVocal :: Char -> Bool
esVocal unChar = elem unChar "aeiouAEIOU"

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula unaCadena = head unaCadena == toUpper (head unaCadena)

 {-

ritualDeFechorias :: [Evento] -> Evento 
ritualDeFechorias eventos unBarbaro = pasaUnaAventura any unBarbaro eventos 

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> pasaUnaAventura all unBarbaro unaAventura) unosBarbaros

pasaUnaAventura criterio unBarbaro unaAventura = criterio (\evento -> evento unBarbaro) unaAventura

-}

sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (cabeza:cola) 
    | elem cabeza cola = cola 
    | otherwise = (cabeza:cola)

descendiente :: Barbaro -> Barbaro
descendiente  = utilizarObjetos.modificarNombre (++ "*").modificarHabilidades sinRepetidos 

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro )

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro