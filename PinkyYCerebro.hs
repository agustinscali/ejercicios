import Text.Show.Functions
import Data.List (union)

data Animal = UnAnimal {
    coeficienteIntelectual :: IQ,
    especie :: Especie,
    capacidades :: [Capacidad]
} deriving Show

type IQ = Int 
type Especie = String
type Capacidad = String

type Transformacion = Animal -> Animal

modificarIQ :: (Int->Int) -> Animal -> Animal
modificarIQ unaFuncion unAnimal = unAnimal {coeficienteIntelectual = unaFuncion.coeficienteIntelectual $ unAnimal} 

modificarCapacidades :: ([Capacidad]->[Capacidad]) -> Animal -> Animal
modificarCapacidades unaFuncion unAnimal = unAnimal {capacidades = unaFuncion.capacidades $ unAnimal}
 
inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior unNumero unAnimal = modificarIQ (+unNumero) unAnimal

pinkificar :: Transformacion
pinkificar = modificarCapacidades (const [])

superPoderes :: Transformacion
superPoderes unAnimal | esUnaEspecie "elefante" unAnimal = modificarCapacidades (++ ["no tenerle miedo a los ratones"]) unAnimal 
                      | esUnaEspecie "raton" unAnimal && coeficienteIntelectual unAnimal > 100 = modificarCapacidades (++ ["hablar"]) unAnimal
                      | otherwise = unAnimal

esUnaEspecie :: Especie -> Animal -> Bool
esUnaEspecie unaEspecie unAnimal = especie unAnimal == unaEspecie

--3

type CriterioDeExito = Animal -> Bool

antropomorfico :: CriterioDeExito
antropomorfico unAnimal = tieneHabilidad "hablar" unAnimal && coeficienteIntelectual unAnimal > 60

tieneHabilidad :: Capacidad -> Animal -> Bool 
tieneHabilidad unaCapacidad unAnimal = elem unaCapacidad (capacidades unAnimal)

noTanCuerdo :: CriterioDeExito
noTanCuerdo unAnimal = length (habilidadesPinkiescas unAnimal) >= 2

pinkiesco :: Capacidad -> Bool
pinkiesco unaCapacidad = (take 5 unaCapacidad) == "hacer" && esPinkiesca (drop 6 unaCapacidad)

esPinkiesca :: String -> Bool
esPinkiesca unaPalabra = length unaPalabra <= 4 && hayVocal unaPalabra

hayVocal :: String -> Bool 
hayVocal = any esVocal 

esVocal :: Char -> Bool 
esVocal unChar = elem unChar "aeiouAEIOU"

habilidadesPinkiescas :: Animal -> [Capacidad] 
habilidadesPinkiescas unAnimal = filter pinkiesco (capacidades unAnimal)

-- 4

type Experimento = ([Transformacion], CriterioDeExito)

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso (listaDeTransformaciones,criterioDeExito) unAnimal = criterioDeExito $ aplicarTransformacion unAnimal listaDeTransformaciones

aplicarTransformacion :: Animal -> [Transformacion] -> Animal
aplicarTransformacion  = foldr ($) 

raton1 :: Animal
raton1 = UnAnimal 17 "raton" ["destruenglonir el mundo", "hacer planes desalmados"]

experimento1 :: Experimento
experimento1 = ([pinkificar,inteligenciaSuperior 10,superPoderes],antropomorfico)

-- experimentoExitoso experimento1 raton1 

-- 5

reporte1 :: [Animal] -> [Capacidad] -> [Transformacion] -> [Int]
reporte1 = crearReporte coeficienteIntelectual tieneCapacidad
--   map coeficienteIntelectual (filter (tieneCapacidad listaDeCapacidades) (listaDeAnimalesTransformados listaAnimales listaDeTransformaciones))

tieneCapacidad :: [Capacidad] -> Animal -> Bool
tieneCapacidad listaCapacidades unAnimal = (/=) (union (capacidades unAnimal) listaCapacidades) []
 
listaDeAnimalesTransformados :: [Animal] -> [Transformacion] -> [Animal]
listaDeAnimalesTransformados (animal1:cola) listaDeTransformaciones = foldr ($) animal1 listaDeTransformaciones : listaDeAnimalesTransformados cola listaDeTransformaciones

reporte2 :: [Animal] -> [Capacidad] -> [Transformacion] -> [Especie]
reporte2  = crearReporte especie tieneTodasLasCapacidades 
--    map especie (filter (tieneTodasLasCapacidades listaDeCapacidades) (listaDeAnimalesTransformados listaAnimales listaDeTransformaciones))

tieneTodasLasCapacidades :: [Capacidad] -> Animal -> Bool
tieneTodasLasCapacidades listaDeCapacidades unAnimal = capacidades unAnimal `union` listaDeCapacidades == listaDeCapacidades

reporte3 :: [Animal] -> [Capacidad] -> [Transformacion] -> [Int]
reporte3  = crearReporte (length.capacidades) noTieneCapacidad
--  map (length.capacidades) (filter (not.tieneCapacidad listaDeCapacidades) (listaDeAnimalesTransformados listaAnimales listaDeTransformaciones))

noTieneCapacidad :: [Capacidad] -> Animal -> Bool
noTieneCapacidad listaCapacidades = not.tieneCapacidad listaCapacidades

crearReporte :: (Animal -> b) -> (t -> Animal -> Bool) -> [Animal] -> t -> [Transformacion] -> [b]
crearReporte funcionParaLoQueSeBuscaRetornar funcionQueFiltraLosAnimalesTransformados listaDeAnimales listaDeCapacidades listaDeTransformaciones 
 = (map funcionParaLoQueSeBuscaRetornar.filter (funcionQueFiltraLosAnimalesTransformados listaDeCapacidades)) $ listaDeAnimalesTransformados listaDeAnimales listaDeTransformaciones

nuevoAnimal = UnAnimal 0 