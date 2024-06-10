import Text.Show.Functions
import Data.List (union)

data Chico = UnChico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving Show

type Habilidad = String
type Deseo = Chico -> Chico 

-- A 

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = unChico {habilidades = habilidades unChico ++ unasHabilidades}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = aprenderHabilidades grosoEnNeedForSpeed

grosoEnNeedForSpeed :: [Habilidad]
grosoEnNeedForSpeed = map jugarNeedForSpeed [1..]

jugarNeedForSpeed :: Int -> Habilidad
jugarNeedForSpeed unNumero = "jugar need for speed " ++ show unNumero 

serMayor :: Deseo 
serMayor  = modificarEdad (const 18)

wanda :: Chico -> Chico 
wanda = cumplirPrimerDeseo.modificarEdad (+1)

cumplirPrimerDeseo :: Chico -> Chico 
cumplirPrimerDeseo unChico = head (deseos unChico) unChico

modificarEdad :: (Int -> Int) -> Chico -> Chico
modificarEdad unaFuncion unChico = unChico {edad = unaFuncion.edad $ unChico}

cosmo :: Chico -> Chico 
cosmo = modificarEdad (div 2)

muffinMagico :: Chico -> Chico 
muffinMagico unChico = foldr ($) unChico (deseos unChico)

-- B

tieneHabilidad :: Habilidad -> CondicionParaElegirChico
tieneHabilidad unaHabilidad unChico = elem unaHabilidad $ habilidades unChico

esSuperMaduro :: CondicionParaElegirChico
esSuperMaduro unChico = tieneHabilidad "sabe manejar" unChico && esMayor unChico

esMayor :: Chico -> Bool
esMayor = (>18).edad

data Chica = UnaChica {
    nombreChica :: String,
    condicion :: CondicionParaElegirChico
} deriving Show

type CondicionParaElegirChico = Chico -> Bool 

quienConquistaA :: Chica -> [Chico] -> Chico 
quienConquistaA unaChica (pretendiente1:cola) | elChicoTieneLasCondiciones pretendiente1 unaChica = pretendiente1 
                                              | otherwise = quienConquistaA unaChica cola
                                                
elChicoTieneLasCondiciones :: Chico -> Chica -> Bool 
elChicoTieneLasCondiciones unChico unaChica =   condicion unaChica unChico                              

nuevaChica = UnaChica "Chica de prueba" (tieneHabilidad "sabe cocinar")  

-- quienConquistaA nuevaChica listaDePretendientes

-- C

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules (chico1:cola) | (/=) (union deseosProhibidos (take 5 $ habilidades chico1)) [] = nombre chico1 : infractoresDeDaRules cola
                                   | otherwise = infractoresDeDaRules cola
deseosProhibidos :: [Habilidad]
deseosProhibidos = ["enamorar","matar","dominar el mundo"]

-- D



