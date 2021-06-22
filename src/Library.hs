module Library where
import PdePreludat


-- Punto 1
type Condicion = Viaje -> Bool
type Fecha = (Number, Number, Number)

data Chofer = Chofer {
    nombre      :: String,
    kilometraje :: Number,
    viajes      :: [Viaje],
    condicion   :: Condicion
} deriving (Show)

data Viaje = Viaje {
    fecha   :: Fecha,
    cliente :: Cliente,
    costo   :: Number
} deriving (Show)

data Cliente = Cliente {
    nombreCliente :: String,
    residencia    :: String 
} deriving (Show)

-- Punto 2
cualquierViaje :: Condicion
cualquierViaje _ = True

viajesCaros :: Condicion
viajesCaros = (>=200) . costo

tieneCantidadDeLetras :: Number -> Condicion
tieneCantidadDeLetras cantidad = (>cantidad) . length . nombreCliente . cliente

noVivenEn :: String -> Condicion
noVivenEn lugar = (/= lugar) . residencia . cliente

-- Punto 3
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje (20, 04, 2017) lucas 150] (noVivenEn "Olivos")

alejandra :: Chofer
alejandra = Chofer "Alejndra" 180000 [] cualquierViaje

-- Punto 4
puedeTomarElViaje :: Viaje -> Chofer -> Bool
puedeTomarElViaje viaje chofer = condicion chofer $ viaje

-- Punto 5
liquidacionChofer :: Chofer -> Number
liquidacionChofer chofer = foldr ((+) . costo) 0 (viajes chofer)

-- Punto 6
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = agregarViaje viaje . menosViajesTiene . puedenViajar viaje 

-- a)
puedenViajar :: Viaje -> [Chofer] -> [Chofer]
puedenViajar viaje choferes = filter (puedeTomarElViaje viaje) choferes

-- b)
menosViajesTiene :: [Chofer] -> Chofer
menosViajesTiene [chofer] = chofer
menosViajesTiene (primerChofer:segundoChofer:restoChoferes) = 
    menosViajesTiene ((elQueMenosViajesTiene primerChofer segundoChofer):restoChoferes)

elQueMenosViajesTiene :: Chofer -> Chofer -> Chofer
elQueMenosViajesTiene primerChofer segundoChofer
 | cantidadViajes primerChofer < cantidadViajes segundoChofer = primerChofer
 | otherwise                                                  = segundoChofer

cantidadViajes :: Chofer -> Number
cantidadViajes = length . viajes

-- c)
agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer {
    viajes = viajes chofer ++ [viaje]
}

-- Punto 7
-- a)
nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repetirViaje (Viaje (11, 03, 2017) lucas 50)) (tieneCantidadDeLetras 3)

repetirViaje viaje = viaje : repetirViaje viaje

-- b) 
-- liquidacionChofer nito ... no termina nunca!!
-- c) 
-- puedeTomarViaje (Viaje (2,5,2017) lucas 50) nito
-- True
-- porque no involucra a la lista de viajes

-- Punto 8
-- gongNeng Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
-- gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3
