type Punto = (Int,Int)
type Transparencia = Int
data Poligono = Poligono { puntos :: [Punto], alfa :: Transparencia } deriving(Show)

type Transformacion = Poligono -> Poligono

cuadrado = Poligono [(0,0),(0,1),(1,1),(1,0)] 20

--EJERCICIO 1
desplazar :: [Punto] -> Transformacion
desplazar listaPuntos unPoligono = unPoligono { puntos = listaPuntos }

espejar :: Transformacion
espejar unPoligono = unPoligono { puntos = (map (\(puntox,puntoy) -> ( ((*) (-1)) puntox, ((*) (-1)) puntoy )).puntos) unPoligono }

aclararEn :: Int -> Transformacion
aclararEn = operarAlfa (-) 

oscurecerEn :: Int -> Transformacion 
oscurecerEn = operarAlfa (+)

operarAlfa :: (Int->Int->Int) -> Int -> Transformacion
operarAlfa funcion operando unPoligono
 | (funcion.alfa) unPoligono operando >= 0 &&  (funcion.alfa) unPoligono operando <= 255 = unPoligono { alfa = (funcion.alfa) unPoligono operando }
 | (funcion.alfa) unPoligono operando < 0 = unPoligono { alfa = 0 }
 | otherwise = unPoligono { alfa = 255 }

simplificar :: Int -> Transformacion
simplificar numero unPoligono = unPoligono { puntos = (drop numero.puntos) unPoligono }

--EJERCICIO 2
barrelRoll :: Int -> Transformacion -> Transformacion
barrelRoll cantidadVeces = foldl1 (.).replicate cantidadVeces

pipelineCaprichoso :: [Transformacion] -> Transformacion
pipelineCaprichoso [] unPoligono = unPoligono
pipelineCaprichoso (x:xs) unPoligono 
 | (any (==(0,0)).puntos.x) unPoligono = pipelineCaprichoso xs unPoligono
 | otherwise = (pipelineCaprichoso xs.x) unPoligono

restar1AlYDel2doPunto :: Transformacion
restar1AlYDel2doPunto unPoligono = unPoligono { puntos = (take 1.puntos) unPoligono ++ map (\(x,y) -> (x,y-1)) ((tail.take 2.puntos) unPoligono) ++ (drop 2.puntos) unPoligono }

--PUNTO 3
esInutil :: Transformacion -> Poligono -> Bool
esInutil unaTransformacion unPoligono = (==) (puntos unPoligono) ((puntos.unaTransformacion) unPoligono) && (==) (alfa unPoligono) ((alfa.unaTransformacion) unPoligono)

transformacionesUtiles :: Poligono -> [Transformacion] -> [Transformacion]
transformacionesUtiles unPoligono = filter (\unaTransformacion -> ((not.esInutil unaTransformacion) unPoligono))

--PUNTO 4
-- A
type Animacion = Poligono -> [Poligono]

animacion :: (Int->Int) -> (Int->Int) -> (Int->Int) -> Animacion
animacion funcionAlfa funcionx funciony unPoligono = iterate (modificarComponentes funcionAlfa funcionx funciony) unPoligono

modificarComponentes :: (Int->Int) -> (Int->Int) -> (Int->Int) -> Transformacion
modificarComponentes funcionAlfa funcionx funciony unPoligono = (modificarAlfa funcionAlfa.operarPuntos funcionx funciony) unPoligono 

operarPuntos :: (Int->Int) -> (Int->Int) -> Transformacion
operarPuntos funcionx funciony unPoligono = unPoligono { puntos = (map (\(x,y)->(funcionx x,funciony y)).puntos) unPoligono }

modificarAlfa :: (Int->Int) -> Transformacion
modificarAlfa funcionAlfa unPoligono = unPoligono { alfa = (funcionAlfa.alfa) unPoligono }

--B
pasearPorLasX :: Animacion
pasearPorLasX unPoligono = animacion (*1) (+1) (*1) unPoligono