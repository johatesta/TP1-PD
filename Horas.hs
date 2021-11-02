{-# LANGUAGE BlockArguments #-}
import System.IO (openFile, hGetContents, IOMode (ReadMode), hClose)
import Data.List (isPrefixOf)




module Tiempo 
( Hora,
  getHora,
  getMin,
  getRepMin,
  getRepNor,
  getDifHora,
  getDif
) where

data Hora = Hora Int Int deriving (Eq)


instance Show Hora where
  show (Hora hora minuto) = (if hora > 10
                             then (show hora)
                             else ("0" ++ show hora))
                            ++ ":" ++
                            (if minuto > 10
                             then (show minuto)
                             else ("0" ++ show minuto))


-- Get de Hora
getHora :: Hora -> Int
getHora (Hora h _) = h

-- Get de Min
getMin :: Hora -> Int
getMin (Hora _ m) = m

-- Convierte una hora y devuelve los minutos totales
getRepMin :: Hora -> Int
getRepMin (Hora h m) = h * 60 + m

-- Convierte minutos totales y devuelve la hora
getRepNor :: Int -> Hora
getRepNor m = Hora (m `div` 60) (m `mod` 60)

-- Devuelve la diferencia horaria entre dos horas en un formato Hora
getDifHora :: Hora -> Hora -> Hora
getDifHora (Hora h1 m1) (Hora h2 m2) 
  | h1 >= h2 = getRepNor $ getRepMin (Hora h1 m1) - getRepMin (Hora h2 m2)
  | otherwise = getRepNor $ getRepMin (Hora h2 m2) - getRepMin (Hora h1 m1) 

-- Devuelve la diferencia horaria entre dos horas en un formato Minutos totales
getDif :: Hora -> Hora -> Int
getDif (Hora h1 m1) (Hora h2 m2) 
  | h1 >= h2 = getRepMin (Hora h1 m1) - getRepMin (Hora h2 m2)
  | otherwise = getRepMin (Hora h2 m2) - getRepMin (Hora h1 m1) 



main = do
    archivo <- openFile "09septiembre2019.txt" ReadMode  
    contenido  <- hGetContents archivo
    let lineas = lines contenido
        registros = filter tomardias lineas
        fecha = map((!!1). words ) registros
        entrada = map (tomarCampo 2) registros
        salida  = map (tomarCampo 3) registros
        hingreso= map tomarHora entrada
        hegreso= map tomarHora salida
        mingreso = map timeToMinutes entrada
        megreso =  map timeToMinutes salida
        minutos= zipWith (-)  megreso mingreso 
        horas = zipWith (-) hegreso hingreso
        permanenciahoras = map show horas
        permanenciaMinutos = map show minutos
        imprimir = add1 fecha entrada salida permanenciaMinutos permanenciahoras  
    putStrLn  imprimir
    hClose archivo
----putStrLn $ show [1,2,3]
tomarCampo :: Int -> String -> [Char]
tomarCampo n = tail . init . (!! n) . words

{- Evita error cuando no hay registro -}
tomarHora :: [Char] -> Int
tomarHora "" = 0
tomarHora n = read (takeWhile (/= ':') n) :: Int



timeToMinutes :: [Char] -> Int
timeToMinutes "" = 0
timeToMinutes n = hours * 60 
    where hours = read (takeWhile (/= ':')  n) :: Int


{-
timeToMinutes 2-}

tomardias :: [Char ] -> Bool 
tomardias xs |"\"Lunes"    `isPrefixOf` xs = True 
             |"\"Martes"   `isPrefixOf` xs = True 
             |"\"Miercoles"`isPrefixOf` xs = True 
             |"\"Jueves"   `isPrefixOf` xs = True 
             |"\"Viernes"  `isPrefixOf` xs = True 
             |otherwise                     =False 


add :: [[Char]] -> [[Char]] -> [[Char]] -> [String] -> [Char] 
add (x:xs) (y:ys) (l:ls) (u:us)= x ++ " " ++ y ++ " " ++ l ++ " " ++ u ++ " " ++ "\n" ++ add xs ys ls us
add [] _ _ _ = []
add (_:_) [] _ _ = []
add (_:_)  (_:_) [] _ = []
add (_:_) (_:_) (_:_) [] = []

--funcion de stackoverflow, no imprime nada (por ahora, probar recursion)
{-add (x:xs) (y:ys) = zs ++ ws
    where
        zs = if elem x xs then xs
                          else x:xs
        ws = if elem y ys then ys
                          else y:ys-}    

add1 :: [[Char]] -> [[Char]] -> [[Char]] -> [String] -> [String]-> [Char] 
add1 (x:xs) (y:ys) (l:ls) (u:us) (a:as)= x ++ " " ++ y ++ " " ++ l ++ " " ++ u ++ " " ++ " "++ a ++" "++ "\n" ++ add1 xs ys ls us as
add1 [] _ _ _ _= []
add1 (_:_) [] _ _ _ = []
add1 (_:_)  (_:_) [] _ _ = []
add1 (_:_) (_:_) (_:_) [] _ = []      
add1 (_:_) (_:_) (_:_) (_:_) [] = []                      