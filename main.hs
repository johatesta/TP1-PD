
import System.IO 
import Data.List 
import Horas

main :: IO ()
main = menuLoop

-- Diplay menu
menu = do
    putStrLn "\nSistema de empleados"
    putStrLn "[1] Datos de cada registro"
    putStrLn "[2] Cantidad de empleados"
    putStrLn "[3] Cantidad de horas trabajadas en el mes"
    putStrLn "[4] Cantidad de horas/hombre en el mes por todos los empledos"
    putStrLn "[5] Promedio diario de horas/hombre"
    putStrLn "[6] Salir"
    putStrLn "Por favor elija una opcion"
    getLine


-- Exit option
exitMenu :: IO ()
exitMenu = putStrLn "Hasta pronto"

-- Pattern matching of options with its output
menuSelection :: String -> IO ()
menuSelection "1" = registros >> menu >>= menuSelection
menuSelection "2" = totalempe >> menu >>= menuSelection
menuSelection "3" = hsacumladas  >> menu >>= menuSelection
menuSelection "4" = canthorashombre >> menu >>= menuSelection
menuSelection "5" = promedio >> menu >>= menuSelection
menuSelection "6" = exitMenu

-- Recursive menu
menuLoop = menu >>= menuSelection
registros:: IO()
registros = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length  $filter filtraEmpleados lineas
      totaldehoras = snd tuplas / 60
      promedioHorasHombre = totaldehoras / devuelveFloat empleados
      promediohorasdiarias = promedioHorasHombre / 20
  putStrLn (unlines (fst tuplas) )
  hClose archivo
  
-- Total average function
totalempe :: IO()
totalempe  = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length $filter filtraEmpleados lineas
      cantidadEmpleados = ["Total de empleados listados: " ++ show cantidadEmpleados] 
      totaldehoras = snd tuplas / 60
     -- promedioHorasHombre = totaldehoras / devuelveFloat empleados
     -- promediohorasdiarias = promedioHorasHombre / 20
      informe= cantidadEmpleados
  putStrLn $unlines informe
  hClose archivo

hsacumladas :: IO()
hsacumladas = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length  $filter filtraEmpleados lineas
      totaldehoras = snd tuplas / 60
      imprimetotalhoras= ["Cantidad de horas trabajadas" ++ show totaldehoras ++ "Horas"]
      promedioHorasHombre = totaldehoras / devuelveFloat empleados
      promediohorasdiarias = promedioHorasHombre / 20
      informe= imprimetotalhoras
  putStrLn $unlines informe
  hClose archivo


canthorashombre:: IO()
canthorashombre = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length  $filter filtraEmpleados lineas
      totaldehoras = snd tuplas / 60
      promedioHorasHombre = totaldehoras / devuelveFloat empleados
      imprimehorasHombre= ["Cantidad de Horas/Hombre" ++ show promedioHorasHombre ++ "Horas"]
      promediohorasdiarias = promedioHorasHombre / 20
      informe= imprimehorasHombre
  putStrLn $unlines informe
  hClose archivo

    

promedio :: IO()
promedio = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length  $filter filtraEmpleados lineas
      totaldehoras = snd tuplas / 60
      promedioHorasHombre = totaldehoras / devuelveFloat empleados
      promediohorasdiarias = promedioHorasHombre / 20
      imprimehorasdiarias= ["Promedio de horas diarias" ++ show promediohorasdiarias ++ "Horas"]
      informe= imprimehorasdiarias

  putStrLn $unlines informe
  hClose archivo



filtrarRenglones :: String -> Bool
filtrarRenglones str | str == "\r" = False
                     | otherwise = (!! 0) (words str) `elem` ["\"Lunes\"", "\"Martes\"", "\"Miercoles\"", "\"Jueves\"", "\"Viernes\""]
--Quitamos caracteres que sobran para una mejor lectura
filtrarCampo :: Int -> String -> String
filtrarCampo n = filter (`notElem` "\"") . (!! n) . words

--Filtramos los registros que tiene la cantidad de empleados
filtraEmpleados :: String -> Bool
filtraEmpleados str
                    | str == "\r" = False
                    | otherwise = (!! 0) (words str) == "\"Empleado:\""
{-Creamos la lista a mostrar-}
creaLista :: [String] -> [String] -> Float -> ([String], Float )
creaLista [] s y = (s, y)
creaLista w s y = creaLista (tail w) (s ++ [formatea]) (y + devuelveFloat (desnormalizar  desnormali))
  where
    desnormali = crearHoras 2 (head w)
    formatea = filtrarCampo 0 (head w) ++ " " ++ filtrarCampo 1 (head w) ++ " " ++ show desnormali
--esta funcion hace que los resultados sean float
devuelveFloat :: Int -> Float
devuelveFloat n = read $ show n :: Float
--
crearHoras :: Int -> String -> Hora
crearHoras i n
  | filtrarCampo i n /= "" && filtrarCampo (i + 1) n /= "" = diferenciaHoraria (tomaHorasandMinutos (filtrarCampo (i + 1) n)) (tomaHorasandMinutos (filtrarCampo i n))
  | i /= 10 = crearHoras (i + 2) n
  | otherwise = tomaHorasandMinutos "00:00"
--Filtra todo lo que no es fecha
filtrarfechas :: [String] -> String -> Bool 
filtrarfechas [] x = False
filtrarfechas i x = head i `isPrefixOf` x || filtrarfechas (tail i) x

--funcion unificada de los minutos y las horas
tomaHorasandMinutos :: [Char] -> Hora
tomaHorasandMinutos "" = Hora (-1) (-1)
tomaHorasandMinutos n = Hora (read $ takeWhile (/= ':') n :: Int) (read $ reverse $ takeWhile (/= ':') $ reverse n :: Int)
--calcula la diferencia horaria
diferenciaHoraria :: Hora -> Hora -> Hora
diferenciaHoraria  (Hora x y) (Hora a b) = normalizar  (desnormalizar  (Hora x y) - desnormalizar  (Hora a b))