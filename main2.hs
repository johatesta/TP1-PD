
import System.IO 
import Data.List 
import Horas
import Graphics.Win32.Control (createListBox)

main :: IO ()
main = do
  archivo <- openFile "09septiembre2019.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter filtrarRenglones lineas
      tuplas = creaLista registros [""] 0
      empleados = length  $filter filtraEmpleados lineas
      totaldehoras = snd tuplas / 60
      promedioHorasHombre = totaldehoras / devuelveFloat empleados
      promediohorasdiarias = promedioHorasHombre / 20
  putStrLn (unlines (fst tuplas) ++ "\n" ++ show totaldehoras ++ "\n" ++ show promedioHorasHombre ++ "\n" ++ show promediohorasdiarias)
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
creaLista w s y = creaLista (tail w) (s ++ [formattedString]) (y + devuelveFloat (desnormalizar  substract))
  where
    substract = crearHoras 2 (head w)
    formattedString = filtrarCampo 0 (head w) ++ " " ++ filtrarCampo 1 (head w) ++ " " ++ show substract
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