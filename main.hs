
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
menuSelection "3" = hsacumuladas  >> menu >>= menuSelection
menuSelection "4" = canthorashombre >> menu >>= menuSelection
menuSelection "5" = promedio >> menu >>= menuSelection
menuSelection "6" = exitMenu

-- Recursive menu
menuLoop = menu >>= menuSelection
registros:: IO()
registros = do
    archivo <- openFile "09septiembre2019.txt" ReadMode
    contenido  <- hGetContents archivo
    let lineas = lines contenido
        registros = filter filtrarRenglones lineas
        horarios = map words registros
        filtrahorarios = map (map (filter (`notElem` "\"\""))) horarios
        filtraregistro = map hacerTuplas filtrahorarios
        separafecha = map (unirStrings . take 1) tuplas
        horarios = map (tomarTuplas . drop 1) tuplas
        horasSumadas = map (map sumarHoras) horarios
        horasAcumuladas = map sum horasSumadas
        horasNormalizadas = map getDifeHora horasAcumuladas
        stringHoras = map show horasNormalizadas
        resultado = zip diaFecha stringHoras
        registrosEmpleados = map mostrarResultado resultado
        mostrar = registrosEmpleados
    putStrLn $ unlines mostrar
    hClose archivo

-- Total average function
totalempe :: IO()
totalempe  = do
 archivo <- openFile "09septiembre2019.txt" ReadMode
 contenido  <- hGetContents archivo
let lineas = lines contenido
    registros = filter filtrarRenglones lineas
    horarios = map words registros
    filtrahorarios= map (map (filter (`notElem` "\"\""))) horarios
    tuplas = map crealista filtrahorarios
    cantidadEmpleados = filter buscarEmpleados lineas
    totalEmpleados = read $ filter (`elem` "0123456789") (unwords cantidadEmpleados) :: Int
    imprimeEmpleados = ["\n" ++ filter (`notElem` "\"\"") (unwords cantidadEmpleados)] 
    mostrar = stringRegistrosEmpleados
    putStrLn $ unlines mostrar
     hClose archivo


hsacumladas :: IO()
hsacumladas = do
    archivo <- openFile "09septiembre2019.txt" ReadMode
    contenido  <- hGetContents archivo
    let lineas = lines contenido
        registros = filter filtrarRenglones lineas
        horarios = map words registros
        filtrahorarios = map (map (filter (`notElem` "\"\""))) horarios
        filtraregistro = map hacerTuplas filtrahorarios
        separafecha = map (unirStrings . take 1) tuplas
        horarios = map (tomarTuplas . drop 1) tuplas
        horasSumadas = map (map sumarHoras) horarios
        horasAcumuladas = map sum horasSumadas
        horasNormalizadas = map getDifeHora horasAcumuladas
        stringHoras = map show horasNormalizadas
        resultado = zip diaFecha stringHoras
        registrosEmpleados = map mostrarResultado resultado
        tomaHorasAcumuladas = fromIntegral (sum horasAcumuladas) / 60
        imprimeHorasAcumuladas =  ["\nHoras Acumuladas en el mes: " ++ show horasAcumuladas ++ " Horas"]
        mostrar = imprimeHorasAcumuladas
    putStrLn $ unlines mostrar
     hClose archivo 

-- Weekly average output
canthorashombre :: IO()
canthorashombre x = do
        archivo <- openFile "09septiembre2019.txt" ReadMode
    contenido  <- hGetContents archivo
    let lineas = lines contenido
        registros = filter filtrarRenglones lineas
        horarios = map words registros
        filtrahorarios = map (map (filter (`notElem` "\"\""))) horarios
        filtraregistro = map hacerTuplas filtrahorarios
        separafecha = map (unirStrings . take 1) tuplas
        horarios = map (tomarTuplas . drop 1) tuplas
        horasSumadas = map (map sumarHoras) horarios
        horasAcumuladas = map sum horasSumadas
        horasNormalizadas = map getDifeHora horasAcumuladas
        stringHoras = map show horasNormalizadas
        resultado = zip diaFecha stringHoras
        registrosEmpleados = map mostrarResultado resultado
        tomaHorasAcumuladas = fromIntegral (sum horasAcumuladas) / 60
        cantidadEmpleados = filter buscarEmpleados lineas
        totalEmpleados = read $ filter (`elem` "0123456789") (unwords cantidadEmpleados) :: Int
        horasHombre =  tomaHorasAcumuladas / fromIntegral cantidadEmpleados
        imprimehorasHombre = ["\n Horas/hombre en el mes: " ++ show floatHorasHombre ++ " Horas"]
        mostrar = imprimehorasHombre
    putStrLn $ unlines mostrar
     hClose archivo 
        

        

promedio :: IO()
promedio = do
    -- Leemos el archivo 09septiembre2019.txt y lo guardamos en la variable lineas
        archivo <- openFile "09septiembre2019.txt" ReadMode
    contenido  <- hGetContents archivo
    let lineas = lines contenido
        registros = filter filtrarRenglones lineas
        horarios = map words registros
        filtrahorarios = map (map (filter (`notElem` "\"\""))) horarios
        filtraregistro = map hacerTuplas filtrahorarios
        separafecha = map (unirStrings . take 1) tuplas
        horarios = map (tomarTuplas . drop 1) tuplas
        horasSumadas = map (map sumarHoras) horarios
        horasAcumuladas = map sum horasSumadas
        horasNormalizadas = map getDifeHora horasAcumuladas
        stringHoras = map show horasNormalizadas
        resultado = zip diaFecha stringHoras
        registrosEmpleados = map mostrarResultado resultado
        tomaHorasAcumuladas = fromIntegral (sum horasAcumuladas) / 60
        cantidadEmpleados = filter buscarEmpleados lineas
        totalEmpleados = read $ filter (`elem` "0123456789") (unwords cantidadEmpleados) :: Int
        horasHombre =  tomaHorasAcumuladas / fromIntegral cantidadEmpleados
        muestraAvgHorasHombre = horasHombre / 30
        imprimeAvgHorasHombre = ["\nPromedio diario: " ++ show muestraAvgHorasHombre ++ " Horas"]
        
        -- concatenamos los strings para mostrar el resultado final
        muestra =  imprimeAvgHorasHombre

    putStrLn $ unlines muestra
    hClose archivo

filtrarRenglones :: String -> Bool
filtrarRenglones str | str == "\r" = False
                     | otherwise = (!! 0) (words str) `elem` ["\"Lunes\"", "\"Martes\"", "\"Miercoles\"", "\"Jueves\"", "\"Viernes\""]

buscarEmpleados :: String -> Bool
buscarEmpleados str | str == "\r" = False
                        | otherwise = (!! 0) (words str) == "\"Total"
       
crealista :: [String] -> [(String, String)]
crealista [] = []
crealista str = ((!!0) str, (!!1) str) : crealista (tail (tail str))

tomarHoras :: String -> Int
tomarHoras "" = -1
tomarHoras = read (takeWhile (/= ':') n) :: Int


tomarMinuto :: String -> Int
tomarMinuto "" = -1
tomarMinuto n = read (drop 3 n) :: Int


creaHoras :: (String,String) -> (Hora,Hora)
creaHoras (x,y) = (Hora (tomarHora x) (tomarMinuto x), Hora (tomarHora y) (tomarMinuto y))

sumarHoras :: (Hora,Hora) -> Int
sumarHoras (x,y) 
            | hora x == -1 || hora y == -1 = 0
            | otherwise = getDif  x y

convertirHoras :: [(String,String)] -> [(Hora,Hora)]
convertirHoras = map creaHoras


unirStrings :: [(String,String)] -> String
unirStrings [] = ""
unirStrings ((_, _):_:_) = ""
unirStrings [(x,y)] | x == "Miercoles" = x ++ "\t" ++ y
                    | otherwise = x ++ "\t\t" ++ y

mostrarResultado :: (String,String) -> String
mostrarResultado (x,y) = x ++ "\t" ++ y
