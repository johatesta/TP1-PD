module Horas 
( Hora(..),
  getHora,
  getMinutes,
  desnormalizar,
  tomarHora,
  tomarMinuto,
  normalizar
) where

data Hora = Hora { hora :: Int , minuto :: Int }



instance Show Hora where
  show (Hora hora minuto) = (if hora > 10
                             then (show hora)
                             else ("0" ++ show hora))
                            ++ ":" ++
                            (if minuto > 10
                             then (show minuto)
                             else ("0" ++ show minuto))

getHora :: Hora -> Int
getHora (Hora h _) = h

getMinutes :: Hora -> Int
getMinutes (Hora _ m) = m

desnormalizar :: Hora -> Int
desnormalizar h = 60 * getHora h + getMinutes h

tomarHora :: Int -> Int 
tomarHora h = h `div` 60

tomarMinuto :: Int -> Int 
tomarMinuto h = h `mod` 60

normalizar :: Int -> Hora
normalizar x = Hora (tomarHora x) (tomarMinuto x)

diferenciaHoraria :: Hora -> Hora -> Int
diferenciaHoraria h1 h2 = abs (desnormalizar h1 - desnormalizar h2)

