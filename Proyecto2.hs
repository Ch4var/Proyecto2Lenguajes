import Control.Monad (forM_, replicateM)
import Data.Char (toLower)
import Data.IORef
import Data.List
import Data.List (find)
import Data.Maybe (isJust)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import System.IO
import System.IO.Unsafe (unsafePerformIO)

dateToString :: String -> String
dateToString dateString =
  let format = "%d-%m-%Y"
      time = parseTimeOrError True defaultTimeLocale format dateString :: LocalTime
      dayOfWeek = formatTime defaultTimeLocale "%A" time
      dayOfMonth = formatTime defaultTimeLocale "%e" time
      monthName = formatTime defaultTimeLocale "%B" time
      year = formatTime defaultTimeLocale "%Y" time
   in dayOfWeek ++ " " ++ trimLeadingZero dayOfMonth ++ " " ++ monthName ++ " " ++ year

trimLeadingZero :: String -> String
trimLeadingZero (c : cs) = if c == '0' then cs else c : cs

informacion_hotel :: IO ()
informacion_hotel = do
  handle <- openFile "info_hotel.txt" ReadMode
  contents <- hGetContents handle
  let hotelInfo = splitOn ',' (head (lines contents))
  let nombre = hotelInfo !! 0
  let cedula = hotelInfo !! 1
  let sitioWeb = hotelInfo !! 2
  let telefono = hotelInfo !! 3
  let pais = hotelInfo !! 4
  let provincia = hotelInfo !! 5
  putStrLn $ "Informacion del Hotel"
  putStrLn $ "Nombre: " ++ nombre
  putStrLn $ "Cedula Juridica: " ++ cedula
  putStrLn $ "Sitio Web: " ++ sitioWeb
  putStrLn $ "Telefono: " ++ telefono
  putStrLn $ "Pais: " ++ pais
  putStrLn $ "Provincia: " ++ provincia
  hClose handle

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = first : splitOn c (drop 1 rest) where (first, rest) = span (/= c) s

type TipoHabitacion = (String, String, Int)

cargarTiposHabitaciones :: FilePath -> IO [TipoHabitacion]
cargarTiposHabitaciones rutaArchivo = do
  contenido <- readFile rutaArchivo
  let lineas = lines contenido
  let tiposHabitaciones = map parseTipoHabitacion lineas
  return tiposHabitaciones

parseTipoHabitacion :: String -> TipoHabitacion
parseTipoHabitacion linea = (nombre, descripcion, maximoHuespedes)
  where
    campos = splitOn ',' linea
    nombre = campos !! 0
    descripcion = campos !! 1
    maximoHuespedes = read (campos !! 2)

mostrarTiposHabitaciones :: [TipoHabitacion] -> IO ()
mostrarTiposHabitaciones tiposHabitaciones = do
  putStrLn "Tipos de habitaciones:"
  mapM_ mostrarTipoHabitacion tiposHabitaciones

mostrarTipoHabitacion :: TipoHabitacion -> IO ()
mostrarTipoHabitacion (nombre, descripcion, maximoHuespedes) = do
  putStrLn $ "Nombre: " ++ nombre
  putStrLn $ "Descripcion: " ++ descripcion
  putStrLn $ "Maximo huespedes: " ++ show maximoHuespedes

type Habitacion = (Int, String)

asignar_cantidad_habitaciones :: [TipoHabitacion] -> IO [Habitacion]
asignar_cantidad_habitaciones tiposHabitaciones = do
  habitaciones <- mapM asignar_cantidad_habitaciones_tipo tiposHabitaciones
  let habitacionesAplanadas = concat habitaciones
  putStrLn "Listado de habitaciones por tipo:"
  mapM_ mostrarHabitacion habitacionesAplanadas
  return habitacionesAplanadas

asignar_cantidad_habitaciones_tipo :: TipoHabitacion -> IO [Habitacion]
asignar_cantidad_habitaciones_tipo (nombreTipo, _, _) = do
  putStr $ "Ingrese la cantidad de habitaciones para el tipo " ++ nombreTipo ++ ": "
  cantidadStr <- getLine
  let cantidad = read cantidadStr
  let habitaciones = [(i, nombreTipo) | i <- [1 .. cantidad]]
  return habitaciones

mostrarHabitacion :: Habitacion -> IO ()
mostrarHabitacion (idHabitacion, tipoHabitacion) = do
  putStrLn $ "Habitacion " ++ show idHabitacion ++ " - Tipo: " ++ tipoHabitacion

type Tarifa = (Int, Int)

carga_tarifas :: FilePath -> IO [Tarifa]
carga_tarifas rutaArchivo = do
  contenido <- readFile rutaArchivo
  let lineas = lines contenido
  let tarifas = map parseTarifa lineas
  return tarifas

parseTarifa :: String -> Tarifa
parseTarifa linea = (tipo, tarifa)
  where
    campos = splitOn ',' linea
    tipo = read (campos !! 0)
    tarifa = read (campos !! 1)

-- Modulo de reservaciones

data RoomType = Single | Double | Twin | Suite deriving (Eq, Show)

type Room = (RoomType, Int, Double)

data Reservation = Reservation
  { resId :: Int,
    resName :: String,
    resCheckin :: String,
    resCheckout :: String,
    resAdults :: Int,
    resChildren :: Int,
    resRooms :: [Room],
    resTotalCost :: Double,
    resPaid :: Bool
  }
  deriving (Eq, Show)

-- Variable global para almacenar todas las reservas
allReservations :: IORef [Reservation]
allReservations = unsafePerformIO (newIORef [])

-- Función para verificar si una fecha está después de otra
dateAfter :: String -> String -> Bool
dateAfter date1 date2 = date1 > date2

-- Función para actualizar la lista de reservas
updateReservations :: [Reservation] -> IO ()
updateReservations updatedReservations = do
  writeIORef allReservations updatedReservations
  putStrLn "La lista de reservas ha sido actualizada."

-- Tipo de datos para representar el resultado de la operación de reserva
data ReservationResult
  = ReservationSuccess Reservation
  | ReservationError String
  deriving (Show)

-- Función para hacer una reserva
makeReservation :: IO ReservationResult
makeReservation = do
  putStrLn "Ingrese su nombre:"
  name <- getLine
  putStrLn "Ingrese la fecha de check-in (formato dd/mm/aaaa):"
  checkin <- getLine
  putStrLn "Ingrese la fecha de check-out (formato dd/mm/aaaa):"
  checkout <- getLine
  putStrLn "Ingrese la cantidad de adultos:"
  adults <- readLn
  putStrLn "Ingrese la cantidad de niños:"
  children <- readLn
  rooms <- makeRoomsReservation
  case checkin `dateAfter` checkout of
    True -> return $ ReservationError "La fecha de check-in debe ser anterior a la fecha de check-out."
    False -> do
      reservations <- readIORef allReservations
      case checkAvailability reservations checkin checkout rooms of
        Left errorMessage -> return $ ReservationError errorMessage
        Right availableRooms ->
          let id = length reservations + 1
              totalCost = calculateTotalCost rooms
              reservation = Reservation id name checkin checkout adults children availableRooms totalCost False
              updatedReservations = reservation : reservations
           in updateReservations updatedReservations >> return (ReservationSuccess reservation)

instance Read RoomType where
  readsPrec _ input = case map toLower input of
    "single" -> [(Single, "")]
    "double" -> [(Double, "")]
    "twin" -> [(Twin, "")]
    "suite" -> [(Suite, "")]
    _ -> []

-- Función para ingresar los datos de las habitaciones a reservar
makeRoomsReservation :: IO [Room]
makeRoomsReservation = do
  putStrLn "¿Cuántas habitaciones desea reservar?"
  numRooms <- readLn
  replicateM numRooms makeRoomReservation

-- Función para ingresar los datos de una habitación a reservar
makeRoomReservation :: IO Room
makeRoomReservation = do
  putStrLn "Ingrese el tipo de habitación (Single/Double/Twin/Suite):"
  roomType <- readLn
  putStrLn "Ingrese el número de habitación:"
  roomNum <- readLn
  putStrLn "Ingrese el precio de la habitación:"
  roomPrice <- readLn
  return (roomType, roomNum, roomPrice)

-- Calcular el costo total de la reserva
calculateTotalCost :: [Room] -> Double
calculateTotalCost rooms = sum (map (\(_, _, price) -> price) rooms)

-- Función para verificar la disponibilidad de habitaciones en las fechas solicitadas
checkAvailability :: [Reservation] -> String -> String -> [Room] -> Either String [Room]
checkAvailability reservations checkin checkout rooms =
  let overlappingReservations = filter (\r -> overlaps r checkin checkout) reservations

      reservedRooms = concatMap resRooms overlappingReservations

      availableRooms = filter (\r -> r `notElem` reservedRooms) rooms
   in if length availableRooms < length rooms
        then Left "No hay suficientes habitaciones disponibles para las fechas solicitadas."
        else Right availableRooms

-- Función auxiliar para verificar si una reserva se superpone con las fechas solicitadas
overlaps :: Reservation -> String -> String -> Bool
overlaps reservation checkin checkout = resCheckout reservation > checkin && resCheckin reservation < checkout

-- Función para imprimir una reserva en el formato deseado
printReservation :: Reservation -> IO ()
printReservation res = do
  putStrLn $ "ID: " ++ show (resId res)
  putStrLn $ "Cliente: " ++ resName res
  putStrLn $ "Check-IN: " ++ resCheckin res
  putStrLn $ "Check-OUT: " ++ resCheckout res
  putStrLn $ "Numero de Adultos: " ++ show (resAdults res)
  putStrLn $ "Numero de Niños: " ++ show (resChildren res)
  putStrLn $ "Total de clientes: " ++ show (resAdults res + resChildren res)
  putStrLn $ "Costo total: $" ++ show (resTotalCost res)
  putStrLn $ "Pagado: " ++ show (resPaid res)

-- Función para imprimir todas las reservas
printAllReservations :: IO ()
printAllReservations = do
  reservations <- readIORef allReservations
  mapM_ printReservation reservations

opciones_administrativas :: [TipoHabitacion] -> [Habitacion] -> [Tarifa] -> IO ()
opciones_administrativas tiposHabitaciones habitaciones tarifas = do
  putStrLn "1. Informacion de hotel"
  putStrLn "2. Mostrar tipos de habitaciones"
  putStrLn "3. Asignar Cantidad de habitaciones por tipo"
  putStrLn "4. Carga de Tarifas"
  putStrLn "5. Consultar Reservaciones"
  putStrLn "6. Estadisticas de ocupacion"
  putStrLn "7. Salir"
  putStr "Elija una opcion: "
  opcion <- getLine
  case opcion of
    "1" -> informacion_hotel >> opciones_administrativas tiposHabitaciones habitaciones tarifas
    "2" -> do
      mostrarTiposHabitaciones tiposHabitaciones
      opciones_administrativas tiposHabitaciones habitaciones tarifas
    "3" -> do
      if null habitaciones
        then do
          nuevasHabitaciones <- asignar_cantidad_habitaciones tiposHabitaciones
          opciones_administrativas tiposHabitaciones nuevasHabitaciones tarifas
        else do
          putStrLn "Las habitaciones ya han sido generadas."
          opciones_administrativas tiposHabitaciones habitaciones tarifas
    "4" -> do
      nuevasTarifas <- carga_tarifas "tarifas.txt"
      opciones_administrativas tiposHabitaciones habitaciones nuevasTarifas
    "5" -> do
      printAllReservations
      opciones_administrativas tiposHabitaciones habitaciones reservaciones
    -- "6" -> estadisticas_ocupacion >> opciones_administrativas tiposHabitaciones habitaciones tarifas
    "7" -> return ()
    _ -> do
      putStrLn "Opcion invalida"
      opciones_administrativas tiposHabitaciones habitaciones tarifas

main :: IO ()
main = do
  tiposHabitaciones <- cargarTiposHabitaciones "tipos_habitaciones.txt"
  let habitaciones = []
  let tarifas = []
  opciones_administrativas tiposHabitaciones habitaciones tarifas
