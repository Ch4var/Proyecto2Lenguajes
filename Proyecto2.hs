import System.IO
import Data.List
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

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
trimLeadingZero (c:cs) = if c == '0' then cs else c:cs

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
splitOn c s = first : splitOn c (drop 1 rest) where (first, rest) = span (/=c) s

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
    let habitaciones = [ (i, nombreTipo) | i <- [1..cantidad] ]
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
        --"5" -> consultar_reservaciones >> opciones_administrativas tiposHabitaciones habitaciones tarifas
        --"6" -> estadisticas_ocupacion >> opciones_administrativas tiposHabitaciones habitaciones tarifas
        "7" -> return ()
        _   -> do
            putStrLn "Opcion invalida"
            opciones_administrativas tiposHabitaciones habitaciones tarifas

main :: IO ()
main = do
    tiposHabitaciones <- cargarTiposHabitaciones "tipos_habitaciones.txt"
    let habitaciones = []
    let tarifas = []
    opciones_administrativas tiposHabitaciones habitaciones tarifas
