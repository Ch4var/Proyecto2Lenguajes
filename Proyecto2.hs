import System.IO
import Data.List
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

fechasEntre :: String -> String -> [String]
fechasEntre ingreso salida = map (formatTime defaultTimeLocale "%d-%m-%Y") [inicio..fin]
  where
    inicio = readTime defaultTimeLocale "%d-%m-%Y" ingreso :: Day
    fin = readTime defaultTimeLocale "%d-%m-%Y" salida :: Day

dateToString :: String -> String
dateToString dateString =
    let format = "%d-%m-%Y"
        time = parseTimeOrError True defaultTimeLocale format dateString :: LocalTime
        dayOfWeek = formatTime defaultTimeLocale "%A" time
        dayOfMonth = formatTime defaultTimeLocale "%e" time
        monthName = formatTime defaultTimeLocale "%B" time
        year = formatTime defaultTimeLocale "%Y" time
    in dayOfWeek ++ trimLeadingZero dayOfMonth ++ " " ++ monthName ++ " " ++ year

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
    habitaciones <- asignar_cantidad_habitaciones' tiposHabitaciones 0
    putStrLn "Listado de habitaciones por tipo:"
    mapM_ mostrarHabitacion habitaciones
    return habitaciones

asignar_cantidad_habitaciones' :: [TipoHabitacion] -> Int -> IO [Habitacion]
asignar_cantidad_habitaciones' [] _ = return []
asignar_cantidad_habitaciones' (tipoHabitacion:tiposHabitaciones) n = do
    habitacionesTipo <- asignar_cantidad_habitaciones_tipo tipoHabitacion n
    let n' = n + length habitacionesTipo
    habitacionesResto <- asignar_cantidad_habitaciones' tiposHabitaciones n'
    return (habitacionesTipo ++ habitacionesResto)

asignar_cantidad_habitaciones_tipo :: TipoHabitacion -> Int -> IO [Habitacion]
asignar_cantidad_habitaciones_tipo (nombreTipo, _, _) n = do
    putStr $ "Ingrese la cantidad de habitaciones para el tipo " ++ nombreTipo ++ ": "
    cantidadStr <- getLine
    let cantidad = read cantidadStr
    let habitaciones = [ (n + i, nombreTipo) | i <- [1..cantidad] ]
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

totalReservacion :: Int -> Int -> String -> String -> [Tarifa] -> Int
totalReservacion cantAdultos cantNinos fechaIngreso fechaSalida tarifas = sum $ map (tarifaPorDia tarifas cantAdultos cantNinos) (fechasEntre fechaIngreso fechaSalida)

tarifaPorDia :: [Tarifa] -> Int -> Int -> String -> Int
tarifaPorDia tarifas cantAdultos cantNinos fecha = tarifaAdultos + tarifaNinos
  where
    diaSemana = head $ words $ dateToString fecha
    mes = read (fecha !! 3 : fecha !! 4 : []) :: Int
    tarifaAdultos
      | diaSemana `elem` ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"] && mes >= 4 && mes <= 10 = cantAdultos * (snd (tarifas !! 0))
      | diaSemana `elem` ["Friday", "Saturday"] && mes >= 4 && mes <= 10 = cantAdultos * (snd (tarifas !! 2))
      | diaSemana `elem` ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"] && (mes < 4 || mes > 10) = cantAdultos * (snd (tarifas !! 4))
      | otherwise = cantAdultos * (snd (tarifas !! 6))
    tarifaNinos
      | diaSemana `elem` ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"] && mes >= 4 && mes <= 10 = cantNinos * (snd (tarifas !! 1))
      | diaSemana `elem` ["Friday", "Saturday"] && mes >= 4 && mes <= 10 = cantNinos * (snd (tarifas !! 3))
      | diaSemana `elem` ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"] && (mes < 4 || mes > 10) = cantNinos * (snd (tarifas !! 5))
      | otherwise = cantNinos * (snd (tarifas !! 7))

type Reservacion = (Int, String, String, String, String, Int, Int, Int, Int, String)

hacerReservacion :: [TipoHabitacion] -> [Habitacion] -> [Reservacion] -> [Tarifa] -> IO [Reservacion]
hacerReservacion tiposHabitaciones habitaciones reservaciones tarifas = do
    putStrLn "Tipos de habitaciones disponibles:"
    mapM_ mostrarTipoHabitacion tiposHabitaciones
    putStr "Elija un tipo de habitacion: "
    tipoHabitacion <- getLine
    let maximoHuespedes = getMaximoHuespedes tiposHabitaciones tipoHabitacion
    putStr "Ingrese la fecha de ingreso (dd-mm-yyyy): "
    fechaIngreso <- getLine
    putStr "Ingrese la fecha de salida (dd-mm-yyyy): "
    fechaSalida <- getLine
    
    putStr "Ingrese la cantidad de adultos: "
    cantAdultosStr <- getLine
    let cantAdultos = read cantAdultosStr
    putStr "Ingrese la cantidad de niños: "
    cantNinosStr <- getLine
    let cantNinos = read cantNinosStr
    if cantAdultos + cantNinos > maximoHuespedes || cantAdultos + cantNinos < 1
        then do
            putStrLn "La cantidad de huespedes excede el limite del tipo de habitacion o es menor a 1."
            return reservaciones
        else do
            let habitacionesDisponibles = filter (habitacionDisponible reservaciones fechaIngreso fechaSalida) (filter ((== tipoHabitacion) . snd) habitaciones)
            if null habitacionesDisponibles
                then do
                    putStrLn "No hay disponibilidad para el tipo de habitacion y fechas seleccionadas."
                    return reservaciones
                else do
                    let idHabitacion = fst (head habitacionesDisponibles)
                    putStr "Ingrese el nombre de quien reserva: "
                    nombre <- getLine
                    fechaHoraActual <- getCurrentTime
                    let fechaHoraReservacion = formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" fechaHoraActual
                    let total = totalReservacion cantAdultos cantNinos fechaIngreso fechaSalida tarifas
                    let idReservacion = length reservaciones + 1
                    putStrLn "Comprobante:"
                    putStrLn $ "Identificador: " ++ show idReservacion
                    putStrLn $ "Nombre: " ++ nombre
                    putStrLn $ "Fecha y hora: " ++ fechaHoraReservacion
                    putStrLn $ "Fecha ingreso: " ++ fechaIngreso
                    putStrLn $ "Fecha salida: " ++ fechaSalida
                    putStrLn $ "Cantidad adultos: " ++ show cantAdultos
                    putStrLn $ "Cantidad niños: " ++ show cantNinos
                    putStrLn $ "Total: $" ++ show total
                    putStrLn $ "ID Habitacion: " ++ show idHabitacion
                    putStrLn $ "Tipo Habitacion: " ++ tipoHabitacion

                    let nuevaReservacion = (idReservacion, nombre, fechaHoraReservacion, fechaIngreso, fechaSalida, cantAdultos, cantNinos, total, idHabitacion, tipoHabitacion)
                    return (reservaciones ++ [nuevaReservacion])

getMaximoHuespedes :: [TipoHabitacion] -> String -> Int
getMaximoHuespedes tiposHabitaciones tipoHabitacion = maximoHuespedes
  where (_, _, maximoHuespedes) = head (filter ((== tipoHabitacion) . fst3) tiposHabitaciones)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

habitacionDisponible :: [Reservacion] -> String -> String -> Habitacion -> Bool
habitacionDisponible reservaciones fechaIngreso fechaSalida (idHabitacion, _) = all (\(idReservacion, _, _, fechaIngresoReservacion, fechaSalidaReservacion, _, _, _, idHabitacionReservacion, _) -> idHabitacion /= idHabitacionReservacion || not (fechasSolapadas fechaIngreso fechaSalida fechaIngresoReservacion fechaSalidaReservacion)) reservaciones

fechasSolapadas :: String -> String -> String -> String -> Bool
fechasSolapadas fechaIngreso1 fechaSalida1 fechaIngreso2 fechaSalida2 = not (fechaSalida1' < fechaIngreso2' || fechaIngreso1' > fechaSalida2')
  where
    fechaIngreso1' = parseDate fechaIngreso1
    fechaSalida1' = parseDate fechaSalida1
    fechaIngreso2' = parseDate fechaIngreso2
    fechaSalida2' = parseDate fechaSalida2

parseDate :: String -> UTCTime
parseDate dateStr = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" dateStr

mostrarReservaciones :: [Reservacion] -> IO ()
mostrarReservaciones reservaciones = do
    putStrLn "Listado de reservaciones:"
    mapM_ mostrarReservacion reservaciones

mostrarReservacion :: Reservacion -> IO ()
mostrarReservacion (idReservacion, nombre, fechaHoraReservacion, fechaIngreso, fechaSalida, cantAdultos, cantNinos, total, idHabitacion, tipoHabitacion) = do
    putStrLn $ "Identificador: " ++ show idReservacion
    putStrLn $ "Nombre: " ++ nombre
    putStrLn $ "Fecha y hora: " ++ fechaHoraReservacion
    putStrLn $ "Fecha ingreso: " ++ fechaIngreso
    putStrLn $ "Fecha salida: " ++ fechaSalida
    putStrLn $ "Cantidad adultos: " ++ show cantAdultos
    putStrLn $ "Cantidad niños: " ++ show cantNinos
    putStrLn $ "Total: $" ++ show total -- Asumiendo que las tarifas estan en dolares.
    putStrLn $ "ID Habitacion: " ++ show idHabitacion
    putStrLn $ "Tipo Habitacion: " ++ tipoHabitacion

facturarReservacion :: [Reservacion] -> [Reservacion] -> IO [Reservacion]
facturarReservacion reservaciones reservacionesFacturadas = do
    mostrarReservaciones reservaciones
    putStrLn "Ingrese el índice de la reservación que desea facturar:"
    indiceStr <- getLine
    let indice = read indiceStr :: Int
    if indice < 1 || indice > length reservaciones
        then do
            putStrLn "Índice fuera de rango. Intente de nuevo."
            facturarReservacion reservaciones reservacionesFacturadas
        else do
            let reservacion = reservaciones !! (indice - 1)
            if elem reservacion reservacionesFacturadas
                then do
                    putStrLn "La reservación ya está facturada."
                    return reservacionesFacturadas
                else do
                    putStrLn "La reservación ha sido facturada."
                    return (reservacion : reservacionesFacturadas)

estadisticas :: [Reservacion] -> [Habitacion] -> IO ()
estadisticas reservacionesFacturadas habitaciones = do
    let totalHuespedes = sum $ map (\(_, _, _, _, _, cantAdultos, cantNinos, _, _, _) -> cantAdultos + cantNinos) reservacionesFacturadas
    let habitacionesOcupadas = nub $ map (\(_, _, _, _, _, _, _, _, idHabitacion, _) -> idHabitacion) reservacionesFacturadas
    let totalHabitacionesNoOcupadas = length habitaciones - length habitacionesOcupadas
    let montoRecaudado = sum $ map (\(_, _, _, _, _, _, _, total, _, _) -> total) reservacionesFacturadas
    let montoRecaudadoConImpuestos = fromIntegral montoRecaudado * 1.13

    putStrLn $ "Total de huespedes: " ++ show totalHuespedes
    putStrLn $ "Listado de habitaciones ocupadas: " ++ show habitacionesOcupadas
    putStrLn $ "Total de habitaciones no ocupadas: " ++ show totalHabitacionesNoOcupadas
    putStrLn $ "Monto recaudado con impuestos: $" ++ show montoRecaudadoConImpuestos

menu_principal :: [TipoHabitacion] -> [Habitacion] -> [Tarifa] -> [Reservacion] -> [Reservacion] -> IO ()
menu_principal tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas = do
    putStrLn "Bienvenido al menú principal"
    putStrLn "1. Opciones Administrativas"
    putStrLn "2. Opciones Generales"
    putStrLn "3. Salir"
    putStr "Elija una opcion: \n"
    choice <- getLine
    case choice of
        "1" -> opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "2" -> opciones_generales tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "3" -> return ()
        _   -> do
          putStrLn "Opcion invalida"
          menu_principal tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas

opciones_generales :: [TipoHabitacion] -> [Habitacion] -> [Tarifa] -> [Reservacion] -> [Reservacion] ->  IO ()
opciones_generales tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas = do
    putStrLn "Bienvenido al menú de Opciones Generales"
    putStrLn "1. Reservacion"
    putStrLn "2. Facturar Reservacion"
    putStrLn "3. Salir"
    putStr "Elija una opcion: \n"
    opcion <- getLine
    case opcion of
        "1" -> do
            nuevasReservaciones <- hacerReservacion tiposHabitaciones habitaciones reservaciones tarifas
            opciones_generales tiposHabitaciones habitaciones tarifas nuevasReservaciones reservacionesFacturadas
        "2" -> do
            nuevasReservacionesFacturadas <- facturarReservacion reservaciones reservacionesFacturadas
            opciones_generales tiposHabitaciones habitaciones tarifas reservaciones nuevasReservacionesFacturadas
        "3" -> do
            menu_principal tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        _   -> do
            putStrLn "Opcion invalida"
            opciones_generales tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas

opciones_administrativas :: [TipoHabitacion] -> [Habitacion] -> [Tarifa] -> [Reservacion] -> [Reservacion] -> IO ()
opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas = do
    putStrLn "Bienvenido al menú de Opciones Administrativas"
    putStrLn "1. Informacion de hotel"
    putStrLn "2. Mostrar tipos de habitaciones"
    putStrLn "3. Asignar Cantidad de habitaciones por tipo"
    putStrLn "4. Carga de Tarifas"
    putStrLn "5. Consultar Reservaciones"
    putStrLn "6. Estadisticas de ocupacion"
    putStrLn "7. Salir"
    putStr "Elija una opcion: \n"
    opcion <- getLine
    case opcion of
        "1" -> informacion_hotel >> opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "2" -> do
            mostrarTiposHabitaciones tiposHabitaciones
            opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "3" -> do
            if null habitaciones
                then do
                    nuevasHabitaciones <- asignar_cantidad_habitaciones tiposHabitaciones
                    opciones_administrativas tiposHabitaciones nuevasHabitaciones tarifas reservaciones reservacionesFacturadas
                else do
                    putStrLn "Las habitaciones ya han sido generadas."
                    opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "4" -> do
            nuevasTarifas <- carga_tarifas "tarifas.txt"
            opciones_administrativas tiposHabitaciones habitaciones nuevasTarifas reservaciones reservacionesFacturadas
        "5" -> mostrarReservaciones reservaciones >> opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "6" -> estadisticas reservacionesFacturadas habitaciones >> opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        "7" -> do
            menu_principal tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
        _   -> do
            putStrLn "Opcion invalida"
            opciones_administrativas tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas

main :: IO ()
main = do
    tiposHabitaciones <- cargarTiposHabitaciones "tipos_habitaciones.txt"
    let habitaciones = []
    let tarifas = []
    let reservaciones = []
    let reservacionesFacturadas = []
    menu_principal tiposHabitaciones habitaciones tarifas reservaciones reservacionesFacturadas
