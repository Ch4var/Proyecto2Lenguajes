import System.IO
import Data.List
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

{-El objetivo de esta función es tomar dos cadenas de texto que representan fechas en el formato "día-mes-año" 
y devuelve una lista de cadenas de texto que representan todas las fechas entre esas dos fechas, 
incluyendo las fechas de inicio y fin-}
fechasEntre :: String -> String -> [String]
fechasEntre ingreso salida = map (formatTime defaultTimeLocale "%d-%m-%Y") [inicio..fin]
  where
    inicio = readTime defaultTimeLocale "%d-%m-%Y" ingreso :: Day
    fin = readTime defaultTimeLocale "%d-%m-%Y" salida :: Day

{-El objetivo de esta función es tomar una cadena de texto que representa una fecha y hora en formato "día-mes-año hora:minutos"
 y devuelve una cadena de texto que representa la fecha y hora-}
dateToString :: String -> String
dateToString dateString =
    let format = "%d-%m-%Y"
        time = parseTimeOrError True defaultTimeLocale format dateString :: LocalTime
        dayOfWeek = formatTime defaultTimeLocale "%A" time
        dayOfMonth = formatTime defaultTimeLocale "%e" time
        monthName = formatTime defaultTimeLocale "%B" time
        year = formatTime defaultTimeLocale "%Y" time
    in dayOfWeek ++ trimLeadingZero dayOfMonth ++ " " ++ monthName ++ " " ++ year

{-El objetivo de esta función es quitar el 0 a la izquierda de las fechas-}
trimLeadingZero :: String -> String
trimLeadingZero (c:cs) = if c == '0' then cs else c:cs

{-El objetivo de esta función es leer un archivo de texto llamado "info_hotel.txt" y muestra información sobre el hotel en la consola. 
La función utiliza la función openFile para abrir el archivo en modo de lectura, la función hGetContents para leer el contenido del archivo en una cadena de texto-}
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

{-El objetivo de esta función es separar en las comas los string, para leer del archivo-}
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = first : splitOn c (drop 1 rest) where (first, rest) = span (/=c) s

{-Tupla para almacenar un Tipo de habitación-}
type TipoHabitacion = (String, String, Int)

{-El objetivo de esta función es recibir una ruta de archivo como parámetro, leer el contenido del archivo en dicha ruta y separarlo en líneas.
Luego, llama a parseTipoHabitacion con cada línea para obtener una lista de tuplas (nombre, descripcion, maximoHuespedes), 
donde cada tupla representa un tipo de habitación, luego devuelve esta lista de tuplas.-}
cargarTiposHabitaciones :: FilePath -> IO [TipoHabitacion]
cargarTiposHabitaciones rutaArchivo = do
    contenido <- readFile rutaArchivo
    let lineas = lines contenido
    let tiposHabitaciones = map parseTipoHabitacion lineas
    return tiposHabitaciones

{-El objetivo de esta función es recibir una línea de texto y devolver una tupla de tipo TipoHabitacion con
el nombre, descripción y máximo de huéspedes de una habitación.-}
parseTipoHabitacion :: String -> TipoHabitacion
parseTipoHabitacion linea = (nombre, descripcion, maximoHuespedes)
  where
    campos = splitOn ',' linea
    nombre = campos !! 0
    descripcion = campos !! 1
    maximoHuespedes = read (campos !! 2)

{- El objetivo de esta función es recibir una lista de TipoHabitacion y mostrar en consola los detalles de cada uno.-}
mostrarTiposHabitaciones :: [TipoHabitacion] -> IO ()
mostrarTiposHabitaciones tiposHabitaciones = do
    putStrLn "Tipos de habitaciones:"
    mapM_ mostrarTipoHabitacion tiposHabitaciones

{- El objetivo de esta función es recibir una tupla de TipoHabitacion y mostrar en consola sus detalles.-}
mostrarTipoHabitacion :: TipoHabitacion -> IO ()
mostrarTipoHabitacion (nombre, descripcion, maximoHuespedes) = do
    putStrLn $ "Nombre: " ++ nombre
    putStrLn $ "Descripcion: " ++ descripcion
    putStrLn $ "Maximo huespedes: " ++ show maximoHuespedes

{-Tupla para almacenar una habitación-}
type Habitacion = (Int, String)

{- El objetivo de esta función es tomar una lista de tipos de habitaciones y retornar una acción IO que, 
cuando se ejecuta, solicita la cantidad de habitaciones de cada tipo y retorna una lista de habitaciones. 
Primero llama a asignar_cantidad_habitaciones' con un acumulador inicial de 0 y luego muestra en pantalla el listado de habitaciones por tipo, 
llamando a la función mostrarHabitacion, antes de retornar la lista.-}
asignar_cantidad_habitaciones :: [TipoHabitacion] -> IO [Habitacion]
asignar_cantidad_habitaciones tiposHabitaciones = do
    habitaciones <- asignar_cantidad_habitaciones' tiposHabitaciones 0
    putStrLn "Listado de habitaciones por tipo:"
    mapM_ mostrarHabitacion habitaciones
    return habitaciones

{- El objetivo de esta función es tomar una lista de tipos de habitaciones y un número entero n que representa el identificador de la primera habitación 
que se asignará, y retorna una acción IO que, cuando se ejecuta, solicita la cantidad de habitaciones de cada tipo y retorna una lista de habitaciones. 
La función utiliza la función asignar_cantidad_habitaciones_tipo para obtener las habitaciones para el primer tipo de habitación en la lista, 
luego llama a sí misma con el resto de la lista y un nuevo valor n' que es igual al valor n original más la cantidad de habitaciones para el primer tipo de habitación. 
Finalmente, retorna la lista de habitaciones para el primer tipo de habitación concatenada con la lista de habitaciones para el resto de los tipos de habitación-}
asignar_cantidad_habitaciones' :: [TipoHabitacion] -> Int -> IO [Habitacion]
asignar_cantidad_habitaciones' [] _ = return []
asignar_cantidad_habitaciones' (tipoHabitacion:tiposHabitaciones) n = do
    habitacionesTipo <- asignar_cantidad_habitaciones_tipo tipoHabitacion n
    let n' = n + length habitacionesTipo
    habitacionesResto <- asignar_cantidad_habitaciones' tiposHabitaciones n'
    return (habitacionesTipo ++ habitacionesResto)

{- El objetivo de esta función es tomar un tipo de habitación y un número entero n que representa el identificador de la primera habitación 
que se asignará para ese tipo, y retorna una acción IO que, cuando se ejecuta, solicita la cantidad de habitaciones de ese tipo 
y retorna una lista de habitaciones. La función utiliza una comprensión de lista para crear una lista de habitaciones, 
donde cada habitación tiene un identificador igual al identificador inicial n más un número de índice en el rango [1..cantidad].-}
asignar_cantidad_habitaciones_tipo :: TipoHabitacion -> Int -> IO [Habitacion]
asignar_cantidad_habitaciones_tipo (nombreTipo, _, _) n = do
    putStr $ "Ingrese la cantidad de habitaciones para el tipo " ++ nombreTipo ++ ": "
    cantidadStr <- getLine
    let cantidad = read cantidadStr
    let habitaciones = [ (n + i, nombreTipo) | i <- [1..cantidad] ]
    return habitaciones

{- El objetivo de esta función es tomar una habitación y mostrar en pantalla su información, incluyendo su identificador y tipo de habitación.-}
mostrarHabitacion :: Habitacion -> IO ()
mostrarHabitacion (idHabitacion, tipoHabitacion) = do
    putStrLn $ "Habitacion " ++ show idHabitacion ++ " - Tipo: " ++ tipoHabitacion

{-Tupla para almacenar una tarifa-}
type Tarifa = (Int, Int)

{- El objetivo de esta función es recibir la ruta de un archivo como argumento y retornar una lista de tuplas de tipo Tarifa. 
La función lee el contenido del archivo en la ruta especificada, divide el contenido en líneas y para cada línea utiliza 
la función parseTarifa para convertirla en una tupla de tipo Tarifa.-}
carga_tarifas :: FilePath -> IO [Tarifa]
carga_tarifas rutaArchivo = do
    contenido <- readFile rutaArchivo
    let lineas = lines contenido
    let tarifas = map parseTarifa lineas
    return tarifas

{- El objetivo de esta función es recibir una cadena de texto y retornar una tupla de tipo Tarifa. 
La función divide la cadena en campos utilizando la coma como separador, convierte el primer campo en un valor de tipo TipoHabitacion 
y el segundo campo en un valor de tipo Int y retorna una tupla que contiene ambos valores.-}
parseTarifa :: String -> Tarifa
parseTarifa linea = (tipo, tarifa)
  where
    campos = splitOn ',' linea
    tipo = read (campos !! 0)
    tarifa = read (campos !! 1)

{- El objetivo de esta función es recibir la cantidad de adultos y niños, las fechas de ingreso y salida, y una lista de tarifas. 
La función utiliza la función fechasEntre para obtener la lista de fechas entre las fechas de ingreso y salida, 
luego utiliza la función tarifaPorDia para obtener la tarifa correspondiente a cada fecha. Finalmente, la función suma todas las tarifas y retorna el total.-}
totalReservacion :: Int -> Int -> String -> String -> [Tarifa] -> Int
totalReservacion cantAdultos cantNinos fechaIngreso fechaSalida tarifas = sum $ map (tarifaPorDia tarifas cantAdultos cantNinos) (fechasEntre fechaIngreso fechaSalida)

{- El objetivo de esta función es recibir una lista de tarifas, la cantidad de adultos y niños, y una fecha. 
La función utiliza la función dateToString para obtener el día de la semana correspondiente a la fecha, 
luego utiliza la información de la tarifa correspondiente al día de la semana y mes para calcular la tarifa para adultos y niños por separado. 
La función retorna la suma de ambas tarifas.-}
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

{-Tupla para almacenar una Reservacion-}
type Reservacion = (Int, String, String, String, String, Int, Int, Int, Int, String)

{- El objetivo de esta función es pedir al usuario la informacion de la reservacion. 
Luego verifica que exista disponibilidad en las habitaciones e imprime un comprobante. 
Recibe los arreglos de los tipos de habitaciones, de las habitaciones, de las reservaciones y de las tarifas 
y devuelve el arreglo de reservaciones con la nueva reservación-}
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

{- El objetivo de esta función es recibir una lista de tipos de habitación y el nombre de un tipo de habitación. 
Devuelve el número máximo de huéspedes que pueden alojarse en el tipo de habitación especificado. 
La función filtra la lista de tipos de habitación para obtener el tipo de habitación especificado 
y luego devuelve el tercer elemento de la tupla, que corresponde al número máximo de huéspedes.-}
getMaximoHuespedes :: [TipoHabitacion] -> String -> Int
getMaximoHuespedes tiposHabitaciones tipoHabitacion = maximoHuespedes
  where (_, _, maximoHuespedes) = head (filter ((== tipoHabitacion) . fst3) tiposHabitaciones)

{- El objetivo de esta función es tomar una tupla de 3 elementos y devolver el primer elemento de la tupla-}
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

{- El objetivo de esta función es recibir una lista de reservaciones, una fecha de ingreso, una fecha de salida y una habitación. 
Devuelve un valor booleano que indica si la habitación está disponible para las fechas especificadas. 
La función filtra la lista de reservaciones para obtener todas las reservaciones que se solapan con las fechas especificadas 
y luego devuelve False si la habitación en cuestión está incluida en alguna de esas reservaciones.-}
habitacionDisponible :: [Reservacion] -> String -> String -> Habitacion -> Bool
habitacionDisponible reservaciones fechaIngreso fechaSalida (idHabitacion, _) = all (\(idReservacion, _, _, fechaIngresoReservacion, fechaSalidaReservacion, _, _, _, idHabitacionReservacion, _) -> idHabitacion /= idHabitacionReservacion || not (fechasSolapadas fechaIngreso fechaSalida fechaIngresoReservacion fechaSalidaReservacion)) reservaciones

{- El objetivo de esta función es recibir dos pares de fechas (fecha de ingreso y fecha de salida) 
y devolver un valor booleano que indica si los dos pares de fechas se solapan. 
Devuelve True si las fechas se solapan y False si no se solapan.-}
fechasSolapadas :: String -> String -> String -> String -> Bool
fechasSolapadas fechaIngreso1 fechaSalida1 fechaIngreso2 fechaSalida2 = not (fechaSalida1' < fechaIngreso2' || fechaIngreso1' > fechaSalida2')
  where
    fechaIngreso1' = parseDate fechaIngreso1
    fechaSalida1' = parseDate fechaSalida1
    fechaIngreso2' = parseDate fechaIngreso2
    fechaSalida2' = parseDate fechaSalida2

{- El objetivo de esta función es recibir una cadena de fecha en el formato "dd-mm-yyyy" y devuelve un objeto UTCTime correspondiente a esa fecha. 
La función utiliza la función parseTimeOrError para analizar la cadena de fecha y devuelve el resultado.-}
parseDate :: String -> UTCTime
parseDate dateStr = parseTimeOrError True defaultTimeLocale "%d-%m-%Y" dateStr

{- El objetivo de esta función es recibir una lista de reservaciones y mostrar en consola un encabezado, 
luego utiliza la función mapM_ para aplicar la función mostrarReservacion a cada una de las reservaciones de la lista.-}
mostrarReservaciones :: [Reservacion] -> IO ()
mostrarReservaciones reservaciones = do
    putStrLn "Listado de reservaciones:"
    mapM_ mostrarReservacion reservaciones

{- El objetivo de esta función es recibir una reservación y mostrar en consola cada uno de los atributos: 
identificador, nombre, fecha y hora de la reservación, fecha de ingreso, fecha de salida, cantidad de adultos, cantidad de niños, total y tipo de habitación.-}
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

{- El objetivo de esta función es recibir dos listas de reservaciones, la primera lista es la lista completa de reservaciones 
y la segunda lista es la lista de reservaciones facturadas hasta el momento. La función muestra en consola la lista completa de reservaciones 
y pide al usuario que ingrese el índice de la reservación que desea facturar. Se verifica si la reservación ya está en la lista de reservaciones facturadas, si es así, 
se muestra un mensaje indicando que la reservación ya está facturada y se retorna la lista de reservaciones facturadas sin cambios. 
Si la reservación no está en la lista de reservaciones facturadas, 
se muestra un mensaje indicando que la reservación ha sido facturada y se retorna la lista de reservaciones facturadas con la nueva reservación añadida.-}
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

{- El objetivo de esta función es recibir una lista de reservaciones facturadas y una lista de habitaciones. 
La función obtiene el total de huéspedes, la lista de habitaciones ocupadas, el total de habitaciones no ocupadas y el monto recaudado. 
Luego calcula el monto recaudado con impuestos (asumiendo que los impuestos son del 13%) y muestra en consola cada uno de los resultados obtenidos.-}
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

{- El objetivo de esta función es crear el menu principal del programa-}
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

{- El objetivo de esta función es crear el menu de opciones generales del programa-}
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

{- El objetivo de esta función es crear el menu de opciones administrativas del programa-}
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