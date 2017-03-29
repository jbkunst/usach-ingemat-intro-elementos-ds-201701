library(nycflights13)
library(dplyr)
library(readr)

glimpse(airlines)
names(airlines) <- c("carrier", "nombre")
airlines

glimpse(airports)
names(airports) <- c("faa", "nombre", "lat", "lon", "alt", "tz", "dst", "tzone")
glimpse(airports)

glimpse(flights)
names(flights) <- c("anio", "mes", "dia",
                    "salida_hora", "salida_hora_programada", "salida_hora_retraso",
                    "llegada_hora", "llegada_hora_programada", "llegada_hora_retrado",
                    "carrier", "vuelo", "avion_num", "origen", "destino", "tiempo_vuelo",
                    "distancia", "hora", "minuto", "tiempo_hora")
glimpse(flights)

glimpse(planes)
names(planes) <- c("avion_num", "anio", "tipo", "fabricante", "modelo", "motores",
                   "asientos", "velocidad", "motor")
glimpse(planes)

glimpse(weather)
names(weather) <- c("origen", "anio", "mes", "dia", "hora", "temperatura",
                    "rocio", "humedad", "viento_direccion", "viento_velocidad",
                    "viento_rafaga", "precipitacion", "presion", "visibilidad",
                    "tiempo_hora")
glimpse(weather)


path <- "../clase-02-dplyr/data/"
write_tsv(airlines, file.path(path, "aerolineas.txt"))
write_tsv(airports, file.path(path, "aeropuertos.txt"))
write_tsv(flights, file.path(path, "vuelos.txt"))
write_tsv(planes, file.path(path, "aviones.txt"))
write_tsv(weather, file.path(path, "clima.txt"))

