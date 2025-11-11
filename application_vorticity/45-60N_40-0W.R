########## Alle Koordinaten mit in den Datensatz aufnehmen ##################

# Die Daten liegen in Form von .nc Dateien vor
# Paket ncdf4 um mit diesen Dateien zu arbeiten

library("ncdf4")


library("tidyverse")

# Christinas FCPP Paket (gibt es nur bei Github)

#remotes::install_github("CMeschede/FCPP")

### working directory

setwd("C:/Users/mendel/Documents/application_vorticity_data/data")

# Einlesen der .nc Dateien nach Christina
# Die dann eingelesenen Dateien sind dann ein ncdf4 Objekt, dass dann
# noch weiter verarbeitet werden muss
# 2020-2022 sind in einer .nc Datei und 2023 ist alleine 
# in einer .nc Datei

start_year <- c(seq(1940, 2020, 10), 2023)
end_year <-  c(seq(1949, 2019, 10), 2022, 2023)

### Einlesen der Dateien

name <- vector(mode = "character", length = length(start_year))
time.name <- vector(mode = "character", length = length(start_year))

for(i in seq_along(start_year)) {
  # Objektname fuer die Zuweisung der i. .nc Datei
  name[i] <- paste("nc", i, sep = "")
  # Name der i. .nc Datei
  data <- paste("vorticity_850hPa_", start_year[i], "_", end_year[i], 
                ".nc", sep = "")
  # Zuweisung
  assign(x = name[i], value = ncdf4::nc_open(data))
  # Kontrolle
  print(paste("Kontrolle: Jahre ", start_year[i], " bis ", end_year[i], sep = ""))
  
  # Longitudes
  lon_all <- ncdf4::ncvar_get(get(name[i]), "longitude")
  print(lon_all)
  nlon <- dim(lon_all)
  
  # Latitude
  lat_all <- ncdf4::ncvar_get(get(name[i]), "latitude")
  print(lat_all)
  nlat <- dim(lat_all)
  
  # Namen fuer die Zuweisung der Zeitpunkte (UNIX time)
  time.name[i] <- paste("time", i, sep = "")
  # Zuweisung
  assign(x = time.name[i], value = ncdf4::ncvar_get(get(name[i]), "time"))
  # Kontrolle
  print(lubridate::as_datetime(range(get(time.name[i]))*3600, origin = "1900-01-01"))
}

### Daten bis zum 31.3.2023 vorhanden

### alle .nc Dateien wurden eingelesen

### Zeitpunkte erstellen:

# Vektor mit allen Zeitpunkten in UNIX time "Sekunden seit 1.1.1900 0 Uhr"
time_UNIX <- seq(time1[1], time10[length(time10)], 1)
head(time_UNIX); tail(time_UNIX)
# Umformung in Datum + Uhrzeit  
time_UTC <- lubridate::as_datetime(time_UNIX*3600, origin = "1900-01-01"); 
head(time_UTC)
tail(time_UTC)
# Anzahl Zeitpunkte
nt <- length(time_UNIX); nt

### Orte erstellen

# Ortgitter: (Ort und Gitter einschraenken)

# data.frame, in dem spaeter die Daten gespeichert werden sollen 
out <- tidyr::crossing( # Atlantik + Europa mit [1, 1] Gitter
  lon = seq(-40, 0, 1), 
  lat = seq(45, 60, 1)
)

# Anzahl Gitterpunkte
nout <- nrow(out); nout


#### Alle 6 Stunden ####
out_6h <- out |> 
  dplyr::mutate(
    JJ = list(NULL), # Ueberschreitungshoehen
    WW = list(NULL), # Wartezeiten zwischen zwei Ueberschreitungen 
    TT = list(NULL), # Eintrittszeiten der JJ
    u = vector(mode = "numeric", length = nout), # kleinste Ueberschreitungshoehe / Threshold
    noepy = list(NULL) # dataframe mit Anzahl an Ueberschreitungen pro Jahr
  )


ind_time_6h <- which(lubridate::hour(time_UTC) %in% c(0, 6, 12, 18)) 
# ab Jan 1940 bis Feb 2023, 6 hourly
length(ind_time_6h)

time_UNIX_6h <- time_UNIX[ind_time_6h]
W_6h <- c(time_UNIX_6h[1], diff(time_UNIX_6h))
table(W_6h)
### interW brauch ich nicht, weil ich ja nicht nur den Winter 
### betrachte
#interW_6h <- 6606

ntime_6h <- length(ind_time_6h); ntime_6h # Anzahl der Zeitpunkte 
q001 <- 0.01 # 1 % der Beobachtungen als Ueberschreitungen waehlen
#q0001 <- 0.001 # 0.1 % der Beobachtungen waehlen
#q0005 <- 0.005 # 0.5 % der Beobachtungen waehlen
k_6h <- round(ntime_6h*q001)  # + length(unique(lubridate::year(time_UTC))) - 2; k_6h


for(i in 1:nout) {
  print(i)
  ind_lon <- which(lon_all == out$lon[i])
  ind_lat <- which(lat_all == out$lat[i])
  ### Werte für die Vorticity holen
  vo <- list()
  for(j in seq_along(start_year)) { 
    vo[[j]] <- ncdf4::ncvar_get(get(name[j]), "vo", start = c(ind_lon, ind_lat, 1), count = c(1, 1, length(get(time.name[j]))))
  }
  
  ### nun zu den Zeiten, die wir sehen wollen
  vo <- unlist(vo)
  vo_6h <- vo[ind_time_6h]
  
  thinned_6h <- FCPP::thin(tibble::tibble(JJ = vo_6h, WW = W_6h), k = k_6h)
  JJ_6h <- FCPP::magnitudes(thinned_6h) # excess value
  WW_6h <- FCPP::interarrivaltime(thinned_6h) # time between two exceedances
  TT_6h <- FCPP::arrivaltime(thinned_6h) # excess time
  WW_6h <- WW_6h / 6 # time between two exceedances within one winter and 1 = 6h
  month_6h <- lubridate::year(lubridate::as_datetime(TT_6h*3600, origin = "1900-01-01"))
  year_6h <- lubridate::year(lubridate::as_datetime(TT_6h*3600, origin = "1900-01-01"))
  year_6h[month_6h == 12] <- year_6h[month_6h == 12] + 1
  x.help <- table(factor(year_6h, levels = 1940:2022))
  noepy_6h <- tibble::tibble(winter_year = as.integer(attributes(x.help)$dimnames[[1]]), no.exceedances = as.integer(x.help))
  # number of exceedances per year
  out_6h$noepy[i] <- list(noepy_6h)
  # excess values
  out_6h$JJ[i] <- list(JJ_6h)
  # time between two exceedances 
  out_6h$WW[i] <- list(WW_6h)
  # excess time in UNIX
  out_6h$TT[i] <- list(TT_6h)
  # threshold
  out_6h$u[i] <- min(thinned_6h$newJJ)
}
setwd("C:/Users/mendel/Documents/paper2/application_vorticity")
save(out_6h, file = "45-60N_40-0W.RData")
