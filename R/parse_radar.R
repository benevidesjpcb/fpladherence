# Leitura de dados de trajetoria RADAR (ou ADS-B) realmente voada
#
# Formato esperado do CSV (colunas minimas):
#   callsign, timestamp (ISO8601), lat, lon, altitude_ft
#
# Este parser e propositalmente simples: em producao, os dados de RADAR
# normalmente vem de um sistema de vigilancia (ex: ASTERIX, ADS-B decodificado)
# e ja chegam padronizados. Aqui assumimos um CSV ja extraido desse sistema.

#' Le uma trajetoria de radar a partir de um CSV
#'
#' @param path caminho do arquivo CSV
#' @return data.frame ordenado por tempo com colunas:
#'   callsign, timestamp (POSIXct), lat, lon, altitude_ft
read_radar_track <- function(path) {
  track <- read.csv(path, stringsAsFactors = FALSE)
  required_cols <- c("callsign", "timestamp", "lat", "lon", "altitude_ft")
  missing_cols <- setdiff(required_cols, names(track))
  if (length(missing_cols) > 0) {
    stop("Colunas obrigatorias ausentes no radar track: ",
         paste(missing_cols, collapse = ", "))
  }
  track$timestamp <- as.POSIXct(track$timestamp, tz = "UTC")
  track <- track[order(track$timestamp), ]
  rownames(track) <- NULL
  track
}
