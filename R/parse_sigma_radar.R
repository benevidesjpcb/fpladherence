# Adaptador para o formato REAL de exportacao de RADAR do DECEA/SIGMA
# (ex.: radar_2025_12_10.csv), alternativa ao leitor generico em
# R/parse_radar.R (que assume colunas ja no formato canonico).
#
# Colunas observadas na amostra real (separador ";", ~8.8 milhoes de linhas
# para um dia inteiro de operacao -- todas as aeronaves, nao um voo so):
#   id, callsign, addep, addes, dh_inicio, dh_fim, ds_acctypes,
#   vl_latitude, vl_longitude, nr_flightlevel, nr_speed, nr_ssr, dt_radar
#
# Notas:
#  - 'nr_flightlevel' esta em NIVEL DE VOO (FL), nao em pes -- multiplicar
#    por 100 para obter altitude_ft.
#  - 'addep'/'addes' costumam vir vazios nesta amostra; 'ds_acctypes' as
#    vezes traz uma lista de cidades (ex.: "['CURITIBA', 'BRASILIA']"),
#    aparentemente uma referencia textual de origem/destino quando
#    addep/addes nao estao preenchidos -- NAO e o tipo de aeronave, apesar
#    do nome da coluna.
#  - 'nr_ssr' e o codigo transponder (squawk), util para casar RADAR com o
#    FPL correspondente quando o callsign nao bate exatamente (ver coluna
#    'ssr' em R/parse_sigma_fpl.R).
#  - Arquivo grande demais para read.csv() na pratica -- usa-se
#    data.table::fread(), bem mais rapido e econômico em memoria.

#' Le o log de posicoes de RADAR exportado do SIGMA
#'
#' @param path caminho do CSV (separador ";")
#' @param ... argumentos adicionais repassados a data.table::fread()
#'   (ex.: nrows = 1e5 para uma amostra, ou select = c(...) para colunas)
#' @return data.frame (data.table) com as colunas originais do arquivo
read_sigma_radar_log <- function(path, ...) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Pacote 'data.table' necessario para ler arquivos de radar grandes.")
  }
  data.table::fread(path, sep = ";", ...)
}

#' Filtra o log de radar para um unico voo, por codigo de transponder (ssr)
#' e/ou callsign, e converte para o formato canonico usado pelo restante do
#' pipeline (mesmas colunas produzidas por read_radar_track() em
#' R/parse_radar.R): callsign, timestamp (POSIXct), lat, lon, altitude_ft.
#'
#' @param radar_log data.frame/data.table retornado por
#'   read_sigma_radar_log()
#' @param ssr codigo de transponder do voo (ex.: 2019); opcional se
#'   callsign for informado
#' @param callsign identificador da aeronave no radar (ex.: "4XCUZ");
#'   opcional se ssr for informado
#' @return data.frame ordenado por tempo, no formato canonico de radar_track
sigma_radar_to_track <- function(radar_log, ssr = NULL, callsign = NULL) {
  if (is.null(ssr) && is.null(callsign)) {
    stop("Informe 'ssr' e/ou 'callsign' para selecionar um unico voo.")
  }

  track <- radar_log
  if (!is.null(ssr)) track <- track[track$nr_ssr == ssr, ]
  if (!is.null(callsign)) {
    cs_clean <- gsub('"', '', track$callsign)
    track <- track[trimws(cs_clean) == trimws(callsign), ]
  }

  out <- data.frame(
    callsign = trimws(gsub('"', '', track$callsign)),
    timestamp = as.POSIXct(track$dt_radar, tz = "UTC"),
    lat = track$vl_latitude,
    lon = track$vl_longitude,
    altitude_ft = track$nr_flightlevel * 100,
    stringsAsFactors = FALSE
  )

  out <- out[order(out$timestamp), ]
  rownames(out) <- NULL
  out
}
