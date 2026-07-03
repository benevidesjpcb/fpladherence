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
#  - 'nr_ssr' e o codigo transponder (squawk). NA com muita frequencia nos
#    dados reais -- NAO USAR como criterio principal de casamento FPL-RADAR.
#    Preferir find_callsign_by_time_location() (horario do FPL + proximidade
#    geografica do aerodromo), abaixo.
#  - Arquivo grande demais para read.csv() na pratica -- usa-se
#    data.table::fread(), bem mais rapido e econômico em memoria.

library(geosphere)

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

#' Faz o parse de 'dt_radar' (texto) para POSIXct UMA UNICA VEZ, guardando o
#' resultado na coluna 'ts'. IMPORTANTE rodar isso uma vez so logo depois de
#' ler o log (read_sigma_radar_log()) e reaproveitar o resultado -- e usa
#' lubridate::ymd_hms() em vez de as.POSIXct(): para milhoes de linhas,
#' as.POSIXct() (mesmo com format explicito) chegou a levar ~20s so para 3
#' milhoes de linhas nos testes, contra ~0.6s do lubridate (~35x mais rapido)
#' -- diferenca que, multiplicada pelos 8.8 milhoes de linhas do arquivo real
#' e por varias chamadas num loop, era o que estava travando o script.
#' sigma_radar_to_track()/find_callsign_by_time_location() usam a coluna
#' 'ts' se ela ja existir, em vez de re-parsear a cada chamada.
#'
#' @param radar_log data.frame/data.table retornado por
#'   read_sigma_radar_log()
#' @return radar_log com a coluna 'ts' (POSIXct) adicionada
parse_radar_timestamps <- function(radar_log) {
  radar_log$ts <- lubridate::ymd_hms(radar_log$dt_radar, tz = "UTC")
  radar_log
}

#' Retorna o timestamp (POSIXct) do log de radar, reaproveitando a coluna
#' 'ts' se ja tiver sido calculada por parse_radar_timestamps() -- senao,
#' calcula na hora (mais lento, mas funciona standalone).
radar_timestamps <- function(radar_log) {
  if ("ts" %in% names(radar_log)) return(radar_log$ts)
  lubridate::ymd_hms(radar_log$dt_radar, tz = "UTC")
}

#' Filtra o log de radar para um unico voo, por codigo de transponder (ssr)
#' e/ou callsign, e converte para o formato canonico usado pelo restante do
#' pipeline (mesmas colunas produzidas por read_radar_track() em
#' R/parse_radar.R): callsign, timestamp (POSIXct), lat, lon, altitude_ft.
#'
#' IMPORTANTE: codigo de transponder (ssr/squawk) e REUTILIZADO por
#' diferentes voos ao longo do dia -- filtrar so por ssr, sem uma janela de
#' horario, pode misturar posicoes de varias aeronaves diferentes num "voo"
#' so (o resultado fica com saltos absurdos de altitude). Por isso e
#' fortemente recomendado informar 'time_window' com o horario real do voo
#' (ver extract_actual_times() em R/parse_sigma_fpl.R).
#'
#' @param radar_log data.frame/data.table retornado por
#'   read_sigma_radar_log()
#' @param ssr codigo de transponder do voo (ex.: 2019); opcional se
#'   callsign for informado
#' @param callsign identificador da aeronave no radar (ex.: "4XCUZ");
#'   opcional se ssr for informado
#' @param time_window vetor de 2 POSIXct, c(inicio, fim) -- mantem so
#'   posicoes dentro dessa janela. Fortemente recomendado quando filtrando
#'   por ssr (ver nota acima).
#' @return data.frame ordenado por tempo, no formato canonico de radar_track
sigma_radar_to_track <- function(radar_log, ssr = NULL, callsign = NULL,
                                  time_window = NULL) {
  if (is.null(ssr) && is.null(callsign)) {
    stop("Informe 'ssr' e/ou 'callsign' para selecionar um unico voo.")
  }

  track <- radar_log
  if (!is.null(ssr)) {
    # comparacao como texto (trimmed) -- evita falso-negativo por zeros a
    # esquerda perdidos ou diferenca de tipo (character vs numeric) entre
    # a leitura do FPL (read.csv) e a do radar (data.table::fread)
    track <- track[trimws(as.character(track$nr_ssr)) == trimws(as.character(ssr)), ]
  }
  if (!is.null(callsign)) {
    cs_clean <- gsub('"', '', track$callsign)
    track <- track[trimws(cs_clean) == trimws(callsign), ]
  }

  ts <- radar_timestamps(track)

  if (!is.null(time_window)) {
    manter <- !is.na(ts) & ts >= time_window[1] & ts <= time_window[2]
    track <- track[manter, ]
    ts <- ts[manter]
  }

  out <- data.frame(
    callsign = trimws(gsub('"', '', track$callsign)),
    timestamp = ts,
    lat = track$vl_latitude,
    lon = track$vl_longitude,
    altitude_ft = track$nr_flightlevel * 100,
    stringsAsFactors = FALSE
  )

  # posicoes sem lat/lon ou sem timestamp existem no dado real e nao servem
  # para nenhuma analise (nem vertical nem horizontal) -- descarta na fonte
  out <- out[!is.na(out$lat) & !is.na(out$lon) & !is.na(out$timestamp), ]

  out <- out[order(out$timestamp), ]
  rownames(out) <- NULL
  out
}

#' Encontra o(s) callsign(s) do radar cuja trajetoria passa perto do ADEP na
#' hora da decolagem E perto do ADES na hora do pouso -- casamento FPL-RADAR
#' por horario + localizacao, em vez de codigo de transponder (ssr), que tem
#' muitos valores faltantes nos dados reais.
#'
#' @param radar_log data.frame/data.table retornado por
#'   read_sigma_radar_log() (colunas: callsign, vl_latitude, vl_longitude,
#'   dt_radar)
#' @param adep_coords c(lat=, lon=) do aerodromo de partida
#' @param ades_coords c(lat=, lon=) do aerodromo de chegada
#' @param dep_time horario estimado/real de decolagem (POSIXct) -- ver
#'   eobt_full em select_filed_plan() ou actual_dep em extract_actual_times()
#' @param arr_time horario estimado/real de pouso (POSIXct) -- ver
#'   actual_arr em extract_actual_times(), ou estimado a partir de dep_time
#' @param max_dist_nm raio (NM) de proximidade aceitavel do aerodromo
#' @param max_time_min janela (min) aceitavel ao redor de dep_time/arr_time
#' @return vetor de callsigns candidatos (aparecem perto do ADEP na janela de
#'   decolagem E perto do ADES na janela de pouso); vazio se nenhum bater
find_callsign_by_time_location <- function(radar_log, adep_coords, ades_coords,
                                            dep_time, arr_time,
                                            max_dist_nm = 30, max_time_min = 30) {
  ts <- radar_timestamps(radar_log)

  perto_dep_time <- !is.na(ts) &
    abs(as.numeric(difftime(ts, dep_time, units = "mins"))) <= max_time_min
  perto_arr_time <- !is.na(ts) &
    abs(as.numeric(difftime(ts, arr_time, units = "mins"))) <= max_time_min

  dist_adep_nm <- rep(NA_real_, nrow(radar_log))
  if (any(perto_dep_time)) {
    dist_adep_nm[perto_dep_time] <- distHaversine(
      cbind(radar_log$vl_longitude[perto_dep_time], radar_log$vl_latitude[perto_dep_time]),
      c(adep_coords["lon"], adep_coords["lat"])
    ) / 1852
  }

  dist_ades_nm <- rep(NA_real_, nrow(radar_log))
  if (any(perto_arr_time)) {
    dist_ades_nm[perto_arr_time] <- distHaversine(
      cbind(radar_log$vl_longitude[perto_arr_time], radar_log$vl_latitude[perto_arr_time]),
      c(ades_coords["lon"], ades_coords["lat"])
    ) / 1852
  }

  cs <- trimws(gsub('"', '', radar_log$callsign))
  candidatos_dep <- unique(cs[perto_dep_time & !is.na(dist_adep_nm) & dist_adep_nm <= max_dist_nm])
  candidatos_arr <- unique(cs[perto_arr_time & !is.na(dist_ades_nm) & dist_ades_nm <= max_dist_nm])

  intersect(candidatos_dep, candidatos_arr)
}
