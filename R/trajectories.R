# ETAPA 1 do pipeline: extracao de trajetorias a partir do radar bruto.
#
# Ideia central de performance: o arquivo de radar bruto (milhoes de linhas,
# todas as aeronaves do dia) e lido e processado UMA VEZ SO aqui, produzindo
# uma tabela de trajetorias ja limpa e segmentada por voo (fid), com a
# origem/destino de cada voo ja DETECTADOS como colunas. As etapas seguintes
# (comparacao com o plano de voo, analise por par de cidades) trabalham em
# cima dessa tabela compacta -- nunca mais releem o bruto. Isso e o que
# permite escalar para um mes/ano (basta empilhar as trajetorias de varios
# dias, ja processadas).
#
# Tudo em data.table por causa da escala (sort/agrupamento de milhoes de
# linhas). A representacao de saida e "uma linha por posicao" (radar limpo +
# fid + adep/ades detectados); a geometria por voo (LINESTRING via sf) fica
# para a etapa de comparacao, onde de fato ajuda.

library(data.table)
library(geosphere)

#' Limpa o log de radar bruto e o converte para o formato canonico de
#' posicoes, ja como data.table: callsign, ts (POSIXct), lat, lon,
#' altitude_ft. Descarta posicoes sem lat/lon/timestamp (inuteis).
#'
#' @param radar_log data.table retornado por read_sigma_radar_log()
#' @return data.table com colunas callsign, ts, lat, lon, altitude_ft
clean_radar_log <- function(radar_log) {
  dt <- data.table::as.data.table(radar_log)

  cs <- trimws(gsub('"', '', dt$callsign, fixed = TRUE))
  ts <- lubridate::ymd_hms(dt$dt_radar, tz = "UTC")

  out <- data.table::data.table(
    callsign = cs,
    ts = ts,
    lat = as.numeric(dt$vl_latitude),
    lon = as.numeric(dt$vl_longitude),
    altitude_ft = as.numeric(dt$nr_flightlevel) * 100
  )
  out <- out[!is.na(lat) & !is.na(lon) & !is.na(ts) & callsign != ""]
  out
}

#' Segmenta as posicoes limpas em voos (fid): um novo voo comeca quando o
#' callsign muda OU quando ha uma lacuna de tempo maior que 'max_gap_min'
#' (mesma logica do BRA-HFE). Ordena por callsign + tempo.
#'
#' @param positions data.table de clean_radar_log()
#' @param max_gap_min lacuna maxima (min) considerada "mesmo voo"
#' @return positions com coluna 'fid' (inteiro, 1 por trecho continuo)
segment_trajectories <- function(positions, max_gap_min = 30) {
  data.table::setorder(positions, callsign, ts)
  n <- nrow(positions)
  if (n == 0) {
    positions[, fid := integer(0)]
    return(positions)
  }
  gap_min <- c(Inf, as.numeric(diff(positions$ts), units = "mins"))
  novo_voo <- c(TRUE, positions$callsign[-1] != positions$callsign[-n]) |
    (gap_min > max_gap_min)
  positions[, fid := cumsum(novo_voo)]
  positions
}

#' Detecta origem/destino de cada voo (fid): o aerodromo mais proximo da
#' PRIMEIRA e da ULTIMA posicao de cada trajetoria, desde que dentro de
#' 'max_dist_nm' (senao NA). Tambem devolve resumo por voo (n posicoes,
#' hora de inicio/fim).
#'
#' @param positions data.table segmentado (com fid), de segment_trajectories()
#' @param airports_db data.frame com colunas icao, latitude, longitude
#' @param max_dist_nm raio (NM) para aceitar um aerodromo como origem/destino
#' @return data.table (indice de voos): fid, callsign, adep_det, ades_det,
#'   n_pos, t_start, t_end, dist_adep_nm, dist_ades_nm
detect_od_per_flight <- function(positions, airports_db, max_dist_nm = 30) {
  # extremos de cada voo (primeira e ultima posicao, ja ordenado por ts)
  extremos <- positions[, .(
    callsign = callsign[1],
    n_pos = .N,
    t_start = ts[1], t_end = ts[.N],
    lat_first = lat[1], lon_first = lon[1],
    lat_last = lat[.N], lon_last = lon[.N]
  ), by = fid]

  ap_lon <- airports_db$longitude
  ap_lat <- airports_db$latitude
  ap_icao <- airports_db$icao

  nearest_airport <- function(lon_v, lat_v) {
    icao <- character(length(lon_v))
    dist_nm <- numeric(length(lon_v))
    for (i in seq_along(lon_v)) {
      d_nm <- distHaversine(c(lon_v[i], lat_v[i]), cbind(ap_lon, ap_lat)) / 1852
      j <- which.min(d_nm)
      if (d_nm[j] <= max_dist_nm) {
        icao[i] <- ap_icao[j]; dist_nm[i] <- d_nm[j]
      } else {
        icao[i] <- NA_character_; dist_nm[i] <- NA_real_
      }
    }
    list(icao = icao, dist_nm = dist_nm)
  }

  dep <- nearest_airport(extremos$lon_first, extremos$lat_first)
  arr <- nearest_airport(extremos$lon_last, extremos$lat_last)

  data.table::data.table(
    fid = extremos$fid, callsign = extremos$callsign,
    adep_det = dep$icao, ades_det = arr$icao,
    n_pos = extremos$n_pos, t_start = extremos$t_start, t_end = extremos$t_end,
    dist_adep_nm = round(dep$dist_nm, 1), dist_ades_nm = round(arr$dist_nm, 1)
  )
}

#' Escreve uma tabela em Parquet (via 'arrow' ou 'nanoparquet', o que
#' estiver instalado) e/ou CSV. Parquet e o formato para escalar (mes/ano);
#' o CSV serve para amostras que se queira abrir no Excel.
#'
#' @param dt data.frame/data.table a gravar
#' @param out_parquet caminho .parquet (ou NULL para nao gravar)
#' @param out_csv caminho .csv (ou NULL para nao gravar)
#' @param csv_sample_n se informado e a tabela for maior, grava so as
#'   primeiras N linhas no CSV (o Parquet leva sempre a tabela inteira)
write_trajectory_table <- function(dt, out_parquet = NULL, out_csv = NULL,
                                    csv_sample_n = NULL) {
  df <- as.data.frame(dt)

  if (!is.null(out_parquet)) {
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::write_parquet(df, out_parquet)
    } else if (requireNamespace("nanoparquet", quietly = TRUE)) {
      nanoparquet::write_parquet(df, out_parquet)
    } else {
      warning("Nem 'arrow' nem 'nanoparquet' instalados -- Parquet NAO gravado (",
              out_parquet, "). Rode install.packages('arrow') na sua maquina.")
    }
  }

  if (!is.null(out_csv)) {
    d <- if (!is.null(csv_sample_n) && nrow(df) > csv_sample_n) df[seq_len(csv_sample_n), ] else df
    data.table::fwrite(d, out_csv)
  }

  invisible(NULL)
}
