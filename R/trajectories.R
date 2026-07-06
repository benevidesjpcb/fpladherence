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

#' Limpa string de codigo ICAO: remove aspas/espacos, poe em maiuscula, e
#' converte vazio ("") para NA.
.clean_icao <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- toupper(trimws(gsub('"', '', x, fixed = TRUE)))
  x[x == ""] <- NA_character_
  x
}

#' Limpa o log de radar bruto e o converte para o formato canonico de
#' posicoes, ja como data.table: callsign, ts (POSIXct), lat, lon,
#' altitude_ft, addep, addes. Descarta posicoes sem lat/lon/timestamp.
#'
#' addep/addes sao a ORIGEM/DESTINO que ja vem no proprio radar (fonte
#' primaria de O/D -- ver resolve_flight_od()). Se essas colunas nao
#' existirem no arquivo, sao criadas como NA.
#'
#' @param radar_log data.table retornado por read_sigma_radar_log()
#' @return data.table com colunas callsign, ts, lat, lon, altitude_ft,
#'   addep, addes
clean_radar_log <- function(radar_log) {
  dt <- data.table::as.data.table(radar_log)

  cs <- trimws(gsub('"', '', dt$callsign, fixed = TRUE))
  ts <- lubridate::ymd_hms(dt$dt_radar, tz = "UTC")

  out <- data.table::data.table(
    callsign = cs,
    ts = ts,
    lat = as.numeric(dt$vl_latitude),
    lon = as.numeric(dt$vl_longitude),
    altitude_ft = as.numeric(dt$nr_flightlevel) * 100,
    addep = if ("addep" %in% names(dt)) .clean_icao(dt$addep) else NA_character_,
    addes = if ("addes" %in% names(dt)) .clean_icao(dt$addes) else NA_character_
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

#' Adiciona a DISTANCIA ACUMULADA VOADA (NM) ao longo da trajetoria de cada
#' voo (fid): soma das distancias grande-circulo entre posicoes consecutivas,
#' comecando em 0 no primeiro ponto do voo. Equivale ao cumulative_distance()
#' do pacote trrrj (EUROCONTROL), mas nativo (sem a dependencia, que e dificil
#' de instalar) -- e a base do PERFIL VERTICAL (altitude vs distancia voada),
#' separado do caminho horizontal (lat/lon).
#'
#' @param positions data.table segmentado (com fid, lat, lon, ts)
#' @return positions (ordenado por fid, ts) com coluna cum_dist_nm
add_cumulative_flown_distance <- function(positions) {
  data.table::setorder(positions, fid, ts)
  n <- nrow(positions)
  if (n == 0) { positions[, cum_dist_nm := numeric(0)]; return(positions) }
  if (n == 1) { positions[, cum_dist_nm := 0]; return(positions) }

  # distancia entre posicoes consecutivas; zera na fronteira entre voos
  d_nm <- c(0, distHaversine(
    cbind(positions$lon[-n], positions$lat[-n]),
    cbind(positions$lon[-1], positions$lat[-1])
  ) / 1852)
  fronteira <- c(TRUE, positions$fid[-1] != positions$fid[-n])
  d_nm[fronteira] <- 0

  positions[, .seg_nm := d_nm]
  positions[, cum_dist_nm := cumsum(.seg_nm), by = fid]
  positions[, .seg_nm := NULL]
  positions
}

#' Aerodromo mais proximo de cada ponto (lon_v, lat_v), dentro de
#' 'max_dist_nm' (senao NA). Loop so sobre os poucos pontos passados
#' (tipicamente os extremos dos voos que ficaram sem O/D no radar).
#'
#' @return list(icao = vetor de codigos, dist_nm = distancias)
nearest_airport <- function(lon_v, lat_v, airports_db, max_dist_nm) {
  ap_lon <- airports_db$longitude; ap_lat <- airports_db$latitude
  ap_icao <- airports_db$icao
  icao <- rep(NA_character_, length(lon_v))
  dist_nm <- rep(NA_real_, length(lon_v))
  for (i in seq_along(lon_v)) {
    if (is.na(lon_v[i]) || is.na(lat_v[i])) next
    d_nm <- distHaversine(c(lon_v[i], lat_v[i]), cbind(ap_lon, ap_lat)) / 1852
    j <- which.min(d_nm)
    if (d_nm[j] <= max_dist_nm) { icao[i] <- ap_icao[j]; dist_nm[i] <- d_nm[j] }
  }
  list(icao = icao, dist_nm = dist_nm)
}

#' Resolve a origem/destino (ADEP/ADES) de cada voo (fid).
#'
#' FONTE PRIMARIA: as colunas addep/addes que ja vem no proprio radar
#' (pega o primeiro valor nao-NA de cada voo). O radar e a fonte de verdade
#' de O/D -- a maioria dos voos ja tem isso preenchido.
#'
#' FALLBACK (so quando addep/addes estao NA): detecta geometricamente o
#' aerodromo mais proximo da primeira/ultima posicao, com um raio PEQUENO
#' ('fallback_radius_nm', padrao 5 NM) para nao pegar um aerodromo errado
#' no caminho -- se nada estiver perto o suficiente, fica NA (melhor NA do
#' que O/D errado).
#'
#' NOTA (refinamento futuro): outra forma de preencher os NA seria casar por
#' indicativo (callsign) + horario com o plano de voo (que tem adep/ades),
#' ou com registros de outros dias do mesmo voo -- fica para a integracao
#' com a Etapa 2 (FPL). A coluna 'od_src' marca de onde veio cada O/D.
#'
#' @param positions data.table segmentado (com fid, addep, addes), de
#'   segment_trajectories() sobre clean_radar_log()
#' @param airports_db data.frame com colunas icao, latitude, longitude
#' @param fallback_radius_nm raio (NM) do fallback geometrico (padrao 5)
#' @return data.table (indice de voos): fid, callsign, adep_det, ades_det,
#'   od_src ("radar"/"trajectory"/NA), n_pos, t_start, t_end
resolve_flight_od <- function(positions, airports_db, fallback_radius_nm = 5) {
  first_non_na <- function(x) { v <- x[!is.na(x)]; if (length(v)) v[1] else NA_character_ }

  info <- positions[, .(
    callsign = callsign[1],
    n_pos = .N,
    t_start = ts[1], t_end = ts[.N],
    adep_radar = first_non_na(addep),
    ades_radar = first_non_na(addes),
    lat_first = lat[1], lon_first = lon[1],
    lat_last = lat[.N], lon_last = lon[.N]
  ), by = fid]

  info[, adep_det := adep_radar]
  info[, ades_det := ades_radar]
  info[, adep_src := data.table::fifelse(!is.na(adep_radar), "radar", NA_character_)]
  info[, ades_src := data.table::fifelse(!is.na(ades_radar), "radar", NA_character_)]

  # fallback geometrico so onde o radar nao trouxe O/D
  need_dep <- is.na(info$adep_det)
  if (any(need_dep)) {
    nd <- nearest_airport(info$lon_first[need_dep], info$lat_first[need_dep],
                          airports_db, fallback_radius_nm)
    info$adep_det[need_dep] <- nd$icao
    info$adep_src[need_dep] <- data.table::fifelse(!is.na(nd$icao), "trajectory", NA_character_)
  }
  need_arr <- is.na(info$ades_det)
  if (any(need_arr)) {
    na_ <- nearest_airport(info$lon_last[need_arr], info$lat_last[need_arr],
                          airports_db, fallback_radius_nm)
    info$ades_det[need_arr] <- na_$icao
    info$ades_src[need_arr] <- data.table::fifelse(!is.na(na_$icao), "trajectory", NA_character_)
  }

  info[, .(fid, callsign, adep_det, ades_det, adep_src, ades_src,
           n_pos, t_start, t_end)]
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
