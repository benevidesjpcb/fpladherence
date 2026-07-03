# Metodologia de Eficiencia Horizontal de Voo (HFE) no molde EUROCONTROL/PRU,
# replicando a abordagem do repositorio euctrl-pru/BRA-HFE (branch
# dev/radar-hfe-analysis): marcos de distancia (40NM/100NM a partir do
# ADEP/ADES) em vez de comparar o trecho inteiro ADEP-ADES de uma vez --
# isso exclui a area terminal (onde a rota e naturalmente indireta por causa
# de SID/STAR) do calculo de eficiencia, olhando so para o trecho en-route.
#
# Segmentacao de trajetoria: um "voo" no radar (fid) e um trecho continuo de
# posicoes com o MESMO callsign e sem lacuna de tempo maior que
# 'max_gap_min' (30 min, igual ao BRA-HFE) -- independente de casar com o
# FPL. So depois de segmentar e que se verifica quais desses trechos passam
# perto do par de aeroportos de interesse.

library(geosphere)
library(dplyr)
library(lubridate)

#' Converte o log de radar SIGMA inteiro (sem filtrar por voo) para o
#' formato canonico (callsign, timestamp, lat, lon, altitude_ft) -- usado
#' como entrada para segment_radar_by_callsign_gap().
#'
#' @param radar_log data.frame/data.table retornado por
#'   read_sigma_radar_log()
#' @return data.frame no formato canonico, ordenado por callsign e tempo
sigma_radar_to_canonical <- function(radar_log) {
  dt_radar <- radar_log$dt_radar
  dt_radar[trimws(dt_radar) == ""] <- NA

  out <- data.frame(
    callsign = trimws(gsub('"', '', radar_log$callsign)),
    timestamp = as.POSIXct(dt_radar, tz = "UTC"),
    lat = radar_log$vl_latitude,
    lon = radar_log$vl_longitude,
    altitude_ft = radar_log$nr_flightlevel * 100,
    stringsAsFactors = FALSE
  )
  out[!is.na(out$timestamp), ]
}

#' Segmenta o radar do dia inteiro em trechos continuos por voo (fid): uma
#' nova segmentacao comeca quando o callsign muda OU quando ha uma lacuna de
#' tempo maior que 'max_gap_min' -- igual ao BRA-HFE (dev/radar-hfe-analysis).
#'
#' @param radar_canonical data.frame no formato canonico (ver
#'   sigma_radar_to_canonical())
#' @param max_gap_min lacuna maxima (minutos) considerada "mesmo voo"
#' @return radar_canonical ordenado, com coluna fid (identificador do trecho)
segment_radar_by_callsign_gap <- function(radar_canonical, max_gap_min = 30) {
  radar_canonical |>
    arrange(callsign, timestamp) |>
    mutate(
      delta = if_else(
        (!is.na(lag(callsign)) & callsign != lag(callsign)) |
          (!is.na(lag(timestamp)) & (timestamp - lag(timestamp)) > minutes(max_gap_min)),
        1, 0
      ),
      fid = cumsum(delta)
    ) |>
    select(-delta)
}

#' Identifica, entre os trechos segmentados (fid), quais comecam perto de um
#' aerodromo e terminam perto do outro -- candidatos ao par de cidades de
#' interesse (em qualquer sentido, ADEP->ADES ou ADES->ADEP).
#'
#' @param segmented resultado de segment_radar_by_callsign_gap()
#' @param city_a c(lat=, lon=) do 1o aerodromo
#' @param city_b c(lat=, lon=) do 2o aerodromo
#' @param near_nm raio (NM) de proximidade aceitavel do aerodromo
#' @param min_pontos numero minimo de posicoes no trecho para considerar
#' @return data.frame com colunas fid, direcao ("A_PARA_B"/"B_PARA_A"),
#'   n_pontos, hora_inicio, hora_fim
identify_city_pair_flights <- function(segmented, city_a, city_b,
                                        near_nm = 50, min_pontos = 10) {
  resumo <- segmented |>
    group_by(fid) |>
    summarise(
      n_pontos = n(),
      callsign = first(callsign),
      hora_inicio = first(timestamp),
      hora_fim = last(timestamp),
      lat_inicio = first(lat), lon_inicio = first(lon),
      lat_fim = last(lat), lon_fim = last(lon),
      .groups = "drop"
    ) |>
    filter(n_pontos >= min_pontos)

  dist_inicio_a <- distHaversine(cbind(resumo$lon_inicio, resumo$lat_inicio),
                                   c(city_a["lon"], city_a["lat"])) / 1852
  dist_fim_b <- distHaversine(cbind(resumo$lon_fim, resumo$lat_fim),
                                c(city_b["lon"], city_b["lat"])) / 1852
  dist_inicio_b <- distHaversine(cbind(resumo$lon_inicio, resumo$lat_inicio),
                                   c(city_b["lon"], city_b["lat"])) / 1852
  dist_fim_a <- distHaversine(cbind(resumo$lon_fim, resumo$lat_fim),
                                c(city_a["lon"], city_a["lat"])) / 1852

  a_para_b <- resumo[dist_inicio_a <= near_nm & dist_fim_b <= near_nm, ]
  if (nrow(a_para_b) > 0) a_para_b$direcao <- "A_PARA_B"
  b_para_a <- resumo[dist_inicio_b <= near_nm & dist_fim_a <= near_nm, ]
  if (nrow(b_para_a) > 0) b_para_a$direcao <- "B_PARA_A"

  rbind(a_para_b, b_para_a)
}

#' Distancia grande-circulo (NM) entre um vetor de pontos e um ponto fixo
#'
#' @param lat,lon vetores de latitude/longitude dos pontos
#' @param ref_coords c(lat=, lon=) do ponto fixo de referencia
#' @return vetor de distancias em NM
gc_distance_nm <- function(lat, lon, ref_coords) {
  distHaversine(cbind(lon, lat), c(ref_coords["lon"], ref_coords["lat"])) / 1852
}

#' Extrai os marcos de distancia de um trecho de trajetoria (um unico fid):
#' FIRST_HIT, 40NM_ADEP, 100NM_ADEP, 100NM_ADES, 40NM_ADES, LAST_HIT.
#'
#' @param track data.frame ordenado por tempo, com colunas timestamp, lat, lon
#' @param adep_coords c(lat=, lon=) do aerodromo de partida (real, do trecho)
#' @param ades_coords c(lat=, lon=) do aerodromo de chegada (real, do trecho)
#' @param thresholds_nm vetor com os 2 limiares de distancia (NM), padrao
#'   c(40, 100)
#' @return data.frame com colunas: milestone, idx (linha em 'track'),
#'   timestamp, lat, lon, dist_from_adep_nm, dist_to_ades_nm
extract_milestones <- function(track, adep_coords, ades_coords,
                                thresholds_nm = c(40, 100)) {
  track <- track[order(track$timestamp), ]
  n <- nrow(track)

  dist_from_adep <- gc_distance_nm(track$lat, track$lon, adep_coords)
  dist_to_ades <- gc_distance_nm(track$lat, track$lon, ades_coords)

  idx_40_adep <- which(dist_from_adep >= thresholds_nm[1])[1]
  idx_100_adep <- which(dist_from_adep >= thresholds_nm[2])[1]
  idx_100_ades <- max(which(dist_to_ades >= thresholds_nm[2]))
  idx_40_ades <- max(which(dist_to_ades >= thresholds_nm[1]))

  idxs <- c(FIRST_HIT = 1, `40NM_ADEP` = idx_40_adep, `100NM_ADEP` = idx_100_adep,
            `100NM_ADES` = idx_100_ades, `40NM_ADES` = idx_40_ades, LAST_HIT = n)

  data.frame(
    milestone = names(idxs), idx = as.integer(idxs),
    timestamp = track$timestamp[idxs],
    lat = track$lat[idxs], lon = track$lon[idxs],
    dist_from_adep_nm = dist_from_adep[idxs],
    dist_to_ades_nm = dist_to_ades[idxs],
    stringsAsFactors = FALSE
  )
}

#' Distancia realmente voada (NM) entre dois indices de um trecho de
#' trajetoria (soma das distancias grande-circulo entre posicoes consecutivas)
flown_distance_nm <- function(track, idx_start, idx_end) {
  sub <- track[idx_start:idx_end, ]
  n <- nrow(sub)
  if (n < 2) return(0)
  sum(distHaversine(as.matrix(sub[-n, c("lon", "lat")]),
                      as.matrix(sub[-1, c("lon", "lat")]))) / 1852
}

#' Calcula a Eficiencia Horizontal de Voo (HFE) entre dois marcos, usando a
#' formula EUROCONTROL: a "distancia alcancada" e a media entre (i) quanto o
#' voo se aproximou do destino e (ii) quanto se afastou da origem, em termos
#' de distancia grande-circulo -- mais robusto que a distancia direta simples
#' entre os dois pontos-marco, porque usa toda a informacao de
#' aproximacao/afastamento ao longo do trecho.
#'
#' @param track data.frame ordenado por tempo (mesmo usado em
#'   extract_milestones())
#' @param milestones resultado de extract_milestones()
#' @param from,to nomes dos marcos que delimitam o trecho (ex.: "100NM_ADEP",
#'   "100NM_ADES")
#' @return data.frame com flown_dist_nm, achieved_dist_nm, hfe_pct
compute_hfe <- function(track, milestones, from, to) {
  m_from <- milestones[milestones$milestone == from, ]
  m_to <- milestones[milestones$milestone == to, ]

  flown <- flown_distance_nm(track, m_from$idx, m_to$idx)
  achieved <- ((m_from$dist_to_ades_nm - m_to$dist_to_ades_nm) +
                 (m_to$dist_from_adep_nm - m_from$dist_from_adep_nm)) / 2

  data.frame(
    trecho = paste(from, "->", to),
    flown_dist_nm = round(flown, 1),
    achieved_dist_nm = round(achieved, 1),
    hfe_pct = round(100 * achieved / flown, 1)
  )
}
