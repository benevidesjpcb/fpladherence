# Aderencia vertical SEM navdata: detecta as fases de voo (subida / cruzeiro
# / descida) diretamente pela taxa de variacao de altitude do proprio RADAR,
# e compara o nivel voado em cruzeiro contra o NIVEL UNICO arquivado no FPL
# (coluna 'lvl'). Nao depende de resolver coordenadas dos pontos da rota
# (ver limitacao de navdata em route_geometry.R / analysis/vertical_adherence.qmd).
#
# Util quando so se tem o FPL (nivel filed) e o RADAR (trajetoria), sem uma
# base de waypoints/aerovias -- e o caso hoje com os dados reais do SIGMA.
#
# Metodologia:
#  - taxa de variacao (ft/min) entre pontos consecutivos do radar, suavizada
#    (media movel) para reduzir ruido de medicao;
#  - SUBIDA: taxa > climb_threshold_fpm; DESCIDA: taxa < descent_threshold_fpm;
#    CRUZEIRO: caso contrario;
#  - aderencia so e avaliada no trecho de CRUZEIRO (unico nivel comparavel
#    sem uma referencia continua de subida/descida); em SUBIDA/DESCIDA
#    reporta-se apenas a duracao e o nivel alcancado, sem tolerancia (o FPL
#    nao descreve perfil de subida/descida).

#' Preenche NA "carregando" o ultimo valor valido anterior (LOCF); NAs no
#' inicio do vetor sao preenchidos com o primeiro valor valido.
fill_na_locf <- function(x) {
  if (all(is.na(x))) return(x)
  idx <- which(!is.na(x))
  if (idx[1] > 1) x[1:(idx[1] - 1)] <- x[idx[1]]
  idx <- which(!is.na(x))
  rep_idx <- findInterval(seq_along(x), idx)
  x[idx][rep_idx]
}

#' Mantem apenas o maior trecho continuo de um radar_track, onde "continuo"
#' significa sem lacunas de tempo maiores que 'max_gap_min'. Rede de
#' seguranca contra ssr reutilizado por outro voo (posicoes de um voo
#' diferente tendem a aparecer isoladas, com uma lacuna grande antes/depois).
#'
#' @param radar_track data.frame ordenado por tempo, com coluna timestamp
#' @param max_gap_min lacuna maxima (minutos) considerada "mesmo voo"
#' @return subconjunto de radar_track (o maior trecho continuo)
keep_largest_contiguous_segment <- function(radar_track, max_gap_min = 5) {
  radar_track <- radar_track[order(radar_track$timestamp), ]
  if (nrow(radar_track) <= 1) return(radar_track)

  gap_min <- c(0, diff(as.numeric(radar_track$timestamp)) / 60)
  segmento <- cumsum(gap_min > max_gap_min) + 1

  tamanhos <- table(segmento)
  maior <- as.integer(names(tamanhos)[which.max(tamanhos)])

  out <- radar_track[segmento == maior, ]
  rownames(out) <- NULL
  out
}

#' Suaviza um vetor numerico por media movel centrada
moving_average <- function(x, window = 5) {
  n <- length(x)
  half <- window %/% 2
  out <- numeric(n)
  for (i in seq_len(n)) {
    lo <- max(1, i - half)
    hi <- min(n, i + half)
    out[i] <- mean(x[lo:hi], na.rm = TRUE)
  }
  out
}

#' Detecta a fase de voo (SUBIDA/CRUZEIRO/DESCIDA) de cada ponto do radar a
#' partir da taxa de variacao de altitude (ft/min), sem depender de rota.
#'
#' @param radar_track data.frame ordenado por tempo, com colunas timestamp
#'   (POSIXct) e altitude_ft
#' @param climb_threshold_fpm taxa (ft/min) acima da qual e considerado SUBIDA
#' @param descent_threshold_fpm taxa (ft/min) abaixo da qual e considerado
#'   DESCIDA (valor negativo)
#' @param smooth_window janela (n. de pontos) da media movel de suavizacao
#' @return radar_track com colunas: vertical_rate_fpm, phase
detect_flight_phases <- function(radar_track, climb_threshold_fpm = 300,
                                  descent_threshold_fpm = -300,
                                  smooth_window = 5) {
  # leituras com timestamp duplicado (comuns em dados reais, ex.: mais de uma
  # fonte de radar) tornam a taxa de variacao indefinida (dt = 0) -- mantem
  # so a primeira ocorrencia de cada instante.
  radar_track <- radar_track[!duplicated(radar_track$timestamp), ]
  rownames(radar_track) <- NULL

  n <- nrow(radar_track)
  dt_min <- c(NA, diff(as.numeric(radar_track$timestamp)) / 60)
  d_alt <- c(NA, diff(radar_track$altitude_ft))
  raw_rate <- d_alt / dt_min
  raw_rate[!is.finite(raw_rate)] <- NA # guarda contra Inf/NaN residual
  raw_rate[1] <- raw_rate[2] # aproxima o primeiro ponto pelo seguinte

  rate <- moving_average(raw_rate, window = smooth_window)
  rate <- fill_na_locf(rate) # nao deixa NA/NaN chegar na classificacao de fase

  phase <- ifelse(rate > climb_threshold_fpm, "SUBIDA",
                   ifelse(rate < descent_threshold_fpm, "DESCIDA", "CRUZEIRO"))

  radar_track$vertical_rate_fpm <- round(rate, 0)
  radar_track$phase <- phase
  radar_track
}

#' Calcula aderencia vertical comparando o radar (com fases ja detectadas)
#' contra o NIVEL UNICO arquivado no FPL. So se aplica tolerancia no trecho
#' de CRUZEIRO; em SUBIDA/DESCIDA nao ha uma referencia continua do FPL, so
#' se reporta o nivel alcancado.
#'
#' @param radar_track resultado de detect_flight_phases()
#' @param filed_level_ft nivel de cruzeiro arquivado no FPL (pes) -- ex.:
#'   level_token_to_ft(plano$lvl) (ver R/parse_fpl.R / R/parse_sigma_fpl.R)
#' @param cruise_tolerance_ft tolerancia (pes) para o trecho de cruzeiro
#' @return radar_track com colunas: deviation_ft (NA fora do cruzeiro),
#'   is_adherent (NA fora do cruzeiro)
compute_vertical_deviation_radar_only <- function(radar_track, filed_level_ft,
                                                   cruise_tolerance_ft = 300) {
  is_cruise <- !is.na(radar_track$phase) & radar_track$phase == "CRUZEIRO"

  radar_track$deviation_ft <- NA_real_
  radar_track$deviation_ft[is_cruise] <- radar_track$altitude_ft[is_cruise] - filed_level_ft

  radar_track$is_adherent <- NA
  radar_track$is_adherent[is_cruise] <- abs(radar_track$deviation_ft[is_cruise]) <= cruise_tolerance_ft

  radar_track
}

#' Resume a aderencia vertical (versao sem navdata): aderencia percentual no
#' cruzeiro, e duracao/altitude alcancada em subida e descida.
#'
#' @param matched resultado de compute_vertical_deviation_radar_only()
#' @return list(cruzeiro = data.frame, subida = data.frame, descida = data.frame)
summarise_vertical_adherence_radar_only <- function(matched) {
  cruzeiro <- matched[!is.na(matched$phase) & matched$phase == "CRUZEIRO", ]

  # nem toda leitura de radar tem nr_flightlevel valido -- ignora essas
  # (na.rm=TRUE) em vez de deixar um unico NA derrubar a metrica inteira
  n_validos <- sum(!is.na(cruzeiro$deviation_ft))
  if (nrow(cruzeiro) == 0 || n_validos == 0) {
    resumo_cruzeiro <- data.frame(n_pontos = nrow(cruzeiro), n_pontos_validos = n_validos,
                                   pct_aderencia = NA, desvio_medio_ft = NA, desvio_max_ft = NA)
  } else {
    resumo_cruzeiro <- data.frame(
      n_pontos = nrow(cruzeiro),
      n_pontos_validos = n_validos,
      pct_aderencia = round(100 * mean(cruzeiro$is_adherent, na.rm = TRUE), 1),
      desvio_medio_ft = round(mean(cruzeiro$deviation_ft, na.rm = TRUE), 0),
      desvio_max_ft = round(max(abs(cruzeiro$deviation_ft), na.rm = TRUE), 0)
    )
  }

  fase_resumo <- function(df) {
    n_validos <- sum(!is.na(df$altitude_ft))
    if (nrow(df) == 0 || n_validos == 0) {
      return(data.frame(n_pontos = nrow(df), duracao_min = NA, alt_alcancada_ft = NA))
    }
    duracao_min <- round(as.numeric(difftime(max(df$timestamp), min(df$timestamp), units = "mins")), 1)
    data.frame(n_pontos = nrow(df), duracao_min = duracao_min,
               alt_alcancada_ft = max(df$altitude_ft, na.rm = TRUE))
  }

  not_na <- !is.na(matched$phase)
  list(
    cruzeiro = resumo_cruzeiro,
    subida = fase_resumo(matched[not_na & matched$phase == "SUBIDA", ]),
    descida = fase_resumo(matched[not_na & matched$phase == "DESCIDA", ])
  )
}
