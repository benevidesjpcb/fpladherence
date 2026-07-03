# Calculo da aderencia vertical: compara o perfil realmente voado (RADAR)
# com o perfil planejado (FPL), ponto a ponto, e resume em metricas.
#
# Tolerancias por fase (padrao, configuravel):
#  - CRUZEIRO: 300 ft   (referencia proxima a banda RVSM de monitoramento)
#  - SUBIDA / DESCIDA: 1500 ft (fases naturalmente mais variaveis, vetoradas
#    por ATC; tolerancia mais larga evita marcar como "nao aderente" uma
#    variabilidade operacional normal)

default_tolerance_ft <- c(SUBIDA = 1500, CRUZEIRO = 300, DESCIDA = 1500)

#' Combina a trajetoria de radar (ja projetada na rota, com dist_nm) com o
#' perfil planejado, calculando o desvio vertical ponto a ponto.
#'
#' @param radar_track radar com colunas timestamp, altitude_ft, dist_nm
#' @param planned_profile rota planejada (build_planned_profile())
#' @param tolerance_ft vetor nomeado de tolerancia (ft) por fase
#' @param transition_nm largura (NM) da janela de transicao de subida em
#'   degrau (ver interpolate_planned_profile())
#' @return radar_track com colunas: planned_alt_ft, phase, deviation_ft,
#'   tolerance_ft, is_adherent
compute_vertical_deviation <- function(radar_track, planned_profile,
                                        tolerance_ft = default_tolerance_ft,
                                        transition_nm = default_transition_nm) {
  interp <- interpolate_planned_profile(radar_track$dist_nm, planned_profile,
                                         transition_nm = transition_nm)

  radar_track$planned_alt_ft <- interp$planned_alt_ft
  radar_track$phase <- interp$phase
  radar_track$deviation_ft <- radar_track$altitude_ft - radar_track$planned_alt_ft
  radar_track$tolerance_ft <- tolerance_ft[radar_track$phase]
  radar_track$is_adherent <- abs(radar_track$deviation_ft) <= radar_track$tolerance_ft

  radar_track
}

#' Resume a aderencia vertical por fase de voo e no geral
#'
#' @param matched data.frame retornado por compute_vertical_deviation()
#' @return data.frame com uma linha por fase + linha "GERAL", colunas:
#'   phase, n_pontos, pct_aderencia, desvio_medio_ft, desvio_abs_medio_ft,
#'   desvio_max_ft, rmse_ft
summarise_vertical_adherence <- function(matched) {
  summarise_group <- function(df, label) {
    data.frame(
      fase = label,
      n_pontos = nrow(df),
      pct_aderencia = round(100 * mean(df$is_adherent), 1),
      desvio_medio_ft = round(mean(df$deviation_ft), 0),
      desvio_abs_medio_ft = round(mean(abs(df$deviation_ft)), 0),
      desvio_max_ft = round(max(abs(df$deviation_ft)), 0),
      rmse_ft = round(sqrt(mean(df$deviation_ft^2)), 0)
    )
  }

  phases <- unique(matched$phase)
  by_phase <- do.call(rbind, lapply(phases, function(p) {
    summarise_group(matched[matched$phase == p, ], p)
  }))
  overall <- summarise_group(matched, "GERAL")

  result <- rbind(by_phase, overall)
  rownames(result) <- NULL
  result
}
