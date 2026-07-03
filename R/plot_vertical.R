# Visualizacao: perfil vertical real (RADAR) sobreposto ao perfil planejado
# (FPL), com banda de tolerancia por fase de voo. Ver plot_horizontal.R para
# a visualizacao lateral (rota).

library(ggplot2)

#' Plota o perfil vertical planejado (FPL) x realizado (RADAR) em funcao da
#' distancia percorrida ao longo da rota (NM).
#'
#' @param matched data.frame retornado por compute_vertical_deviation()
#'   (colunas: dist_nm, altitude_ft, planned_alt_ft, tolerance_ft, phase,
#'   is_adherent)
#' @return objeto ggplot
plot_vertical_adherence <- function(matched) {
  matched <- matched[order(matched$dist_nm), ]

  ggplot(matched, aes(x = dist_nm)) +
    geom_ribbon(aes(ymin = planned_alt_ft - tolerance_ft,
                     ymax = planned_alt_ft + tolerance_ft),
                fill = "steelblue", alpha = 0.15) +
    geom_line(aes(y = planned_alt_ft, linetype = "Planejado (FPL)"),
              color = "steelblue", linewidth = 0.9) +
    geom_line(aes(y = altitude_ft, color = phase, group = 1),
              linewidth = 0.9) +
    geom_point(aes(y = altitude_ft, color = phase, shape = is_adherent),
               size = 1.6) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4),
                        labels = c(`TRUE` = "Aderente", `FALSE` = "Nao aderente"),
                        name = "Aderencia") +
    scale_color_manual(values = c(SUBIDA = "#e07b39", CRUZEIRO = "#2f9e44",
                                   DESCIDA = "#9c36b5"),
                        name = "Fase (real)") +
    scale_linetype_manual(values = c("Planejado (FPL)" = "dashed"), name = NULL) +
    labs(
      title = "Aderencia vertical ao plano de voo",
      subtitle = "Perfil real (RADAR) vs. perfil planejado (FPL), com banda de tolerancia por fase",
      x = "Distancia ao longo da rota (NM)",
      y = "Altitude (ft)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.box = "vertical")
}

#' Plota o perfil vertical realizado (RADAR) x nivel filed no FPL, em funcao
#' do tempo -- versao sem navdata (ver R/vertical_adherence_radar_only.R).
#'
#' @param matched resultado de compute_vertical_deviation_radar_only()
#' @param filed_level_ft nivel de cruzeiro arquivado no FPL (pes)
#' @param cruise_tolerance_ft tolerancia (pes) usada no trecho de cruzeiro
#' @return objeto ggplot
plot_vertical_adherence_radar_only <- function(matched, filed_level_ft,
                                                cruise_tolerance_ft = 300) {
  matched <- matched[order(matched$timestamp), ]

  ggplot(matched, aes(x = timestamp, y = altitude_ft)) +
    annotate("rect", xmin = min(matched$timestamp), xmax = max(matched$timestamp),
             ymin = filed_level_ft - cruise_tolerance_ft,
             ymax = filed_level_ft + cruise_tolerance_ft,
             fill = "steelblue", alpha = 0.15) +
    geom_hline(yintercept = filed_level_ft, linetype = "dashed", color = "steelblue") +
    geom_line(aes(color = phase, group = 1), linewidth = 0.9) +
    geom_point(aes(color = phase), size = 1.2) +
    scale_color_manual(values = c(SUBIDA = "#e07b39", CRUZEIRO = "#2f9e44",
                                  DESCIDA = "#9c36b5"), name = "Fase (detectada)") +
    labs(
      title = "Aderencia vertical ao plano de voo (sem navdata)",
      subtitle = paste0("Nivel filed: ", filed_level_ft,
                         " ft -- fases detectadas pela taxa de subida/descida do radar"),
      x = "Horario", y = "Altitude (ft)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}
