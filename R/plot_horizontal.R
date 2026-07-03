# Visualizacao lateral (horizontal): trajetoria realmente voada (RADAR) x
# plano de voo (FPL) -- rota planejada (quando resolvida via navdata) e/ou
# aerodromos de partida/chegada com a linha direta (grande circulo) de
# referencia.

library(ggplot2)

#' Plota a trajetoria do radar sobre um plano lat/lon, comparando com a rota
#' planejada do FPL. Funciona em dois niveis de detalhe:
#'  - so com ADEP/ADES (sempre disponivel): mostra a linha direta (grande
#'    circulo) entre partida e chegada como referencia;
#'  - com a rota completa resolvida (route_coords, ver R/route_geometry.R):
#'    mostra tambem os fixos/pontos planejados, para comparar o desenho real
#'    da rota, nao so a distancia.
#'
#' @param radar_track data.frame com colunas lat, lon (e, opcionalmente,
#'   phase, se already processado por detect_flight_phases()/
#'   compute_vertical_deviation() -- usado so para colorir a trajetoria)
#' @param adep_coords vetor c(lat=, lon=) do aerodromo de partida
#' @param ades_coords vetor c(lat=, lon=) do aerodromo de chegada
#' @param route_coords opcional: rota planejada resolvida (colunas point,
#'   lat, lon), para desenhar tambem os fixos do FPL
#' @return objeto ggplot
plot_horizontal_track <- function(radar_track, adep_coords, ades_coords,
                                   route_coords = NULL) {
  direta <- data.frame(
    lat = c(adep_coords["lat"], ades_coords["lat"]),
    lon = c(adep_coords["lon"], ades_coords["lon"])
  )

  p <- ggplot()

  if (!is.null(route_coords)) {
    p <- p +
      geom_path(data = route_coords, aes(x = lon, y = lat, linetype = "Planejado (FPL)"),
                color = "steelblue", linewidth = 0.8) +
      geom_point(data = route_coords, aes(x = lon, y = lat),
                 color = "steelblue", size = 2) +
      geom_text(data = route_coords, aes(x = lon, y = lat, label = point),
                color = "steelblue", size = 3, vjust = -0.8, fontface = "italic")
  } else {
    p <- p +
      geom_path(data = direta, aes(x = lon, y = lat, linetype = "Direto ADEP-ADES"),
                color = "steelblue", linewidth = 0.8)
  }

  if ("phase" %in% names(radar_track)) {
    p <- p +
      geom_path(data = radar_track, aes(x = lon, y = lat, group = 1),
                color = "grey40", linewidth = 0.6) +
      geom_point(data = radar_track, aes(x = lon, y = lat, color = phase), size = 1.2) +
      scale_color_manual(values = c(SUBIDA = "#e07b39", CRUZEIRO = "#2f9e44",
                                    DESCIDA = "#9c36b5"), name = "Fase (real)")
  } else {
    p <- p +
      geom_path(data = radar_track, aes(x = lon, y = lat, color = "Real (RADAR)"),
                linewidth = 0.8) +
      scale_color_manual(values = c("Real (RADAR)" = "#2f9e44"), name = NULL)
  }

  aerodromos <- data.frame(
    lat = c(adep_coords["lat"], ades_coords["lat"]),
    lon = c(adep_coords["lon"], ades_coords["lon"]),
    label = c("ADEP", "ADES")
  )

  p +
    geom_point(data = aerodromos, aes(x = lon, y = lat), shape = 17, size = 3, color = "black") +
    geom_text(data = aerodromos, aes(x = lon, y = lat, label = label),
              size = 3.5, vjust = 2.4, fontface = "bold") +
    scale_linetype_manual(values = c("Planejado (FPL)" = "dashed",
                                      "Direto ADEP-ADES" = "dashed"), name = NULL) +
    scale_x_continuous(expand = expansion(mult = 0.08)) +
    scale_y_continuous(expand = expansion(mult = 0.12)) +
    coord_fixed(ratio = 1 / cos(mean(c(adep_coords["lat"], ades_coords["lat"])) * pi / 180),
                clip = "off") +
    labs(
      title = "Aderencia horizontal ao plano de voo",
      subtitle = "Trajetoria real (RADAR) x plano (FPL)",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.margin = margin(10, 20, 10, 20))
}
