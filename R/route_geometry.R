# Geometria de rota: resolucao de coordenadas dos pontos do FPL e projecao
# da trajetoria RADAR sobre a rota planejada (distancia ao longo da rota).
#
# Em producao, a resolucao de coordenadas dos pontos (fixos/waypoints) viria
# de uma base de dados de navegacao (AIRAC/navdata). Aqui usamos uma tabela
# local (data/waypoints.csv) como stand-in para essa base.

library(geosphere)

#' Junta a rota planejada (pontos por nome) com suas coordenadas
#'
#' @param route tibble retornado por parse_fpl_route()
#' @param waypoints_db data.frame com colunas point, lat, lon
#' @return route com colunas lat, lon adicionadas, na ordem original
resolve_route_coords <- function(route, waypoints_db) {
  merged <- merge(route, waypoints_db, by = "point", sort = FALSE)
  merged <- merged[order(merged$seq), ]
  unresolved <- merged[is.na(merged$lat), "point"]
  if (length(unresolved) > 0) {
    stop("Pontos sem coordenadas na base de navdata: ",
         paste(unresolved, collapse = ", "))
  }
  rownames(merged) <- NULL
  merged
}

#' Calcula a distancia acumulada (NM) ao longo da rota planejada
#'
#' @param route_coords rota com colunas lat, lon (ordenada por seq)
#' @return route_coords com coluna dist_nm (distancia acumulada desde DEP)
add_cumulative_distance <- function(route_coords) {
  n <- nrow(route_coords)
  leg_dist_m <- c(0, distHaversine(
    as.matrix(route_coords[-n, c("lon", "lat")]),
    as.matrix(route_coords[-1, c("lon", "lat")])
  ))
  route_coords$dist_nm <- cumsum(leg_dist_m) / 1852
  route_coords
}

#' Projeta cada ponto do radar sobre a polilinha da rota planejada, retornando
#' a distancia ao longo da rota (NM) correspondente a posicao mais proxima.
#'
#' Para cada fix do radar, encontra o segmento da rota mais proximo e
#' interpola a distancia ao longo da rota usando a fracao de projecao naquele
#' segmento (via geosphere::dist2Line).
#'
#' @param radar_track data.frame com colunas lat, lon
#' @param route_coords rota com colunas lat, lon, dist_nm (ver
#'   add_cumulative_distance())
#' @return radar_track com coluna dist_nm adicionada
project_radar_onto_route <- function(radar_track, route_coords) {
  n_seg <- nrow(route_coords) - 1
  seg_start <- as.matrix(route_coords[1:n_seg, c("lon", "lat")])
  seg_end <- as.matrix(route_coords[2:(n_seg + 1), c("lon", "lat")])

  dist_nm <- numeric(nrow(radar_track))

  for (i in seq_len(nrow(radar_track))) {
    pt <- c(radar_track$lon[i], radar_track$lat[i])
    best_dist_m <- Inf
    best_dist_nm <- NA_real_
    for (s in seq_len(n_seg)) {
      proj <- dist2Line(p = matrix(pt, ncol = 2),
                         line = rbind(seg_start[s, ], seg_end[s, ]))
      perp_dist_m <- proj[1, "distance"]
      if (perp_dist_m < best_dist_m) {
        best_dist_m <- perp_dist_m
        seg_len_nm <- route_coords$dist_nm[s + 1] - route_coords$dist_nm[s]
        along_seg_m <- distHaversine(seg_start[s, ], proj[1, c("lon", "lat")])
        seg_len_m <- distHaversine(seg_start[s, ], seg_end[s, ])
        frac <- if (seg_len_m > 0) min(max(along_seg_m / seg_len_m, 0), 1) else 0
        best_dist_nm <- route_coords$dist_nm[s] + frac * seg_len_nm
      }
    }
    dist_nm[i] <- best_dist_nm
  }

  radar_track$dist_nm <- dist_nm
  radar_track
}
