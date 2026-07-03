# Geometria de rota: resolucao de coordenadas dos pontos do FPL e projecao
# da trajetoria RADAR sobre a rota planejada (distancia ao longo da rota).
#
# Fontes de coordenadas: data/waypoints_br.csv (fixos/aerovias, fonte oficial
# AISWEB/DECEA) para os pontos intermediarios da rota, e data/airports_br.csv
# (ver R/horizontal_efficiency.R) para ADEP/ADES. Combine as duas (rbind) num
# so data.frame point/lat/lon antes de chamar resolve_route_coords() -- ver
# scripts/explore_real_flight.R para um exemplo. O antigo data/waypoints.csv
# (so 5 pontos ficticios) continua servindo para o exemplo sintetico.

library(geosphere)

#' Junta a rota planejada (pontos por nome) com suas coordenadas
#'
#' @param route tibble retornado por parse_fpl_route()
#' @param waypoints_db data.frame com colunas point, lat, lon
#' @return route com colunas lat, lon adicionadas, na ordem original
resolve_route_coords <- function(route, waypoints_db) {
  # all.x=TRUE e essencial aqui: merge() por padrao faz inner join e
  # DESCARTA silenciosamente qualquer ponto sem match na base de navdata, em
  # vez de manter a linha com lat/lon NA -- o que faria a checagem de
  # 'unresolved' abaixo nunca disparar mesmo faltando pontos na rota.
  merged <- merge(route, waypoints_db, by = "point", all.x = TRUE, sort = FALSE)
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
#' a distancia ao longo da rota (NM) correspondente a posicao mais proxima E
#' o desvio lateral (cross-track, NM) em relacao a rota -- a base da
#' ADERENCIA HORIZONTAL: nao "quao direto" foi o voo (isso e eficiencia,
#' ver R/hfe_milestones.R), mas o quanto ele se manteve sobre a rota
#' especifica que foi FILED no FPL.
#'
#' Para cada fix do radar, encontra o segmento da rota mais proximo e
#' interpola a distancia ao longo da rota usando a fracao de projecao naquele
#' segmento (via geosphere::dist2Line); a distancia perpendicular ao
#' segmento mais proximo e o desvio lateral.
#'
#' @param radar_track data.frame com colunas lat, lon
#' @param route_coords rota com colunas lat, lon, dist_nm (ver
#'   add_cumulative_distance())
#' @return radar_track com colunas dist_nm (posicao ao longo da rota) e
#'   cross_track_nm (desvio lateral, sempre >= 0) adicionadas
project_radar_onto_route <- function(radar_track, route_coords) {
  n_seg <- nrow(route_coords) - 1
  seg_start <- as.matrix(route_coords[1:n_seg, c("lon", "lat")])
  seg_end <- as.matrix(route_coords[2:(n_seg + 1), c("lon", "lat")])

  dist_nm <- numeric(nrow(radar_track))
  cross_track_nm <- numeric(nrow(radar_track))

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
    cross_track_nm[i] <- best_dist_m / 1852
  }

  radar_track$dist_nm <- dist_nm
  radar_track$cross_track_nm <- cross_track_nm
  radar_track
}

#' Resume a ADERENCIA HORIZONTAL: o quanto a trajetoria real (RADAR) se
#' manteve sobre a rota especifica filed no FPL (desvio lateral/cross-track),
#' em vez de "quao direto" foi o voo (eficiencia -- ver R/hfe_milestones.R).
#'
#' A tolerancia padrao (5 NM) corresponde a especificacao RNAV-5, usada nas
#' rotas ATS en-route do espaco aereo continental brasileiro (erro total do
#' sistema ate 5 NM, contido 95% do tempo, dentro da especificacao). Para
#' rotas oceanicas/remotas (RNP-10) uma tolerancia mais larga seria mais
#' apropriada.
#'
#' A aderencia e ponderada por DISTANCIA ao longo da rota (NM), nao por
#' numero de pontos do radar: cada segmento entre duas leituras consecutivas
#' (ordenadas por dist_nm) e considerado aderente se a MEDIA do desvio
#' lateral dos dois extremos estiver dentro da tolerancia, e contribui para
#' o total proporcionalmente ao seu comprimento (NM). Isso evita que um
#' trecho com leituras de radar mais frequentes (ou mais esparsas) pese
#' desproporcionalmente na metrica.
#'
#' @param radar_track_projetado resultado de project_radar_onto_route()
#'   (precisa ter as colunas dist_nm e cross_track_nm)
#' @param tolerance_nm desvio lateral (NM) considerado aderente (padrao 5 NM
#'   -- RNAV-5)
#' @return data.frame com dist_total_nm, dist_aderente_nm, pct_aderencia,
#'   desvio_medio_nm, desvio_max_nm
summarise_horizontal_adherence <- function(radar_track_projetado, tolerance_nm = 5) {
  track <- radar_track_projetado[order(radar_track_projetado$dist_nm), ]
  n <- nrow(track)

  if (n < 2) {
    return(data.frame(dist_total_nm = 0, dist_aderente_nm = 0, pct_aderencia = NA,
                       desvio_medio_nm = NA, desvio_max_nm = NA))
  }

  seg_dist_nm <- diff(track$dist_nm)
  seg_dist_nm[seg_dist_nm < 0] <- 0 # protege contra ruido de nao-monotonicidade pontual

  cross_media_segmento <- (track$cross_track_nm[-n] + track$cross_track_nm[-1]) / 2
  seg_aderente <- cross_media_segmento <= tolerance_nm

  dist_total_nm <- sum(seg_dist_nm)
  dist_aderente_nm <- sum(seg_dist_nm[seg_aderente])

  data.frame(
    dist_total_nm = round(dist_total_nm, 1),
    dist_aderente_nm = round(dist_aderente_nm, 1),
    pct_aderencia = round(100 * dist_aderente_nm / dist_total_nm, 1),
    desvio_medio_nm = round(mean(track$cross_track_nm, na.rm = TRUE), 2),
    desvio_max_nm = round(max(track$cross_track_nm, na.rm = TRUE), 2)
  )
}
