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

#' Raio da Terra (m) -- mesmo valor usado por geosphere::distHaversine()
#' (raio equatorial WGS84), para as duas formulas ficarem consistentes.
EARTH_RADIUS_M <- 6378137

#' Distancia angular (radianos, formula de haversine) entre dois pontos
.ang_dist_rad <- function(lat1, lon1, lat2, lon2) {
  lat1r <- lat1 * pi / 180; lat2r <- lat2 * pi / 180
  dlat <- (lat2 - lat1) * pi / 180; dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 + cos(lat1r) * cos(lat2r) * sin(dlon / 2)^2
  2 * atan2(sqrt(a), sqrt(pmax(1 - a, 0)))
}

#' Rumo inicial (radianos) de P1 para P2
.bearing_rad <- function(lat1, lon1, lat2, lon2) {
  lat1r <- lat1 * pi / 180; lat2r <- lat2 * pi / 180; dlonr <- (lon2 - lon1) * pi / 180
  y <- sin(dlonr) * cos(lat2r)
  x <- cos(lat1r) * sin(lat2r) - sin(lat1r) * cos(lat2r) * cos(dlonr)
  atan2(y, x)
}

#' Projeta um vetor de pontos (lat_p, lon_p) sobre o SEGMENTO A-B (grande
#' circulo), usando a formula fechada de distancia cross-track/along-track
#' (trigonometria direta -- sem iteracao), bem mais rapida que
#' geosphere::dist2Line() para muitos pontos (~100-900x nos testes com
#' centenas de posicoes de radar). Resultado numericamente equivalente ao
#' dist2Line() a menos de ruido de arredondamento (dif. maxima ~50 m nos
#' testes), exceto perto de vertices da rota, onde a atribuicao "qual
#' segmento e o mais proximo" pode ser ambigua para ambos os metodos.
#'
#' @param lat_p,lon_p vetores de latitude/longitude dos pontos a projetar
#' @param lat_a,lon_a,lat_b,lon_b coordenadas dos extremos do segmento
#' @return data.frame com cross_track_m (>=0), along_track_m (0..seg_len_m,
#'   "clampado" aos extremos do segmento) e seg_len_m
project_point_onto_segment <- function(lat_p, lon_p, lat_a, lon_a, lat_b, lon_b) {
  seg_len_m <- distHaversine(c(lon_a, lat_a), c(lon_b, lat_b))

  d13 <- .ang_dist_rad(lat_a, lon_a, lat_p, lon_p)
  brg13 <- .bearing_rad(lat_a, lon_a, lat_p, lon_p)
  brg12 <- .bearing_rad(lat_a, lon_a, lat_b, lon_b)

  dxt <- asin(pmin(pmax(sin(d13) * sin(brg13 - brg12), -1), 1))
  cos_dat <- pmin(pmax(cos(d13) / cos(dxt), -1), 1)
  dat_m <- acos(cos_dat) * EARTH_RADIUS_M

  sinal <- ifelse(cos(brg13 - brg12) >= 0, 1, -1)
  along_signed_m <- sinal * dat_m
  along_clamped_m <- pmin(pmax(along_signed_m, 0), seg_len_m)

  cross_m <- abs(dxt) * EARTH_RADIUS_M
  # fora do segmento (projecao antes de A ou depois de B): a distancia
  # correta e ate o extremo mais proximo, nao a formula de cross-track
  # (que assume projecao sobre a linha infinita)
  fora_inicio <- along_signed_m < 0
  fora_fim <- along_signed_m > seg_len_m
  if (any(fora_inicio)) {
    cross_m[fora_inicio] <- distHaversine(cbind(lon_p[fora_inicio], lat_p[fora_inicio]),
                                            c(lon_a, lat_a))
  }
  if (any(fora_fim)) {
    cross_m[fora_fim] <- distHaversine(cbind(lon_p[fora_fim], lat_p[fora_fim]),
                                         c(lon_b, lat_b))
  }

  data.frame(cross_track_m = cross_m, along_track_m = along_clamped_m, seg_len_m = seg_len_m)
}

#' Projeta cada ponto do radar sobre a polilinha da rota planejada, retornando
#' a distancia ao longo da rota (NM) correspondente a posicao mais proxima E
#' o desvio lateral (cross-track, NM) em relacao a rota -- a base da
#' ADERENCIA HORIZONTAL: nao "quao direto" foi o voo (isso e eficiencia,
#' ver R/hfe_milestones.R), mas o quanto ele se manteve sobre a rota
#' especifica que foi FILED no FPL.
#'
#' Para cada segmento da rota, projeta TODOS os pontos do radar de uma vez
#' (vetorizado, ver project_point_onto_segment()) e mantem o menor desvio
#' lateral entre os segmentos -- um loop pequeno (por segmento da rota, tipicamente
#' poucas dezenas) em vez de um loop grande (por ponto do radar, que pode ser
#' centenas/milhares) chamando geosphere::dist2Line() individualmente, que e
#' proibitivamente lento nessa escala.
#'
#' @param radar_track data.frame com colunas lat, lon
#' @param route_coords rota com colunas lat, lon, dist_nm (ver
#'   add_cumulative_distance())
#' @return radar_track com colunas dist_nm (posicao ao longo da rota) e
#'   cross_track_nm (desvio lateral, sempre >= 0) adicionadas
project_radar_onto_route <- function(radar_track, route_coords) {
  n_seg <- nrow(route_coords) - 1
  n_radar <- nrow(radar_track)

  best_dist_m <- rep(Inf, n_radar)
  best_dist_nm <- rep(NA_real_, n_radar)

  for (s in seq_len(n_seg)) {
    a <- route_coords[s, ]
    b <- route_coords[s + 1, ]
    proj <- project_point_onto_segment(radar_track$lat, radar_track$lon,
                                        a$lat, a$lon, b$lat, b$lon)

    seg_len_nm <- b$dist_nm - a$dist_nm
    frac <- ifelse(proj$seg_len_m > 0, proj$along_track_m / proj$seg_len_m, 0)
    seg_dist_nm <- a$dist_nm + frac * seg_len_nm

    melhor <- proj$cross_track_m < best_dist_m
    best_dist_m[melhor] <- proj$cross_track_m[melhor]
    best_dist_nm[melhor] <- seg_dist_nm[melhor]
  }

  radar_track$dist_nm <- best_dist_nm
  radar_track$cross_track_nm <- best_dist_m / 1852
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
