# ETAPA 3 -- Comparacao entre a trajetoria VOADA (radar) e a PLANEJADA (FPL).
#
# Casa cada voo do radar com seu plano de voo (por indicativo + O/D + horario)
# e mede a aderencia:
#  - HORIZONTAL: desvio lateral (cross-track) da trajetoria em relacao a rota
#    filed  (via project_radar_onto_route / summarise_horizontal_adherence)
#  - VERTICAL: perfil de altitude voado vs planejado
#    (via build_planned_profile / compute_vertical_deviation)
#
# Reusa a maquinaria ja existente em route_geometry.R, vertical_profile.R,
# vertical_adherence.R. So adiciona o casamento e a orquestracao por voo.

library(data.table)

#' Casa voos do radar com voos do FPL por indicativo + origem/destino +
#' horario de partida mais proximo.
#'
#' @param radar_flights data.table (Etapa 1): fid, callsign, adep_det,
#'   ades_det, t_start
#' @param fpl_flights data.table (Etapa 2): gufi, indicative, adep, ades,
#'   eobt_full, actual_dep, resolvido
#' @param max_dep_diff_min janela (min) entre a partida do radar e a do FPL
#' @return data.table de pares casados: fid, gufi, callsign, adep, ades,
#'   dt_min (diferenca de horario, min)
match_radar_to_fpl <- function(radar_flights, fpl_flights, max_dep_diff_min = 60) {
  r <- as.data.table(radar_flights)[, .(fid, callsign, adep_det, ades_det, t_start)]
  f <- as.data.table(fpl_flights)
  # horario de partida do FPL: real (actual_dep) se houver, senao estimado (eobt)
  f[, dep_time := data.table::fifelse(!is.na(actual_dep), actual_dep, eobt_full)]
  # NAO exige rota resolvida aqui: casar radar<->FPL usa so callsign+O/D+
  # horario; a rota so e necessaria depois, na comparacao. 'resolvido' fica
  # como info (marca se aquele voo casado tem rota disponivel para comparar).
  if (!"resolvido" %in% names(f)) f[, resolvido := NA]
  f <- f[, .(gufi, indicative, adep, ades, dep_time, resolvido)]

  m <- merge(r, f, by.x = c("callsign", "adep_det", "ades_det"),
             by.y = c("indicative", "adep", "ades"), allow.cartesian = TRUE)
  if (nrow(m) == 0) return(data.table(fid = integer(), gufi = character(),
    callsign = character(), adep = character(), ades = character(),
    dt_min = numeric(), resolvido = logical()))

  m[, dt_min := abs(as.numeric(difftime(t_start, dep_time, units = "mins")))]
  m <- m[dt_min <= max_dep_diff_min]
  data.table::setorder(m, fid, dt_min)
  m <- m[, .SD[1], by = fid]  # o FPL de partida mais proxima para cada voo do radar
  m[, .(fid, gufi, callsign, adep = adep_det, ades = ades_det,
        dt_min = round(dt_min, 1), resolvido)]
}

#' Compara UM voo: projeta o radar sobre a rota planejada e calcula desvio
#' lateral (horizontal) e desvio de altitude (vertical).
#'
#' @param radar_positions data.table de posicoes do voo (fid), colunas ts,
#'   lat, lon, altitude_ft
#' @param planned_route data.table dos pontos da rota planejada (gufi),
#'   colunas point, seq, lat, lon, level_ft, cum_dist_nm
#' @param dep_elev_ft,dest_elev_ft elevacao (ft) de origem/destino
#' @return list(radar = radar_track projetado + desvios, route_coords,
#'   resumo_h, resumo_v)
compare_one_flight <- function(radar_positions, planned_route,
                               dep_elev_ft = 0, dest_elev_ft = 0) {
  route_coords <- as.data.frame(planned_route)
  route_coords <- route_coords[order(route_coords$seq), ]
  route_coords$dist_nm <- route_coords$cum_dist_nm  # nome esperado pelo route_geometry

  radar_track <- as.data.frame(radar_positions)
  radar_track <- radar_track[order(radar_track$ts), ]
  names(radar_track)[names(radar_track) == "ts"] <- "timestamp"

  # perfil planejado (pode inserir TOC/TOD se a rota for direta)
  planned_profile <- build_planned_profile(route_coords, dep_elevation_ft = dep_elev_ft,
                                            dest_elevation_ft = dest_elev_ft)
  route_coords <- planned_profile

  radar_track <- project_radar_onto_route(radar_track, route_coords)
  matched_v <- compute_vertical_deviation(radar_track, planned_profile)

  list(
    radar = matched_v,
    route_coords = route_coords,
    resumo_h = summarise_horizontal_adherence(radar_track, tolerance_nm = 5),
    resumo_v = summarise_vertical_adherence(matched_v)
  )
}
