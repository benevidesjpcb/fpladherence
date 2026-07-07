# ETAPA 2 -- Trajetoria PLANEJADA (a partir do FPL).
#
# Espelha a Etapa 1 (radar), mas para o plano de voo: para cada voo arquivado
# (gufi), expande a rota do campo 'route' em pontos, resolve lat/lon de cada
# ponto via navdata (waypoints_br + airports_br) e calcula a distancia
# acumulada ao longo da rota. Saida = "uma linha por ponto da rota" (o
# equivalente planejado das posicoes de radar), com nivel filed por ponto ->
# base do perfil vertical planejado.
#
# Depende de: R/parse_fpl.R (parse_route_token, level_token_to_ft),
# R/parse_sigma_fpl.R (sigma_route_to_route_df), R/route_geometry.R.

library(data.table)
library(geosphere)

#' Constroi as rotas planejadas (pontos resolvidos) de varios voos de uma vez.
#'
#' @param plans data.frame/data.table com uma linha por voo: colunas gufi,
#'   indicative, adep, ades, lvl, route (ver select_filed_plan())
#' @param navdata data.frame point/lat/lon (waypoints_br + airports_br)
#' @return list(routes = data.table de pontos resolvidos com cum_dist_nm;
#'   status = data.table por gufi com n_pontos, n_nao_resolvidos, resolvido)
build_fpl_routes <- function(plans, navdata) {
  plans <- as.data.table(plans)

  # expande cada voo em pontos (seq, point, level_ft) e empilha
  rows <- lapply(seq_len(nrow(plans)), function(i) {
    r <- sigma_route_to_route_df(plans[i])
    data.table(gufi = plans$gufi[i], indicative = plans$indicative[i],
               adep = plans$adep[i], ades = plans$ades[i],
               seq = r$seq, point = r$point, level_ft = r$level_ft)
  })
  pts <- data.table::rbindlist(rows)

  # resolve lat/lon (merge unico) -- all.x mantem pontos nao encontrados (NA)
  nav <- as.data.table(navdata)[, .(point, lat, lon)]
  pts <- merge(pts, nav, by = "point", all.x = TRUE, sort = FALSE)
  data.table::setorder(pts, gufi, seq)

  # status por voo: resolvido = todos os pontos com coordenada
  status <- pts[, .(
    n_pontos = .N,
    n_nao_resolvidos = sum(is.na(lat)),
    pontos_faltando = paste(unique(point[is.na(lat)]), collapse = ",")
  ), by = .(gufi, indicative, adep, ades)]
  status[, resolvido := n_nao_resolvidos == 0]

  # distancia acumulada ao longo da rota, so para voos totalmente resolvidos
  routes <- pts[gufi %in% status[resolvido == TRUE]$gufi]
  routes <- add_cumulative_route_distance(routes)

  list(routes = routes, status = status)
}

#' Distancia acumulada (NM) ao longo da rota planejada, por voo (gufi),
#' ordenando por seq. Mesma logica de add_cumulative_flown_distance(), mas
#' pela sequencia de pontos do FPL em vez do tempo do radar.
#'
#' @param routes data.table de pontos resolvidos (gufi, seq, lat, lon)
#' @return routes com coluna cum_dist_nm
add_cumulative_route_distance <- function(routes) {
  data.table::setorder(routes, gufi, seq)
  n <- nrow(routes)
  if (n == 0) { routes[, cum_dist_nm := numeric(0)]; return(routes) }
  if (n == 1) { routes[, cum_dist_nm := 0]; return(routes) }

  d_nm <- c(0, distHaversine(cbind(routes$lon[-n], routes$lat[-n]),
                             cbind(routes$lon[-1], routes$lat[-1])) / 1852)
  fronteira <- c(TRUE, routes$gufi[-1] != routes$gufi[-n])
  d_nm[fronteira] <- 0
  routes[, .seg_nm := d_nm]
  routes[, cum_dist_nm := cumsum(.seg_nm), by = gufi]
  routes[, .seg_nm := NULL]
  routes
}
