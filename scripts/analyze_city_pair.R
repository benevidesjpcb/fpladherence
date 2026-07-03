# Analise focada de UM par de cidades (ADEP/ADES), usando o pipeline
# COMPLETO (com navdata real): rota resolvida via AISWEB (waypoints_br.csv +
# airports_br.csv), perfil vertical planejado (build_planned_profile) e
# desvio lateral (cross-track) em relacao a rota filed.
#
# Casamento FPL <-> RADAR: por horario (decolagem/pouso confirmados no FPL)
# + proximidade geografica do ADEP/ADES -- ver find_callsign_by_time_location()
# em R/parse_sigma_radar.R.
#
# Como usar: ajuste PAR_ADEP/PAR_ADES abaixo e rode o script inteiro.

setwd(".") # garanta que esta na raiz do projeto

source("R/parse_fpl.R")
source("R/parse_sigma_fpl.R")
source("R/parse_radar.R")
source("R/parse_sigma_radar.R")
source("R/route_geometry.R")
source("R/vertical_profile.R")
source("R/vertical_adherence.R")
source("R/plot_vertical.R")
source("R/horizontal_efficiency.R")
source("R/plot_horizontal.R")

PAR_ADEP <- "SBCF" # Confins (Belo Horizonte)
PAR_ADES <- "SBSP" # Congonhas (Sao Paulo)

fpl_path <- "data/local/sigma_flight_plan_2025_12_10.csv"
radar_path <- "data/local/radar_2025_12_10.csv"

## 1. FPL: so voos do par de cidades (nos dois sentidos) -----------------------
log_fpl <- read_sigma_fpl_log(fpl_path)
planos <- select_filed_plan(log_fpl)
horarios <- extract_actual_times(log_fpl)
planos <- merge(planos, horarios, by = "gufi", all.x = TRUE)

par_ida <- planos$adep == PAR_ADEP & planos$ades == PAR_ADES
par_volta <- planos$adep == PAR_ADES & planos$ades == PAR_ADEP
planos_par <- planos[(par_ida | par_volta) &
                        !is.na(planos$actual_dep) & !is.na(planos$actual_arr), , drop = FALSE]

cat("Voos", PAR_ADEP, "<->", PAR_ADES, "no dia, com decolagem/pouso confirmados:",
    nrow(planos_par), "\n")
print(planos_par[, c("gufi", "indicative", "adep", "ades", "lvl", "actual_dep", "actual_arr")])

if (nrow(planos_par) == 0) {
  stop("Nenhum voo ", PAR_ADEP, " <-> ", PAR_ADES, " com decolagem/pouso confirmados neste dia.")
}

## 2. Navdata: aerodromos + waypoints oficiais (AISWEB) -------------------------
airports_db <- read_airports_db("data/airports_br.csv")
waypoints_br <- read.csv("data/waypoints_br.csv", stringsAsFactors = FALSE)
navdata <- rbind(
  waypoints_br[, c("point", "lat", "lon")],
  data.frame(point = airports_db$icao, lat = airports_db$latitude, lon = airports_db$longitude)
)

## 3. Radar: le uma vez so ------------------------------------------------------
log_radar <- read_sigma_radar_log(
  radar_path,
  select = c("callsign", "vl_latitude", "vl_longitude", "nr_flightlevel", "nr_speed", "dt_radar")
)
cat("\nTotal de posicoes no radar:", nrow(log_radar), "\n")

# parse do timestamp UMA VEZ SO (caro para milhoes de linhas) -- reaproveitado
# por find_callsign_by_time_location()/sigma_radar_to_track() dentro do loop
log_radar <- parse_radar_timestamps(log_radar)
cat("Timestamps do radar parseados.\n")

## 4. Para cada voo do par: casa com o radar, resolve a rota, calcula aderencia -
buffer_min <- 15
resultados <- list()

for (i in seq_len(nrow(planos_par))) {
  voo <- planos_par[i, ]
  adep_coords <- lookup_airport_coords(voo$adep, airports_db)
  ades_coords <- lookup_airport_coords(voo$ades, airports_db)

  candidatos <- find_callsign_by_time_location(log_radar, adep_coords, ades_coords,
                                                voo$actual_dep, voo$actual_arr)
  if (length(candidatos) == 0) {
    cat("\n[", voo$indicative, "] nenhum callsign de radar casado perto do ADEP/ADES ",
        "nos horarios certos -- pulando.\n")
    next
  }

  janela <- c(voo$actual_dep - buffer_min * 60, voo$actual_arr + buffer_min * 60)
  radar_track <- sigma_radar_to_track(log_radar, callsign = candidatos[1], time_window = janela)
  if (nrow(radar_track) < 10) {
    cat("\n[", voo$indicative, "] poucas posicoes de radar (", nrow(radar_track),
        ") -- pulando.\n")
    next
  }
  radar_track <- keep_largest_contiguous_segment(radar_track, max_gap_min = 5)

  route <- sigma_route_to_route_df(voo)
  route_coords <- tryCatch(
    add_cumulative_distance(resolve_route_coords(route, navdata)),
    error = function(e) {
      cat("\n[", voo$indicative, "] rota nao resolvida:", conditionMessage(e), "-- pulando.\n")
      NULL
    }
  )
  if (is.null(route_coords)) next

  dep_elev <- lookup_airport_elevation_ft(voo$adep, airports_db)
  dest_elev <- lookup_airport_elevation_ft(voo$ades, airports_db)
  # build_planned_profile() pode inserir pontos sinteticos de topo de
  # subida/descida (TOC/TOD) quando a rota e direta (so ADEP/ADES, sem
  # fixos intermediarios) -- usa-se a MESMA versao (retornada por ela) para
  # projetar o radar, ja que e a mesma linha geometrica, so com mais pontos
  # de referencia.
  planned_profile <- build_planned_profile(route_coords, dep_elevation_ft = dep_elev,
                                            dest_elevation_ft = dest_elev)
  route_coords <- planned_profile

  radar_track <- project_radar_onto_route(radar_track, route_coords)
  matched_vertical <- compute_vertical_deviation(radar_track, planned_profile)
  resumo_vertical <- summarise_vertical_adherence(matched_vertical)
  resumo_horizontal <- summarise_horizontal_adherence(radar_track, tolerance_nm = 5)

  cat("\n=== [", voo$indicative, voo$adep, "->", voo$ades, "] callsign radar:",
      candidatos[1], "| ", nrow(radar_track), "posicoes ===\n")
  cat("-- Aderencia vertical --\n"); print(resumo_vertical)
  cat("-- Aderencia horizontal (tolerancia 5 NM / RNAV-5) --\n"); print(resumo_horizontal)

  resultados[[voo$indicative]] <- list(
    voo = voo, radar_track = matched_vertical, route_coords = route_coords,
    resumo_vertical = resumo_vertical, resumo_horizontal = resumo_horizontal
  )
}

## 5. Graficos do primeiro voo casado com sucesso -------------------------------
if (length(resultados) > 0) {
  r <- resultados[[1]]
  print(plot_vertical_adherence(r$radar_track))
  print(plot_horizontal_track(r$radar_track, lookup_airport_coords(r$voo$adep, airports_db),
                               lookup_airport_coords(r$voo$ades, airports_db),
                               route_coords = r$route_coords))
} else {
  cat("\nNenhum voo do par foi casado com sucesso com uma trajetoria de radar.\n")
}
