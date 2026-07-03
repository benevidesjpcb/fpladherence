# Testes de fumaca (smoke tests) do pipeline de aderencia vertical.
# Rodar a partir da raiz do projeto: Rscript tests/test_vertical_adherence.R

source("R/parse_fpl.R")
source("R/parse_sigma_fpl.R")
source("R/parse_radar.R")
source("R/parse_sigma_radar.R")
source("R/route_geometry.R")
source("R/vertical_profile.R")
source("R/vertical_adherence.R")
source("R/vertical_adherence_radar_only.R")

# --- parser de FPL (texto ICAO cru) ---------------------------------------
fpl <- parse_fpl(read_fpl_file("data/sample_fpl.txt"))
stopifnot(
  fpl$dep_icao == "SBGR",
  fpl$dest_icao == "SBRJ",
  nrow(fpl$route) == 5,
  fpl$route$point[nrow(fpl$route)] == "SBRJ",
  fpl$route$level_ft[1] == 23000,
  fpl$route$level_ft[nrow(fpl$route)] == 25000
)

# --- geometria de rota ------------------------------------------------------
waypoints_db <- read.csv("data/waypoints.csv")
route_coords <- resolve_route_coords(fpl$route, waypoints_db)
route_coords <- add_cumulative_distance(route_coords)
stopifnot(
  nrow(route_coords) == 5,
  route_coords$dist_nm[1] == 0,
  diff(route_coords$dist_nm) > 0 # distancia estritamente crescente
)

# --- perfil planejado --------------------------------------------------------
planned_profile <- build_planned_profile(route_coords, dep_elevation_ft = 2459,
                                          dest_elevation_ft = 11)
interp_start <- interpolate_planned_profile(0, planned_profile)
interp_end <- interpolate_planned_profile(max(route_coords$dist_nm), planned_profile)
stopifnot(
  interp_start$planned_alt_ft == 2459,
  interp_start$phase == "SUBIDA",
  interp_end$planned_alt_ft == 11,
  interp_end$phase == "DESCIDA"
)

# --- pipeline completo (dados sinteticos) -----------------------------------
radar <- read_radar_track("data/sample_radar.csv")
stopifnot(
  all(diff(as.numeric(radar$timestamp)) > 0) # timestamp ISO8601 parseado corretamente
)
radar <- project_radar_onto_route(radar, route_coords)
matched <- compute_vertical_deviation(radar, planned_profile)
stopifnot(
  all(c("planned_alt_ft", "phase", "deviation_ft", "is_adherent") %in% names(matched)),
  all(matched$phase %in% c("SUBIDA", "CRUZEIRO", "DESCIDA")),
  nrow(matched) == nrow(radar)
)

summary_tbl <- summarise_vertical_adherence(matched)
stopifnot(
  "GERAL" %in% summary_tbl$fase,
  all(summary_tbl$pct_aderencia >= 0 & summary_tbl$pct_aderencia <= 100)
)

# --- pipeline sem navdata (deteccao de fase pela taxa de subida/descida) ----
radar_rate <- detect_flight_phases(read_radar_track("data/sample_radar.csv"))
radar_rate <- compute_vertical_deviation_radar_only(radar_rate, filed_level_ft = fpl$route$level_ft[1])
stopifnot(
  all(radar_rate$phase %in% c("SUBIDA", "CRUZEIRO", "DESCIDA")),
  all(is.na(radar_rate$is_adherent[radar_rate$phase != "CRUZEIRO"])),
  all(!is.na(radar_rate$is_adherent[radar_rate$phase == "CRUZEIRO"]))
)
resumo_radar_only <- summarise_vertical_adherence_radar_only(radar_rate)
stopifnot(resumo_radar_only$cruzeiro$pct_aderencia >= 0)

# --- adaptador SIGMA (FPL real) ---------------------------------------------
sigma_sample <- read_sigma_fpl_log("tests/fixtures/sample_sigma_fpl.csv")
plano <- select_filed_plan(sigma_sample)
horarios <- extract_actual_times(sigma_sample)
stopifnot(
  nrow(plano) == 1,
  plano$indicative == "FIC2036",
  plano$adep == "SBGL",
  plano$ades == "SBSV",
  plano$lvl == "F370",
  format(horarios$actual_dep, "%H:%M") == "00:13",
  format(horarios$actual_arr, "%H:%M") == "01:52"
)

route_real <- sigma_route_to_route_df(plano[1, ])
stopifnot(
  route_real$point[1] == "SBGL",
  route_real$point[nrow(route_real)] == "SBSV",
  all(route_real$level_ft == 37000),
  !"UZ42" %in% route_real$point # aerovia deve ser filtrada
)

# --- adaptador SIGMA (RADAR real) -------------------------------------------
radar_sample <- read_sigma_radar_log("tests/fixtures/sample_sigma_radar.csv")
track_real <- sigma_radar_to_track(radar_sample, ssr = 1234)
stopifnot(
  nrow(track_real) == 5,
  all(diff(track_real$timestamp) >= 0), # ordenado por tempo
  track_real$altitude_ft[1] == 9900
)

cat("Todos os testes passaram.\n")
