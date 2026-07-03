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
source("R/horizontal_efficiency.R")

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

# --- casamento FPL x RADAR por horario + localizacao (nao por ssr) ---------
airports_db_test <- read_airports_db("data/airports_br.csv")
adep_teste <- lookup_airport_coords("SBGL", airports_db_test)
ades_teste <- lookup_airport_coords("SBSV", airports_db_test)
dep_time_teste <- as.POSIXct("2025-12-10 00:13:00", tz = "UTC")
arr_time_teste <- as.POSIXct("2025-12-10 01:52:00", tz = "UTC")

radar_log_teste <- data.frame(
  callsign = c("ABC123", "ABC123", "ABC123", "ABC123", "XYZ999", "XYZ999"),
  vl_latitude = c(adep_teste["lat"] + 0.05, -18, -14, ades_teste["lat"] - 0.05, -5, -5.1),
  vl_longitude = c(adep_teste["lon"] + 0.05, -41, -39, ades_teste["lon"] - 0.05, -60, -60.1),
  dt_radar = c("2025-12-10 00:15:00.000", "2025-12-10 00:45:00.000",
               "2025-12-10 01:20:00.000", "2025-12-10 01:50:00.000",
               "2025-12-10 00:20:00.000", "2025-12-10 00:25:00.000"),
  stringsAsFactors = FALSE
)
match_callsign <- find_callsign_by_time_location(radar_log_teste, adep_teste, ades_teste,
                                                  dep_time_teste, arr_time_teste)
stopifnot(
  identical(match_callsign, "ABC123"), # so o voo real deve bater, nao o ruido XYZ999
  length(find_callsign_by_time_location(radar_log_teste, adep_teste, ades_teste,
                                         dep_time_teste - 3600 * 5, arr_time_teste)) == 0
)

# --- eficiencia/aderencia horizontal -----------------------------------------
airports_db <- read_airports_db("data/airports_br.csv")
adep_coords <- lookup_airport_coords("SBGR", airports_db)
ades_coords <- lookup_airport_coords("SBRJ", airports_db)
stopifnot(
  !is.null(adep_coords), !is.null(ades_coords),
  is.null(lookup_airport_coords("XXXX", airports_db))
)

eficiencia <- compute_horizontal_efficiency(radar, adep_coords, ades_coords)
stopifnot(
  eficiencia$distancia_direta_nm > 0,
  eficiencia$distancia_voada_nm >= eficiencia$distancia_direta_nm, # radar nao e mais direto que a reta
  eficiencia$eficiencia_pct > 0 & eficiencia$eficiencia_pct <= 100
)

# --- base oficial de waypoints (AISWEB/DECEA) e resolucao de rota real ------
waypoints_br <- read.csv("data/waypoints_br.csv", stringsAsFactors = FALSE)
stopifnot(
  nrow(waypoints_br) > 7000,
  all(c("IMBAP", "SIRAP", "OSAGU", "ZUMBA") %in% waypoints_br$point),
  !any(duplicated(waypoints_br$point))
)

# resolve_route_coords() deve dar ERRO (nao descartar silenciosamente) quando
# um ponto da rota nao existe na base de navdata -- ver correcao do merge()
# (all.x=TRUE) em R/route_geometry.R
rota_com_ponto_invalido <- data.frame(
  seq = 1:2, point = c("IMBAP", "PONTO_QUE_NAO_EXISTE"),
  level_ft = c(37000, 37000), is_level_change = c(FALSE, FALSE)
)
erro_capturado <- tryCatch({
  resolve_route_coords(rota_com_ponto_invalido, waypoints_br)
  FALSE
}, error = function(e) TRUE)
stopifnot(erro_capturado)

# rota real completa (SBGL -> IMBAP -> SIRAP -> OSAGU -> ZUMBA -> SBSV)
navdata <- rbind(
  waypoints_br[, c("point", "lat", "lon")],
  data.frame(point = airports_db$icao, lat = airports_db$latitude,
             lon = airports_db$longitude)
)
route_real_completa <- sigma_route_to_route_df(plano[1, ])
route_coords_real <- add_cumulative_distance(resolve_route_coords(route_real_completa, navdata))
stopifnot(
  nrow(route_coords_real) == 6,
  route_coords_real$point[1] == "SBGL",
  route_coords_real$point[nrow(route_coords_real)] == "SBSV",
  route_coords_real$dist_nm[nrow(route_coords_real)] > 600 # SBGL-SBSV ~ 660 NM
)

cat("Todos os testes passaram.\n")
