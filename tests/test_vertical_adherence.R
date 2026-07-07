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
source("R/hfe_milestones.R")
source("R/trajectories.R")
source("R/fpl_trajectories.R")
source("R/plot_vertical.R")
source("R/plot_horizontal.R")
source("R/compare_trajectories.R")

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

# --- rota DIRETA (so 2 pontos, ADEP-ADES sem fixos intermediarios) ---------
# caso real encontrado testando SBCF-SBSP: sem TOC/TOD sinteticos, o trecho
# inteiro seria classificado como uma unica fase (bug).
rota_direta <- data.frame(
  seq = 1:2, point = c("AAAA", "BBBB"), level_ft = c(35000, 35000),
  is_level_change = c(FALSE, FALSE),
  lat = c(0, 0), lon = c(0, 5), stringsAsFactors = FALSE
)
rota_direta <- add_cumulative_distance(rota_direta)
perfil_direto <- build_planned_profile(rota_direta, dep_elevation_ft = 2700, dest_elevation_ft = 2600)
stopifnot(
  nrow(perfil_direto) == 4, # TOC e TOD inseridos
  identical(perfil_direto$point, c("AAAA", "TOC", "TOD", "BBBB")),
  all(diff(perfil_direto$dist_nm) > 0) # distancia estritamente crescente apos insercao
)
interp_meio <- interpolate_planned_profile(mean(perfil_direto$dist_nm[2:3]), perfil_direto)
stopifnot(interp_meio$phase == "CRUZEIRO") # meio da rota direta agora tem cruzeiro

# --- projecao rapida (formula fechada) vs. geometria conhecida a mao -------
rota_simples <- data.frame(point = c("A", "B"), lat = c(0, 0), lon = c(0, 1))
rota_simples <- add_cumulative_distance(rota_simples)
radar_simples <- data.frame(
  lat = c(0, 0.01, 0), # sobre a rota, perto da rota, sobre a rota (mas alem do extremo B)
  lon = c(0.5, 0.5, 1.2) # o 3o (lon=1.2) projeta ALEM do extremo B (lon=1)
)
proj_simples <- project_radar_onto_route(radar_simples, rota_simples)
stopifnot(
  proj_simples$cross_track_nm[1] < 0.01, # ponto 1: exatamente sobre a rota
  abs(proj_simples$dist_nm[1] - rota_simples$dist_nm[2] / 2) < 1, # ~ meio da rota
  proj_simples$cross_track_nm[2] > 0.1, # ponto 2: um pouco fora da rota (lat=0.01)
  abs(proj_simples$dist_nm[3] - rota_simples$dist_nm[2]) < 0.1 # ponto 3 "clampado" no extremo B
)

# posicoes de radar com lat/lon NA (existem no dado real) nao podem quebrar
# a projecao -- devem sair como NA, e as demais continuar normais
radar_com_na <- data.frame(lat = c(0, NA, 0.01), lon = c(0.2, 0.5, NA))
proj_com_na <- project_radar_onto_route(radar_com_na, rota_simples)
stopifnot(
  !is.na(proj_com_na$cross_track_nm[1]),
  is.na(proj_com_na$cross_track_nm[2]),
  is.na(proj_com_na$cross_track_nm[3])
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

# --- aderencia horizontal (desvio lateral vs. rota filed) -------------------
stopifnot("cross_track_nm" %in% names(radar), all(radar$cross_track_nm >= 0))
resumo_horizontal <- summarise_horizontal_adherence(radar, tolerance_nm = 5)
stopifnot(
  abs(resumo_horizontal$dist_total_nm - max(radar$dist_nm)) < 1, # ~ extensao da rota
  resumo_horizontal$dist_aderente_nm <= resumo_horizontal$dist_total_nm,
  resumo_horizontal$pct_aderencia >= 0 & resumo_horizontal$pct_aderencia <= 100,
  resumo_horizontal$desvio_max_nm >= resumo_horizontal$desvio_medio_nm
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

# parse_radar_timestamps() (coluna 'ts' pre-calculada, usada em vez de
# reparsear a cada chamada -- ver find_callsign_by_time_location()) deve dar
# exatamente o mesmo resultado que o fallback (parse na hora)
radar_sample_ts <- parse_radar_timestamps(radar_sample)
stopifnot(
  "ts" %in% names(radar_sample_ts),
  identical(sigma_radar_to_track(radar_sample_ts, ssr = 1234), track_real)
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

# --- marcos de distancia (40NM/100NM) e formula HFE (estilo BRA-HFE) -------
adep_milestone <- c(lat = 0, lon = 0)
ades_milestone <- c(lat = 0, lon = 5) # ~300 NM no equador

n_pts <- 60
track_reta <- data.frame(
  timestamp = as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + seq(0, by = 60, length.out = n_pts),
  lat = rep(0, n_pts),
  lon = seq(0, 5, length.out = n_pts)
)
milestones_reta <- extract_milestones(track_reta, adep_milestone, ades_milestone)
stopifnot(
  identical(milestones_reta$milestone,
            c("FIRST_HIT", "40NM_ADEP", "100NM_ADEP", "100NM_ADES", "40NM_ADES", "LAST_HIT")),
  milestones_reta$dist_from_adep_nm[milestones_reta$milestone == "FIRST_HIT"] == 0,
  milestones_reta$dist_to_ades_nm[milestones_reta$milestone == "LAST_HIT"] == 0
)

hfe_reta <- compute_hfe(track_reta, milestones_reta, "100NM_ADEP", "100NM_ADES")
stopifnot(abs(hfe_reta$hfe_pct - 100) < 0.1) # trajetoria reta = 100% de eficiencia

track_desvio <- track_reta
track_desvio$lat <- track_desvio$lat +
  c(rep(0, 10), seq(0, 0.5, length.out = 15), seq(0.5, 0, length.out = 15), rep(0, n_pts - 40))
milestones_desvio <- extract_milestones(track_desvio, adep_milestone, ades_milestone)
hfe_desvio <- compute_hfe(track_desvio, milestones_desvio, "100NM_ADEP", "100NM_ADES")
stopifnot(hfe_desvio$hfe_pct < hfe_reta$hfe_pct) # desvio reduz a eficiencia

# --- segmentacao de radar por callsign + lacuna (estilo BRA-HFE) -----------
radar_log_dia <- data.frame(
  callsign = c(rep("AAA111", 4), rep("BBB222", 3)),
  vl_latitude = c(0, 0.1, 0.2, 0.3, 5, 5.1, 5.2),
  vl_longitude = c(0, 0.1, 0.2, 0.3, 10, 10.1, 10.2),
  nr_flightlevel = c(100, 150, 200, 250, 300, 310, 320),
  dt_radar = c("2025-01-01 00:00:00", "2025-01-01 00:01:00",
               "2025-01-01 00:02:00", "2025-01-01 00:03:00",
               "2025-01-01 01:00:00", "2025-01-01 01:01:00", "2025-01-01 01:02:00"),
  stringsAsFactors = FALSE
)
canonico <- sigma_radar_to_canonical(radar_log_dia)
segmentado <- segment_radar_by_callsign_gap(canonico, max_gap_min = 30)
stopifnot(
  length(unique(segmentado$fid)) == 2, # dois voos distintos (callsign diferente)
  length(unique(segmentado$fid[segmentado$callsign == "AAA111"])) == 1,
  length(unique(segmentado$fid[segmentado$callsign == "BBB222"])) == 1
)

# --- Etapa 1: extracao de trajetorias (clean + segment + detect O/D) -------
airports_traj <- read_airports_db("data/airports_br.csv")
sbcf_t <- lookup_airport_coords("SBCF", airports_traj)
sbsp_t <- lookup_airport_coords("SBSP", airports_traj)

leg_pos <- function(cs, a, b, n, t0, addep = NA, addes = NA) {
  frac <- seq(0, 1, length.out = n)
  data.frame(
    callsign = cs, addep = addep, addes = addes,
    vl_latitude = a["lat"] + frac * (b["lat"] - a["lat"]),
    vl_longitude = a["lon"] + frac * (b["lon"] - a["lon"]),
    nr_flightlevel = 350,
    dt_radar = format(as.POSIXct(t0, tz = "UTC") + seq(0, by = 30, length.out = n),
                      "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}
raw_traj <- rbind(
  # voo 1: O/D preenchidos no proprio radar (fonte primaria)
  leg_pos("GLO111", sbcf_t, sbsp_t, 40, "2025-12-10 12:00:00", addep = "SBCF", addes = "SBSP"),
  # voo 2: O/D vazios no radar -> fallback geometrico (comeca/termina sobre o aerodromo)
  leg_pos("GLO111", sbsp_t, sbcf_t, 40, "2025-12-10 15:00:00", addep = "", addes = "")
)
raw_traj$vl_latitude[5] <- NA # posicao invalida, deve ser descartada

pos_clean <- clean_radar_log(raw_traj)
stopifnot(
  nrow(pos_clean) == nrow(raw_traj) - 1, # 1 posicao NA descartada
  all(c("callsign", "ts", "lat", "lon", "altitude_ft", "addep", "addes") %in% names(pos_clean)),
  is.na(pos_clean$addep[pos_clean$callsign == "GLO111"][40]) # "" virou NA
)
pos_seg <- segment_trajectories(pos_clean, max_gap_min = 30)
stopifnot(length(unique(pos_seg$fid)) == 2) # mesmo callsign, gap de 3h => 2 voos

flights_traj <- resolve_flight_od(pos_seg, airports_traj, fallback_radius_nm = 5)
stopifnot(
  nrow(flights_traj) == 2,
  all(flights_traj$adep_det %in% c("SBCF", "SBSP")),
  all(flights_traj$ades_det %in% c("SBCF", "SBSP")),
  flights_traj$adep_det[1] == flights_traj$ades_det[2] # ida/volta consistentes
)
# voo 1 veio do radar; voo 2 veio do fallback geometrico
src_voo1 <- flights_traj[flights_traj$fid == 1, ]
src_voo2 <- flights_traj[flights_traj$fid == 2, ]
stopifnot(
  src_voo1$adep_src == "radar", src_voo1$ades_src == "radar",
  src_voo2$adep_src == "trajectory", src_voo2$ades_src == "trajectory"
)

# --- QC de qualidade de O/D: dois trechos colados sao descartados -----------
# voo bom (SBCF->SBSP) + voo com dois trechos colados no mesmo fid
# (SBCF->SBSP seguido de SBSP->SBRJ): addes inconsistente E termina longe do
# destino declarado -> od_ok = FALSE
sbrj_q <- lookup_airport_coords("SBRJ", airports_traj)
raw_bom <- leg_pos("QGO1", sbcf_t, sbsp_t, 40, "2025-12-10 12:00:00", addep = "SBCF", addes = "SBSP")
raw_col1 <- leg_pos("QGO2", sbcf_t, sbsp_t, 30, "2025-12-10 16:00:00", addep = "SBCF", addes = "SBSP")
raw_col2 <- leg_pos("QGO2", sbsp_t, sbrj_q, 30, "2025-12-10 16:20:00", addep = "SBSP", addes = "SBRJ")
pos_q <- segment_trajectories(clean_radar_log(rbind(raw_bom, raw_col1, raw_col2)))
fl_q <- flag_flight_od_quality(resolve_flight_od(pos_q, airports_traj), max_endpoint_nm = 30)
voo_bom <- fl_q[adep_det == "SBCF" & ades_det == "SBSP" & ades_n == 1]
voo_colado <- fl_q[ades_n == 2]
stopifnot(
  nrow(voo_bom) == 1, voo_bom$od_ok == TRUE,
  nrow(voo_colado) == 1, voo_colado$od_ok == FALSE, # descartado
  voo_colado$dist_ades_nm > 100 # termina bem longe do destino declarado
)

# --- QC/plot de trajetorias (smoke test: gera os objetos sem erro) ---------
suppressWarnings(suppressMessages(source("R/plot_trajectories.R")))
pos_plot <- data.table::copy(pos_seg)
pos_plot[, adep_det := "SBCF"][, ades_det := "SBSP"]
pos_plot[, adep_src := "radar"][, ades_src := "radar"]
resumo_qc <- qc_summary(pos_plot)
stopifnot(nrow(resumo_qc) == length(unique(pos_seg$fid)))
stopifnot(inherits(plot_trajectories_map(pos_plot, max_flights = 10), "ggplot"))
stopifnot(inherits(plot_one_trajectory(pos_plot, pos_seg$fid[1]), "ggplot"))

# --- trajetoria VERTICAL: distancia acumulada voada + perfil ---------------
pos_vert <- add_cumulative_flown_distance(data.table::copy(pos_plot))
stopifnot(
  "cum_dist_nm" %in% names(pos_vert),
  # cada voo comeca em 0 e a distancia e nao-decrescente dentro do voo
  all(pos_vert[, .(ok = cum_dist_nm[1] == 0 & !is.unsorted(cum_dist_nm)), by = fid]$ok)
)
stopifnot(inherits(plot_flight_vertical(pos_vert, pos_seg$fid[1]), "ggplot"))
stopifnot(inherits(plot_vertical_profiles_pair(pos_vert, "SBCF", "SBSP"), "ggplot"))

# --- Etapa 2: trajetoria PLANEJADA do FPL -----------------------------------
plans_fpl <- select_filed_plan(read_sigma_fpl_log("tests/fixtures/sample_sigma_fpl.csv"))
plans_fpl$adep <- "SBGL"; plans_fpl$ades <- "SBSV"
navdata_fpl <- rbind(
  read.csv("data/waypoints_br.csv", stringsAsFactors = FALSE)[, c("point", "lat", "lon")],
  data.frame(point = airports_traj$icao, lat = airports_traj$latitude, lon = airports_traj$longitude)
)
res_fpl <- build_fpl_routes(plans_fpl, navdata_fpl)
stopifnot(
  nrow(res_fpl$status) == 1, res_fpl$status$resolvido[1] == TRUE,
  res_fpl$routes$point[1] == "SBGL",
  res_fpl$routes$point[nrow(res_fpl$routes)] == "SBSV",
  res_fpl$routes$cum_dist_nm[1] == 0,
  max(res_fpl$routes$cum_dist_nm) > 600, # SBGL-SBSV ~ 660 NM
  !is.unsorted(res_fpl$routes$cum_dist_nm) # distancia acumulada crescente
)

# --- Etapa 3: casamento radar x FPL + comparacao de um voo -----------------
radar_fl_cmp <- data.table::data.table(
  fid = 1L, callsign = "GLO1", adep_det = "SBCF", ades_det = "SBSP",
  t_start = as.POSIXct("2025-12-10 12:00:00", tz = "UTC"))
fpl_fl_cmp <- data.table::data.table(
  gufi = "G1", indicative = "GLO1", adep = "SBCF", ades = "SBSP",
  eobt_full = as.POSIXct("2025-12-10 12:05:00", tz = "UTC"),
  actual_dep = as.POSIXct("2025-12-10 12:02:00", tz = "UTC"), resolvido = TRUE)
pares_cmp <- match_radar_to_fpl(radar_fl_cmp, fpl_fl_cmp, max_dep_diff_min = 60)
stopifnot(nrow(pares_cmp) == 1, pares_cmp$fid == 1, pares_cmp$gufi == "G1")

sbcf_c <- lookup_airport_coords("SBCF", airports_traj)
sbsp_c <- lookup_airport_coords("SBSP", airports_traj)
plans_cmp <- data.frame(gufi = "G1", indicative = "GLO1", adep = "SBCF", ades = "SBSP",
                        lvl = "F370", route = "DCT", stringsAsFactors = FALSE)
planned_cmp <- build_fpl_routes(plans_cmp, navdata_fpl)$routes
n_c <- 60; fr <- seq(0, 1, length.out = n_c)
radar_pos_cmp <- data.table::data.table(
  fid = 1, callsign = "GLO1", ts = as.POSIXct("2025-12-10 12:00:00", tz = "UTC") + seq(0, by = 60, length.out = n_c),
  lat = sbcf_c["lat"] + fr * (sbsp_c["lat"] - sbcf_c["lat"]),
  lon = sbcf_c["lon"] + fr * (sbsp_c["lon"] - sbcf_c["lon"]),
  altitude_ft = c(seq(2700, 37000, length.out = 20), rep(37000, 20), seq(37000, 2600, length.out = 20)))
cmp <- compare_one_flight(radar_pos_cmp, planned_cmp, dep_elev_ft = 2700, dest_elev_ft = 2600)
stopifnot(
  "cross_track_nm" %in% names(cmp$radar),
  cmp$resumo_h$pct_aderencia > 95,           # radar quase em cima da rota
  "GERAL" %in% cmp$resumo_v$fase,
  cmp$resumo_v$pct_aderencia[cmp$resumo_v$fase == "CRUZEIRO"] == 100
)

cat("Todos os testes passaram.\n")
