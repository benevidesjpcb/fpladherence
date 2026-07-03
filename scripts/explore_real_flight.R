# Script exploratorio para testar o pipeline de aderencia vertical e
# horizontal (sem navdata completa) com um voo real do dia 2025-12-10.
#
# Casamento FPL <-> RADAR: por HORARIO (do FPL) + PROXIMIDADE GEOGRAFICA do
# aerodromo de partida/chegada -- NAO por codigo de transponder (ssr), que
# tem muitos valores faltantes nos dados reais. Ver
# find_callsign_by_time_location() em R/parse_sigma_radar.R.
#
# Como usar (rode no R, de dentro da pasta raiz do projeto):
#   Rode o script inteiro. Ele le o radar UMA VEZ (pode levar 1-2 min) e
#   tenta automaticamente, voo a voo, achar o callsign do radar cuja
#   trajetoria passa perto do ADEP na hora da decolagem e perto do ADES na
#   hora do pouso (ambas confirmadas pelas mensagens DEP/ARR do FPL).
#
# Pacotes necessarios: install.packages(c("ggplot2", "geosphere", "data.table"))

setwd(".") # garanta que esta na raiz do projeto (onde ficam as pastas R/, data/)

source("R/parse_fpl.R")
source("R/parse_sigma_fpl.R")
source("R/parse_radar.R")
source("R/parse_sigma_radar.R")
source("R/route_geometry.R")
source("R/vertical_adherence_radar_only.R")
source("R/plot_vertical.R")
source("R/horizontal_efficiency.R")
source("R/plot_horizontal.R")

fpl_path <- "data/local/sigma_flight_plan_2025_12_10.csv"
radar_path <- "data/local/radar_2025_12_10.csv"

## 1. Le os dois arquivos (uma vez so) -----------------------------------------
log_fpl <- read_sigma_fpl_log(fpl_path)
planos <- select_filed_plan(log_fpl)
horarios <- extract_actual_times(log_fpl)
planos <- merge(planos, horarios, by = "gufi", all.x = TRUE)
cat("Total de voos no FPL:", nrow(planos), "\n")

airports_db <- read_airports_db("data/airports_br.csv")

log_radar <- read_sigma_radar_log(
  radar_path,
  select = c("callsign", "vl_latitude", "vl_longitude", "nr_flightlevel",
             "nr_speed", "dt_radar")
)
cat("Total de posicoes no radar:", nrow(log_radar), "\n")

# parse do timestamp UMA VEZ SO (caro para milhoes de linhas) -- reaproveitado
# por find_callsign_by_time_location()/sigma_radar_to_track() dentro do loop
log_radar <- parse_radar_timestamps(log_radar)
cat("Timestamps do radar parseados.\n")

## 2. So voos com ADEP/ADES conhecidos e decolagem/pouso confirmados --------
planos$adep_coords_ok <- !is.na(match(trimws(planos$adep), trimws(airports_db$icao)))
planos$ades_coords_ok <- !is.na(match(trimws(planos$ades), trimws(airports_db$icao)))

planos_testaveis <- planos[planos$adep_coords_ok & planos$ades_coords_ok &
                              !is.na(planos$actual_dep) & !is.na(planos$actual_arr), , drop = FALSE]
cat("\nVoos com ADEP/ADES em data/airports_br.csv e decolagem+pouso confirmados:",
    nrow(planos_testaveis), "de", nrow(planos), "\n")

if (nrow(planos_testaveis) == 0) {
  stop("Nenhum voo tem ADEP/ADES conhecidos + decolagem/pouso confirmados. ",
       "Investigue manualmente 'planos'.")
}

## 3. Acha automaticamente o primeiro voo casavel por horario+localizacao ----
buffer_min <- 15 # margem antes da decolagem / depois do pouso

voo <- NULL
radar_track <- NULL
adep_coords <- NULL
ades_coords <- NULL

for (i in seq_len(min(nrow(planos_testaveis), 50))) {
  candidato <- planos_testaveis[i, ]
  adep_c <- lookup_airport_coords(candidato$adep, airports_db)
  ades_c <- lookup_airport_coords(candidato$ades, airports_db)

  candidatos_callsign <- find_callsign_by_time_location(
    log_radar, adep_c, ades_c, candidato$actual_dep, candidato$actual_arr
  )
  if (length(candidatos_callsign) == 0) next

  janela <- c(candidato$actual_dep - buffer_min * 60,
              candidato$actual_arr + buffer_min * 60)
  tentativa <- sigma_radar_to_track(log_radar, callsign = candidatos_callsign[1],
                                     time_window = janela)
  if (nrow(tentativa) >= 10) {
    voo <- candidato
    radar_track <- tentativa
    adep_coords <- adep_c
    ades_coords <- ades_c
    cat("\nVoo escolhido:", candidato$indicative, candidato$adep, "->", candidato$ades,
        "| callsign radar:", candidatos_callsign[1],
        "|", nrow(tentativa), "posicoes entre", format(janela[1]), "e", format(janela[2]), "\n")
    print(voo)
    break
  }
}

if (is.null(voo)) {
  stop("Nenhum dos primeiros 50 voos testaveis foi casado com uma trajetoria ",
       "de radar (>= 10 posicoes) perto do ADEP/ADES nos horarios certos. ",
       "Aumente o limite ('min(nrow(planos_testaveis), 50)'), 'max_dist_nm'/",
       "'max_time_min' de find_callsign_by_time_location(), ou investigue ",
       "manualmente.")
}

## 4. Reforco: mantem so o maior trecho continuo -------------------------------
n_antes <- nrow(radar_track)
radar_track <- keep_largest_contiguous_segment(radar_track, max_gap_min = 5)
if (nrow(radar_track) < n_antes) {
  cat("Removidas", n_antes - nrow(radar_track),
      "posicoes fora do maior trecho continuo (provavel outro voo/aeronave).\n")
}

## 5. Aderencia vertical (deteccao de fase pela taxa de subida/descida) --------
filed_level_ft <- level_token_to_ft(voo$lvl)
cat("\nNivel filed:", voo$lvl, "=", filed_level_ft, "ft\n")

radar_track <- detect_flight_phases(radar_track, filed_level_ft = filed_level_ft)
radar_track <- compute_vertical_deviation_radar_only(radar_track, filed_level_ft)

cat("\nResumo por fase:\n")
print(summarise_vertical_adherence_radar_only(radar_track))

## 6. Grafico vertical ------------------------------------------------------------
p_vertical <- plot_vertical_adherence_radar_only(radar_track, filed_level_ft)
print(p_vertical)
# ggplot2::ggsave("data/local/aderencia_vertical_exemplo.png", p_vertical, width = 9, height = 5.5)

## 7. Aderencia/visualizacao horizontal (lateral) ---------------------------------
eficiencia <- compute_horizontal_efficiency(radar_track, adep_coords, ades_coords)
cat("\nEficiencia horizontal:\n")
print(eficiencia)

# tenta resolver a rota completa (fixos citados no FPL) com a base oficial
# AISWEB (data/waypoints_br.csv) + aerodromos (data/airports_br.csv); se
# algum ponto nao for encontrado, cai para o grafico so com ADEP/ADES
waypoints_br <- read.csv("data/waypoints_br.csv", stringsAsFactors = FALSE)
navdata <- rbind(
  waypoints_br[, c("point", "lat", "lon")],
  data.frame(point = airports_db$icao, lat = airports_db$latitude,
             lon = airports_db$longitude)
)

route_real <- sigma_route_to_route_df(voo)
route_coords_real <- tryCatch(
  add_cumulative_distance(resolve_route_coords(route_real, navdata)),
  error = function(e) {
    cat("\nNao foi possivel resolver a rota completa:", conditionMessage(e), "\n")
    NULL
  }
)

p_horizontal <- plot_horizontal_track(radar_track, adep_coords, ades_coords,
                                       route_coords = route_coords_real)
print(p_horizontal)
# ggplot2::ggsave("data/local/aderencia_horizontal_exemplo.png", p_horizontal, width = 9, height = 6)
