# ETAPA 3 -- Compara trajetoria VOADA (radar) x PLANEJADA (FPL).
#
# Le as saidas das Etapas 1 e 2, casa os voos, e para cada voo casado mede a
# aderencia horizontal (desvio da rota) e vertical (perfil de altitude),
# gerando os graficos de comparacao.
#
# Rode DEPOIS de 01_extract_trajectories.R e 03_extract_fpl_routes.R.

setwd(".")

source("R/parse_fpl.R"); source("R/parse_sigma_fpl.R")
source("R/route_geometry.R"); source("R/vertical_profile.R")
source("R/vertical_adherence.R"); source("R/horizontal_efficiency.R")
source("R/plot_vertical.R"); source("R/plot_horizontal.R")
source("R/trajectories.R"); source("R/plot_trajectories.R")
source("R/compare_trajectories.R")

dia_tag <- "2025_12_10"
L <- function(f) file.path("data/local", f)

airports_db <- read_airports_db("data/airports_br.csv")

## 1. Le radar (Etapa 1) e FPL (Etapa 2) --------------------------------------
positions   <- read_trajectories(L(paste0("trajectories_", dia_tag, ".parquet")))
radar_flights <- read_trajectories(L(paste0("flights_", dia_tag, ".parquet")))
fpl_routes  <- read_trajectories(L(paste0("fpl_routes_", dia_tag, ".parquet")))
fpl_flights <- read_trajectories(L(paste0("fpl_flights_", dia_tag, ".parquet")))

## 2. Casa radar x FPL --------------------------------------------------------
pares <- match_radar_to_fpl(radar_flights, fpl_flights, max_dep_diff_min = 60)
cat("Voos casados (radar x FPL):", nrow(pares), "\n")

## 3. Escolhe um voo casado (ajuste PAR_ADEP/PAR_ADES ou o indice) ------------
PAR_ADEP <- "SBCF"; PAR_ADES <- "SBSP"
cand <- pares[adep == PAR_ADEP & ades == PAR_ADES]
if (nrow(cand) == 0) { cat("Nenhum voo casado para", PAR_ADEP, "->", PAR_ADES,
                           "-- usando o primeiro par disponivel.\n"); cand <- pares }
voo <- cand[1]
cat("Comparando fid=", voo$fid, "com gufi=", voo$gufi, "(", voo$adep, "->", voo$ades, ")\n")

radar_pos <- positions[fid == voo$fid]
planned   <- fpl_routes[gufi == voo$gufi]

## 4. Compara -----------------------------------------------------------------
cmp <- compare_one_flight(
  radar_pos, planned,
  dep_elev_ft = lookup_airport_elevation_ft(voo$adep, airports_db),
  dest_elev_ft = lookup_airport_elevation_ft(voo$ades, airports_db))

cat("\n-- Aderencia HORIZONTAL (tolerancia 5 NM / RNAV-5) --\n"); print(cmp$resumo_h)
cat("-- Aderencia VERTICAL --\n"); print(cmp$resumo_v)

## 5. Graficos de comparacao --------------------------------------------------
adep_c <- lookup_airport_coords(voo$adep, airports_db)
ades_c <- lookup_airport_coords(voo$ades, airports_db)

# horizontal: rota planejada (FPL) x trajetoria voada (radar)
print(plot_horizontal_track(cmp$radar, adep_c, ades_c, route_coords = cmp$route_coords))
# vertical: perfil planejado x voado
print(plot_vertical_adherence(cmp$radar))

cat("\nDica: troque PAR_ADEP/PAR_ADES, ou escolha outro par em 'pares'.\n")
