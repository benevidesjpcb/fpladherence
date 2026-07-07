# ETAPA 2 -- Extrai as trajetorias PLANEJADAS (rotas do FPL).
#
# Le o plano de voo do dia, resolve cada rota em pontos lat/lon (via
# waypoints_br + airports_br) e exporta:
#   data/local/fpl_routes_<dia>.parquet   (pontos da rota + cum_dist_nm + nivel filed)
#   data/local/fpl_routes_<dia>_sample.csv
#   data/local/fpl_status_<dia>.csv        (1 linha por voo: resolveu ou nao)
#
# Rode UMA VEZ por dia. Depois a Etapa 3 compara com as trajetorias de radar.

setwd(".")

source("R/parse_fpl.R")
source("R/parse_sigma_fpl.R")
source("R/route_geometry.R")
source("R/horizontal_efficiency.R")
source("R/trajectories.R")     # write_trajectory_table()
source("R/fpl_trajectories.R")

fpl_path <- "data/local/sigma_flight_plan_2025_12_10.csv"
dia_tag <- "2025_12_10"
out_dir <- "data/local"

# mesmos aeroportos de interesse da Etapa 1 (NULL = todos os brasileiros)
AEROPORTOS_INTERESSE <- NULL

## 1. Le o FPL e pega o plano em vigor de cada voo -----------------------------
cat("Lendo plano de voo...\n")
log_fpl <- read_sigma_fpl_log(fpl_path)
plans <- select_filed_plan(log_fpl)
horarios <- extract_actual_times(log_fpl)
plans <- merge(plans, horarios, by = "gufi", all.x = TRUE)
cat("  voos no FPL:", nrow(plans), "\n")

## 2. Filtra aeroportos de interesse ------------------------------------------
airports_db <- read_airports_db("data/airports_br.csv")
keep_icao <- if (is.null(AEROPORTOS_INTERESSE)) airports_db$icao else AEROPORTOS_INTERESSE
plans <- plans[plans$adep %in% keep_icao & plans$ades %in% keep_icao, ]
cat("  voos com O/D de interesse:", nrow(plans), "\n")

## 3. Navdata (waypoints oficiais + aerodromos) -------------------------------
waypoints_br <- read.csv("data/waypoints_br.csv", stringsAsFactors = FALSE)
navdata <- rbind(
  waypoints_br[, c("point", "lat", "lon")],
  data.frame(point = airports_db$icao, lat = airports_db$latitude, lon = airports_db$longitude)
)

## 4. Constroi as rotas planejadas --------------------------------------------
cat("Resolvendo rotas...\n")
res <- build_fpl_routes(plans, navdata)
routes <- res$routes
status <- res$status
cat("  voos com rota totalmente resolvida:", sum(status$resolvido),
    "de", nrow(status), "\n")
cat("  (nao resolvidos = algum fixo da rota fora da base de waypoints)\n")

## 5. Exporta ------------------------------------------------------------------
write_trajectory_table(
  routes,
  out_parquet = file.path(out_dir, paste0("fpl_routes_", dia_tag, ".parquet")),
  out_csv = file.path(out_dir, paste0("fpl_routes_", dia_tag, "_sample.csv")),
  csv_sample_n = 50000
)
data.table::fwrite(status, file.path(out_dir, paste0("fpl_status_", dia_tag, ".csv")))

# indice de voos do FPL com horarios (para casar com o radar na Etapa 3)
fpl_flights <- merge(
  data.table::as.data.table(plans)[, .(gufi, indicative, adep, ades, lvl, eobt_full,
                                        actual_dep, actual_arr)],
  status[, .(gufi, resolvido)], by = "gufi", all.x = TRUE)
write_trajectory_table(
  fpl_flights,
  out_parquet = file.path(out_dir, paste0("fpl_flights_", dia_tag, ".parquet")),
  out_csv = file.path(out_dir, paste0("fpl_flights_", dia_tag, ".csv")))

cat("\nPronto. Rotas planejadas em", out_dir, "\n")

## 6. Fixos que mais faltaram na base (para melhorar a navdata) ---------------
faltando <- status[resolvido == FALSE & pontos_faltando != ""]
if (nrow(faltando) > 0) {
  todos <- unlist(strsplit(faltando$pontos_faltando, ","))
  cat("\nFixos mais ausentes na navdata (top 15):\n")
  print(head(sort(table(todos), decreasing = TRUE), 15))
}
