# ETAPA 1 -- Extracao de trajetorias a partir do radar bruto.
#
# Le o radar bruto do dia UMA VEZ, limpa, segmenta em voos (fid), detecta
# origem/destino de cada voo e exporta:
#   - data/local/trajectories_2025_12_10.parquet  (todas as posicoes -- para processar)
#   - data/local/trajectories_2025_12_10_sample.csv (amostra -- para abrir no Excel)
#   - data/local/flights_2025_12_10.csv            (indice: 1 linha por voo)
#
# Rode UMA VEZ por dia de dados. Depois, a analise (Etapa 3) le so o Parquet
# de trajetorias, nunca mais o radar bruto.
#
# Requisitos: install.packages(c("data.table","geosphere","lubridate","arrow"))
# (arrow so para gravar Parquet; sem ele, grava so os CSV e avisa.)

setwd(".")

source("R/parse_sigma_radar.R")   # read_sigma_radar_log()
source("R/horizontal_efficiency.R") # read_airports_db()
source("R/trajectories.R")

radar_path <- "data/local/radar_2025_12_10.csv"
dia_tag <- "2025_12_10"
out_dir <- "data/local"

# Aeroportos de interesse: mantem so voos cuja ORIGEM E DESTINO estao nesta
# lista. Por padrao, todos os aerodromos brasileiros conhecidos (elimina voos
# internacionais como ...->KJFK, que nao estao na base). Para focar num
# conjunto especifico, descomente e ajuste:
# AEROPORTOS_INTERESSE <- c("SBCF","SBSP","SBGR","SBRJ","SBBR","SBSV","SBKP","SBCT")
AEROPORTOS_INTERESSE <- NULL  # NULL = todos os aerodromos de data/airports_br.csv

## 1. Le o radar bruto (uma vez) -----------------------------------------------
cat("Lendo radar bruto...\n")
log_radar <- read_sigma_radar_log(
  radar_path,
  select = c("callsign", "addep", "addes", "vl_latitude", "vl_longitude",
             "nr_flightlevel", "dt_radar")
)
cat("  posicoes brutas:", nrow(log_radar), "\n")

## 2. Limpa + segmenta em voos (fid) + distancia voada acumulada -------------
cat("Limpando e segmentando...\n")
positions <- clean_radar_log(log_radar)
positions <- segment_trajectories(positions, max_gap_min = 30)
# distancia acumulada voada (NM) -> base do perfil VERTICAL (altitude vs dist)
positions <- add_cumulative_flown_distance(positions)
cat("  posicoes validas:", nrow(positions), "| voos (fid):",
    length(unique(positions$fid)), "\n")

## 3. Resolve origem/destino por voo (radar primeiro, fallback geometrico) -----
cat("Resolvendo origem/destino por voo...\n")
airports_db <- read_airports_db("data/airports_br.csv")
flights <- resolve_flight_od(positions, airports_db, fallback_radius_nm = 5)

od_ok <- sum(!is.na(flights$adep_det) & !is.na(flights$ades_det))
od_radar <- sum(flights$adep_src == "radar" & flights$ades_src == "radar", na.rm = TRUE)
cat("  voos com ADEP e ADES:", od_ok, "de", nrow(flights),
    "(", od_radar, "direto do radar,", od_ok - od_radar, "por fallback geometrico )\n")

## 3b. Descarta voos de O/D duvidosa (dois trechos colados, ou que nao
##     comecam/terminam perto do aeroporto declarado) -------------------------
flights <- flag_flight_od_quality(flights, max_endpoint_nm = 30)
n_incons <- sum(flights$adep_n > 1 | flights$ades_n > 1, na.rm = TRUE)
n_longe <- sum((flights$dist_adep_nm > 30 | flights$dist_ades_nm > 30) &
                 flights$adep_n <= 1 & flights$ades_n <= 1, na.rm = TRUE)
cat("  descartados por O/D inconsistente (2 trechos colados):", n_incons, "\n")
cat("  descartados por extremos longe do aeroporto declarado:", n_longe, "\n")
flights <- flights[od_ok == TRUE]
positions <- positions[fid %in% flights$fid]

## 3c. Filtra para os aeroportos de interesse (origem E destino) ---------------
keep_icao <- if (is.null(AEROPORTOS_INTERESSE)) airports_db$icao else AEROPORTOS_INTERESSE
n_antes <- nrow(flights)
flights <- flights[adep_det %in% keep_icao & ades_det %in% keep_icao]
positions <- positions[fid %in% flights$fid]
cat("  voos mantidos (bons, origem e destino de interesse):", nrow(flights),
    "de", n_antes, "\n")

# anexa adep_det/ades_det as posicoes (para filtrar por par de cidades depois
# sem precisar de join no momento da analise)
positions <- merge(positions, flights[, .(fid, adep_det, ades_det)], by = "fid", all.x = TRUE)
data.table::setorder(positions, fid, ts)

## 4. Exporta ------------------------------------------------------------------
cat("Exportando...\n")
write_trajectory_table(
  positions,
  out_parquet = file.path(out_dir, paste0("trajectories_", dia_tag, ".parquet")),
  out_csv = file.path(out_dir, paste0("trajectories_", dia_tag, "_sample.csv")),
  csv_sample_n = 50000
)
write_trajectory_table(
  flights,
  out_parquet = file.path(out_dir, paste0("flights_", dia_tag, ".parquet")),
  out_csv = file.path(out_dir, paste0("flights_", dia_tag, ".csv"))
)

cat("\nPronto. Arquivos em", out_dir, ":\n")
cat("  trajectories_", dia_tag, ".parquet  (todas as posicoes, com fid + adep/ades)\n", sep = "")
cat("  trajectories_", dia_tag, "_sample.csv (amostra p/ Excel)\n", sep = "")
cat("  flights_", dia_tag, ".csv / .parquet  (1 linha por voo)\n", sep = "")

## 5. Espia os pares de cidades mais frequentes do dia -------------------------
pares <- flights[!is.na(adep_det) & !is.na(ades_det), .N, by = .(adep_det, ades_det)]
data.table::setorder(pares, -N)
cat("\nPares de cidades mais frequentes (top 15):\n")
print(head(pares, 15))
