# ETAPA 1 (QC) -- Conferir visualmente as trajetorias extraidas.
#
# Le o Parquet de trajetorias gerado por scripts/01_extract_trajectories.R,
# imprime um resumo de qualidade e gera mapas para voce olhar se ficou bom.
#
# Rode DEPOIS do 01_extract_trajectories.R.

setwd(".")

source("R/horizontal_efficiency.R") # read_airports_db()
source("R/plot_trajectories.R")

dia_tag <- "2025_12_10"
traj_path <- file.path("data/local", paste0("trajectories_", dia_tag, ".parquet"))
# se nao tiver arrow/nanoparquet instalado, use a amostra CSV:
# traj_path <- file.path("data/local", paste0("trajectories_", dia_tag, "_sample.csv"))

airports_db <- read_airports_db("data/airports_br.csv")

## 1. Le e resume -------------------------------------------------------------
positions <- read_trajectories(traj_path)
flights <- qc_summary(positions)

## 2. Panorama do dia (amostra de voos no mapa) -------------------------------
p_dia <- plot_trajectories_map(positions, max_flights = 300)
print(p_dia)
# ggsave("data/local/qc_trajetorias_dia.png", p_dia, width = 9, height = 8)

## 3. Um par de cidades especifico --------------------------------------------
PAR_ADEP <- "SBCF"
PAR_ADES <- "SBSP"
p_par <- plot_trajectories_map(positions, adep = PAR_ADEP, ades = PAR_ADES,
                               airports_db = airports_db)
print(p_par)
# ggsave("data/local/qc_trajetorias_par.png", p_par, width = 9, height = 8)

## 4. Um voo em detalhe (troque o fid por um da tabela 'flights') -------------
fid_exemplo <- flights[!is.na(adep_det) & !is.na(ades_det)][1]$fid
p_voo <- plot_one_trajectory(positions, fid_exemplo)
print(p_voo)
# ggsave("data/local/qc_voo_exemplo.png", p_voo, width = 9, height = 6)

cat("\nDica: para inspecionar outro voo, escolha um fid na tabela 'flights'\n")
cat("e rode: print(plot_one_trajectory(positions, SEU_FID))\n")
