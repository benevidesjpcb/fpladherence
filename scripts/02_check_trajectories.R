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

## 4. Um voo em detalhe -- HORIZONTAL e VERTICAL separados --------------------
# (troque o fid por um da tabela 'flights')
fid_exemplo <- flights[!is.na(adep_det) & !is.na(ades_det)][1]$fid

# 4a. trajetoria HORIZONTAL (mapa lat/lon)
p_voo_h <- plot_one_trajectory(positions, fid_exemplo)
print(p_voo_h)
# ggsave("data/local/qc_voo_horizontal.png", p_voo_h, width = 9, height = 6)

# 4b. trajetoria VERTICAL (altitude vs distancia voada)
p_voo_v <- plot_flight_vertical(positions, fid_exemplo)
print(p_voo_v)
# ggsave("data/local/qc_voo_vertical.png", p_voo_v, width = 9, height = 5)

## 5. Perfis verticais de um par de cidades sobrepostos ----------------------
p_par_v <- plot_vertical_profiles_pair(positions, PAR_ADEP, PAR_ADES)
print(p_par_v)
# ggsave("data/local/qc_par_vertical.png", p_par_v, width = 9, height = 5)

cat("\nDica: para inspecionar outro voo, escolha um fid na tabela 'flights' e rode:\n")
cat("  print(plot_one_trajectory(positions, SEU_FID))   # horizontal (mapa)\n")
cat("  print(plot_flight_vertical(positions, SEU_FID))  # vertical (altitude x distancia)\n")
