# Script exploratorio para testar o pipeline de aderencia vertical (sem
# navdata) com um voo real do dia 2025-12-10.
#
# Como usar (rode no R, de dentro da pasta raiz do projeto):
#   1. Rode ate a secao "1. Candidatos" e olhe a tabela impressa.
#   2. Escolha um 'ssr' (ou 'indicative') da tabela e cole na secao 2.
#   3. Rode o resto do script.
#
# Pacotes necessarios: install.packages(c("ggplot2", "geosphere", "data.table"))

setwd(".") # garanta que esta na raiz do projeto (onde ficam as pastas R/, data/)

source("R/parse_fpl.R")
source("R/parse_sigma_fpl.R")
source("R/parse_radar.R")
source("R/parse_sigma_radar.R")
source("R/vertical_adherence_radar_only.R")
source("R/plot_vertical.R")

fpl_path <- "data/local/sigma_flight_plan_2025_12_10.csv"
radar_path <- "data/local/radar_2025_12_10.csv"

## 1. Candidatos --------------------------------------------------------------
log_fpl <- read_sigma_fpl_log(fpl_path)
planos <- select_filed_plan(log_fpl)

cat("Total de voos no FPL:", nrow(planos), "\n")
print(head(planos[, c("gufi", "indicative", "adep", "ades", "lvl", "ssr")], 15))

## 2. Escolha um voo -----------------------------------------------------------
# Troque o valor abaixo por um 'ssr' da tabela impressa acima.
ssr_escolhido <- planos$ssr[1]

voo <- planos[planos$ssr == ssr_escolhido, ][1, ]
cat("\nVoo escolhido:\n")
print(voo)

## 3. Radar do voo escolhido ---------------------------------------------------
# Leitura do arquivo grande (pode levar 1-2 min). 'select' reduz colunas
# carregadas para acelerar (nao precisamos de addep/addes/dh_inicio/dh_fim/
# ds_acctypes para a aderencia vertical sem navdata).
log_radar <- read_sigma_radar_log(
  radar_path,
  select = c("callsign", "vl_latitude", "vl_longitude", "nr_flightlevel",
             "nr_speed", "nr_ssr", "dt_radar")
)

radar_track <- sigma_radar_to_track(log_radar, ssr = ssr_escolhido)
cat("\nPosicoes de radar encontradas para esse voo:", nrow(radar_track), "\n")

if (nrow(radar_track) == 0) {
  stop("Nenhuma posicao de radar encontrada para esse ssr -- ",
       "escolha outro voo na tabela da secao 1, ou confira se o voo ",
       "e do mesmo dia do arquivo de radar.")
}

## 4. Aderencia vertical (deteccao de fase pela taxa de subida/descida) -------
filed_level_ft <- level_token_to_ft(voo$lvl)
cat("\nNivel filed:", voo$lvl, "=", filed_level_ft, "ft\n")

radar_track <- detect_flight_phases(radar_track)
radar_track <- compute_vertical_deviation_radar_only(radar_track, filed_level_ft)

cat("\nResumo por fase:\n")
print(summarise_vertical_adherence_radar_only(radar_track))

## 5. Grafico -------------------------------------------------------------------
p <- plot_vertical_adherence_radar_only(radar_track, filed_level_ft)
print(p)
# ggplot2::ggsave("data/local/aderencia_vertical_exemplo.png", p, width = 9, height = 5.5)
