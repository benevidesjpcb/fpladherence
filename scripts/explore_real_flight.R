# Script exploratorio para testar o pipeline de aderencia vertical (sem
# navdata) com um voo real do dia 2025-12-10.
#
# Como usar (rode no R, de dentro da pasta raiz do projeto):
#   1. Rode o script inteiro. Ele le o radar UMA VEZ (pode levar 1-2 min) e
#      tenta automaticamente ate achar um voo com posicoes de radar DENTRO
#      da janela real de decolagem/pouso (evita misturar voos diferentes
#      que reutilizaram o mesmo codigo de transponder -- ssr).
#   2. Se o diagnostico da secao 2 mostrar 0 ssr em comum entre os dois
#      arquivos, cole a saida aqui no chat que a gente ajusta o parser.
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

## 1. Le os dois arquivos (uma vez so) -----------------------------------------
log_fpl <- read_sigma_fpl_log(fpl_path)
planos <- select_filed_plan(log_fpl)
horarios <- extract_actual_times(log_fpl)
planos <- merge(planos, horarios, by = "gufi", all.x = TRUE)
cat("Total de voos no FPL:", nrow(planos), "\n")

log_radar <- read_sigma_radar_log(
  radar_path,
  select = c("callsign", "vl_latitude", "vl_longitude", "nr_flightlevel",
             "nr_speed", "nr_ssr", "dt_radar")
)
cat("Total de posicoes no radar:", nrow(log_radar), "\n")

## 2. Diagnostico de casamento por ssr ------------------------------------------
ssr_fpl <- trimws(as.character(planos$ssr))
ssr_radar <- trimws(as.character(log_radar$nr_ssr))

cat("\n-- Diagnostico --\n")
cat("Classe ssr (FPL):   ", class(planos$ssr), " | exemplos:", paste(head(ssr_fpl, 5), collapse=", "), "\n")
cat("Classe nr_ssr(RADAR):", class(log_radar$nr_ssr), " | exemplos:", paste(head(ssr_radar, 5), collapse=", "), "\n")

# remove NA antes de comparar -- "NA" == "NA" nao deveria contar como par
ssr_fpl_validos <- unique(ssr_fpl[!is.na(planos$ssr)])
ssr_radar_validos <- unique(ssr_radar[!is.na(log_radar$nr_ssr)])

ssr_em_comum <- intersect(ssr_fpl_validos, ssr_radar_validos)
cat("SSRs distintos no FPL:  ", length(ssr_fpl_validos),
    " (", sum(is.na(planos$ssr)), " voos sem ssr)\n")
cat("SSRs distintos no RADAR:", length(ssr_radar_validos),
    " (", sum(is.na(log_radar$nr_ssr)), " posicoes sem ssr)\n")
cat("SSRs em comum:          ", length(ssr_em_comum), "\n")

if (length(ssr_em_comum) == 0) {
  stop("Nenhum ssr em comum entre FPL e RADAR -- e um problema de formato/",
       "tipo de dado (ver exemplos acima), nao de cobertura. Cole essa saida ",
       "no chat.")
}

## 3. So voos com horario real de decolagem/pouso confirmado -------------------
# ATENCAO: ssr (squawk) e reutilizado por varios voos ao longo do dia. Sem
# uma janela de horario, filtrar so por ssr mistura posicoes de aeronaves
# diferentes num "voo" so (perfil com saltos de altitude sem sentido). Por
# isso so tentamos voos com actual_dep/actual_arr confirmados no FPL.
planos_com_radar <- planos[ssr_fpl %in% ssr_em_comum, , drop = FALSE]
planos_testaveis <- planos_com_radar[!is.na(planos_com_radar$actual_dep) &
                                        !is.na(planos_com_radar$actual_arr), , drop = FALSE]
cat("\nVoos com ssr em comum:                          ", nrow(planos_com_radar), "\n")
cat("...com decolagem E pouso confirmados no FPL:      ", nrow(planos_testaveis), "\n")

if (nrow(planos_testaveis) == 0) {
  stop("Nenhum voo com ssr em comum tem decolagem/pouso confirmados (DEP/ARR) ",
       "no FPL -- sem isso nao da pra montar uma janela de horario segura. ",
       "Investigue manualmente 'planos_com_radar'.")
}

## 4. Acha automaticamente o primeiro voo com radar suficiente NA JANELA REAL --
buffer_min <- 15 # margem antes da decolagem / depois do pouso

voo <- NULL
radar_track <- NULL
for (i in seq_len(min(nrow(planos_testaveis), 50))) {
  candidato <- planos_testaveis[i, ]
  janela <- c(candidato$actual_dep - buffer_min * 60,
              candidato$actual_arr + buffer_min * 60)
  tentativa <- sigma_radar_to_track(log_radar, ssr = candidato$ssr, time_window = janela)
  if (nrow(tentativa) >= 10) {
    voo <- candidato
    radar_track <- tentativa
    cat("\nVoo escolhido (", nrow(tentativa), "posicoes de radar entre",
        format(janela[1]), "e", format(janela[2]), "):\n")
    print(voo)
    break
  }
}

if (is.null(voo)) {
  stop("Nenhum dos primeiros 50 voos testaveis tem >= 10 posicoes de radar ",
       "dentro da janela real de decolagem/pouso. Aumente o limite (",
       "'min(nrow(planos_testaveis), 50)') ou investigue manualmente.")
}

## 5. Reforco: mantem so o maior trecho continuo -------------------------------
# rede de seguranca extra contra ssr reaproveitado dentro da propria janela
n_antes <- nrow(radar_track)
radar_track <- keep_largest_contiguous_segment(radar_track, max_gap_min = 5)
if (nrow(radar_track) < n_antes) {
  cat("Removidas", n_antes - nrow(radar_track),
      "posicoes fora do maior trecho continuo (provavel outro voo/ssr).\n")
}

## 6. Aderencia vertical (deteccao de fase pela taxa de subida/descida) --------
filed_level_ft <- level_token_to_ft(voo$lvl)
cat("\nNivel filed:", voo$lvl, "=", filed_level_ft, "ft\n")

radar_track <- detect_flight_phases(radar_track)
radar_track <- compute_vertical_deviation_radar_only(radar_track, filed_level_ft)

cat("\nResumo por fase:\n")
print(summarise_vertical_adherence_radar_only(radar_track))

## 7. Grafico -------------------------------------------------------------------
p <- plot_vertical_adherence_radar_only(radar_track, filed_level_ft)
print(p)
# ggplot2::ggsave("data/local/aderencia_vertical_exemplo.png", p, width = 9, height = 5.5)
