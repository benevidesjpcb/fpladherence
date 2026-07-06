# Visualizacao / controle de qualidade (QC) das trajetorias extraidas na
# Etapa 1 (ver R/trajectories.R, scripts/01_extract_trajectories.R).
#
# Serve para olhar no mapa se a segmentacao por voo (fid) e a
# origem/destino detectados ficaram bons antes de seguir para a comparacao
# com o plano de voo.

library(ggplot2)
library(data.table)

#' Le a tabela de trajetorias (Parquet via arrow/nanoparquet, ou CSV)
#'
#' @param path caminho .parquet ou .csv
#' @return data.table
read_trajectories <- function(path) {
  if (grepl("\\.parquet$", path)) {
    if (requireNamespace("arrow", quietly = TRUE)) {
      return(data.table::as.data.table(arrow::read_parquet(path)))
    }
    if (requireNamespace("nanoparquet", quietly = TRUE)) {
      return(data.table::as.data.table(nanoparquet::read_parquet(path)))
    }
    stop("Instale 'arrow' ou 'nanoparquet' para ler Parquet (ou aponte para o CSV de amostra).")
  }
  data.table::fread(path)
}

#' Resumo de QC do dia: contagens, fonte de O/D, distribuicao de pontos por
#' voo, e sinais de voo possivelmente mal segmentado (poucos pontos, ou
#' inicio/fim longe um do outro sem O/D).
#'
#' @param positions data.table de posicoes (com fid, adep_det, ades_det)
#' @param flights data.table indice de voos (resolve_flight_od()); se NULL,
#'   e derivado das posicoes
#' @return (invisivel) o data.table de voos; imprime o resumo
qc_summary <- function(positions, flights = NULL) {
  dt <- data.table::as.data.table(positions)
  if (is.null(flights)) {
    flights <- dt[, .(n_pos = .N, adep_det = adep_det[1], ades_det = ades_det[1]), by = fid]
  }

  cat("== QC das trajetorias ==\n")
  cat("Posicoes:", nrow(dt), "| Voos (fid):", nrow(flights), "\n\n")

  cat("Pontos por voo (distribuicao):\n")
  print(summary(flights$n_pos))
  poucos <- sum(flights$n_pos < 10)
  cat("  voos com < 10 pontos (suspeitos):", poucos, "\n\n")

  cat("Origem/destino resolvidos:\n")
  cat("  com ADEP e ADES:", sum(!is.na(flights$adep_det) & !is.na(flights$ades_det)),
      "| sem algum:", sum(is.na(flights$adep_det) | is.na(flights$ades_det)), "\n")
  if ("adep_src" %in% names(flights)) {
    cat("  fonte ADEP:", paste(names(table(flights$adep_src, useNA = "ifany")),
                                table(flights$adep_src, useNA = "ifany"), sep = "=", collapse = "  "), "\n")
  }
  cat("\nTop 15 pares de cidades:\n")
  pares <- flights[!is.na(adep_det) & !is.na(ades_det), .N, by = .(adep_det, ades_det)]
  data.table::setorder(pares, -N)
  print(head(pares, 15))

  invisible(flights)
}

#' Plota varias trajetorias num mapa lat/lon (uma linha por voo). Bom para
#' um panorama do dia ou de um par de cidades.
#'
#' @param positions data.table de posicoes (fid, lat, lon, adep_det, ades_det)
#' @param adep,ades opcional: filtra so os voos desse par (em qualquer sentido)
#' @param airports_db opcional: data.frame (icao, latitude, longitude) para
#'   marcar os aerodromos de interesse
#' @param max_flights se houver mais voos que isso, amostra aleatoriamente
#'   (evita mapas ilegiveis / lentos)
#' @return objeto ggplot
plot_trajectories_map <- function(positions, adep = NULL, ades = NULL,
                                   airports_db = NULL, max_flights = 300) {
  dt <- data.table::as.data.table(positions)

  subtitulo <- "todos os voos do dia"
  if (!is.null(adep) && !is.null(ades)) {
    dt <- dt[(adep_det == adep & ades_det == ades) | (adep_det == ades & ades_det == adep)]
    subtitulo <- paste0(adep, " <-> ", ades, " (", length(unique(dt$fid)), " voos)")
  }

  fids <- unique(dt$fid)
  if (length(fids) > max_flights) {
    fids <- sample(fids, max_flights)
    dt <- dt[fid %in% fids]
    subtitulo <- paste0(subtitulo, " -- amostra de ", max_flights)
  }

  p <- ggplot(dt, aes(x = lon, y = lat, group = fid)) +
    geom_path(alpha = 0.35, linewidth = 0.3, color = "steelblue")

  if (!is.null(airports_db) && !is.null(adep) && !is.null(ades)) {
    aps <- airports_db[airports_db$icao %in% c(adep, ades), ]
    p <- p +
      geom_point(data = aps, aes(x = longitude, y = latitude), inherit.aes = FALSE,
                 shape = 17, size = 3, color = "black") +
      geom_text(data = aps, aes(x = longitude, y = latitude, label = icao),
                inherit.aes = FALSE, vjust = -1, size = 3.5, fontface = "bold")
  }

  lat_med <- mean(range(dt$lat, na.rm = TRUE))
  p +
    coord_fixed(ratio = 1 / cos(lat_med * pi / 180)) +
    labs(title = "Trajetorias extraidas do RADAR", subtitle = subtitulo,
         x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 12)
}

#' PERFIL VERTICAL de um voo: altitude (ft) vs distancia acumulada voada
#' (NM) -- a "trajetoria vertical", separada da horizontal (mapa lat/lon).
#' Replica a ideia do plot_flight_vertical_distance() do trrrj (BRA-HFE).
#'
#' @param positions data.table com fid, cum_dist_nm, altitude_ft (rode
#'   add_cumulative_flown_distance() antes, ou use o Parquet da Etapa 1 que
#'   ja traz cum_dist_nm)
#' @param fid_alvo identificador do voo (coluna fid)
#' @return objeto ggplot
plot_flight_vertical <- function(positions, fid_alvo) {
  dt <- data.table::as.data.table(positions)[fid == fid_alvo]
  data.table::setorder(dt, cum_dist_nm)
  od <- paste0(dt$adep_det[1], " -> ", dt$ades_det[1])

  ggplot(dt, aes(x = cum_dist_nm, y = altitude_ft)) +
    geom_line(color = "#9c36b5", linewidth = 0.8) +
    geom_point(size = 0.8, color = "#9c36b5", alpha = 0.5) +
    labs(title = paste0("Perfil vertical -- voo fid=", fid_alvo, " (", dt$callsign[1], ")"),
         subtitle = paste0(od, " -- ", nrow(dt), " posicoes"),
         x = "Distancia voada (NM)", y = "Altitude (ft)") +
    theme_minimal(base_size = 12)
}

#' PERFIS VERTICAIS de todos os voos de um par de cidades sobrepostos --
#' bom para ver a dispersao (subida/cruzeiro/descida) do par de uma vez.
#'
#' @param positions data.table (com fid, cum_dist_nm, altitude_ft, adep_det,
#'   ades_det)
#' @param adep,ades par de cidades (so o sentido adep->ades)
#' @return objeto ggplot
plot_vertical_profiles_pair <- function(positions, adep, ades) {
  dt <- data.table::as.data.table(positions)[adep_det == adep & ades_det == ades]
  n_voos <- length(unique(dt$fid))
  ggplot(dt, aes(x = cum_dist_nm, y = altitude_ft, group = fid)) +
    geom_line(alpha = 0.35, linewidth = 0.4, color = "#9c36b5") +
    labs(title = paste0("Perfis verticais: ", adep, " -> ", ades),
         subtitle = paste0(n_voos, " voos"),
         x = "Distancia voada (NM)", y = "Altitude (ft)") +
    theme_minimal(base_size = 12)
}

#' Plota UMA trajetoria HORIZONTAL em detalhe (mapa lat/lon, colorida pela
#' altitude), com inicio/fim marcados -- para inspecionar um voo especifico.
#'
#' @param positions data.table de posicoes
#' @param fid_alvo identificador do voo (coluna fid)
#' @return objeto ggplot
plot_one_trajectory <- function(positions, fid_alvo) {
  dt <- data.table::as.data.table(positions)[fid == fid_alvo]
  data.table::setorder(dt, ts)
  extremos <- dt[c(1, .N)]
  extremos$marca <- c("inicio", "fim")

  od <- paste0(dt$adep_det[1], " -> ", dt$ades_det[1])
  lat_med <- mean(range(dt$lat, na.rm = TRUE))

  ggplot(dt, aes(x = lon, y = lat)) +
    geom_path(aes(color = altitude_ft), linewidth = 0.8) +
    geom_point(data = extremos, aes(x = lon, y = lat, shape = marca), size = 3) +
    scale_color_viridis_c(name = "Altitude (ft)") +
    scale_shape_manual(values = c(inicio = 16, fim = 4), name = NULL) +
    coord_fixed(ratio = 1 / cos(lat_med * pi / 180)) +
    labs(title = paste0("Voo fid=", fid_alvo, " (", dt$callsign[1], ")"),
         subtitle = paste0(od, " -- ", nrow(dt), " posicoes"),
         x = "Longitude", y = "Latitude") +
    theme_minimal(base_size = 12)
}
