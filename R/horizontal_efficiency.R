# Metodologia de aderencia/eficiencia horizontal: compara a trajetoria
# realmente voada (RADAR) com o plano (FPL), lateralmente. Primeira versao,
# no mesmo espirito do modulo vertical "sem navdata": usa o que ja temos
# (radar + aerodromos de partida/chegada) sem depender de resolver todos os
# fixos citados na rota do FPL.
#
# Metrica classica de eficiencia horizontal (no molde EUROCONTROL/DECEA):
#   eficiencia (%) = distancia direta (grande circulo, ADEP-ADES) /
#                    distancia realmente voada (RADAR) * 100
# Quanto mais perto de 100%, mais "direto" foi o voo.
#
# 'data/airports_br.csv' e uma tabela pequena, feita a mao (coordenadas
# aproximadas), so para os aerodromos mais comuns -- nao e dado oficial de
# AIP/AIRAC. Para navdata real, troque por uma fonte oficial.

library(geosphere)

#' Le a tabela de referencia de aerodromos (coordenadas aproximadas)
#'
#' @param path caminho do CSV (colunas: icao, name, lat, lon)
#' @return data.frame
read_airports_db <- function(path = "data/airports_br.csv") {
  read.csv(path, stringsAsFactors = FALSE)
}

#' Busca lat/lon de um aerodromo pelo codigo ICAO na tabela de referencia
#'
#' @param icao codigo ICAO (ex.: "SBGL")
#' @param airports_db data.frame retornado por read_airports_db()
#' @return vetor nomeado c(lat=, lon=), ou NULL se nao encontrado
lookup_airport_coords <- function(icao, airports_db) {
  row <- airports_db[trimws(airports_db$icao) == trimws(icao), ]
  if (nrow(row) == 0) return(NULL)
  c(lat = row$lat[1], lon = row$lon[1])
}

#' Distancia realmente voada (NM): soma das distancias (grande circulo)
#' entre posicoes consecutivas do radar.
#'
#' @param radar_track data.frame ordenado por tempo, com colunas lat, lon
#' @return distancia total em NM
compute_actual_distance_nm <- function(radar_track) {
  n <- nrow(radar_track)
  if (n < 2) return(0)
  leg_m <- distHaversine(
    as.matrix(radar_track[-n, c("lon", "lat")]),
    as.matrix(radar_track[-1, c("lon", "lat")])
  )
  sum(leg_m) / 1852
}

#' Distancia direta (grande circulo) entre partida e chegada, em NM
#'
#' @param adep_coords vetor c(lat=, lon=)
#' @param ades_coords vetor c(lat=, lon=)
#' @return distancia em NM
compute_direct_distance_nm <- function(adep_coords, ades_coords) {
  distHaversine(c(adep_coords["lon"], adep_coords["lat"]),
                 c(ades_coords["lon"], ades_coords["lat"])) / 1852
}

#' Calcula a eficiencia horizontal de um voo: distancia direta / distancia
#' realmente voada.
#'
#' @param radar_track data.frame com colunas lat, lon (trajetoria real)
#' @param adep_coords vetor c(lat=, lon=) do aerodromo de partida
#' @param ades_coords vetor c(lat=, lon=) do aerodromo de chegada
#' @return data.frame com distancia_direta_nm, distancia_voada_nm,
#'   eficiencia_pct, excesso_nm
compute_horizontal_efficiency <- function(radar_track, adep_coords, ades_coords) {
  direta <- compute_direct_distance_nm(adep_coords, ades_coords)
  voada <- compute_actual_distance_nm(radar_track)

  data.frame(
    distancia_direta_nm = round(direta, 1),
    distancia_voada_nm = round(voada, 1),
    eficiencia_pct = round(100 * direta / voada, 1),
    excesso_nm = round(voada - direta, 1)
  )
}
