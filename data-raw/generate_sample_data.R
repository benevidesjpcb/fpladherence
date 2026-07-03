# Gera a trajetoria de RADAR sintetica usada como exemplo em
# analysis/vertical_adherence.qmd.
#
# Os dados sao ILUSTRATIVOS (nao sao dados operacionais reais). O perfil de
# altitude foi desenhado propositalmente para demonstrar a metodologia:
#  - subida aderente ao plano ate o 1o nivel de cruzeiro
#  - um trecho de cruzeiro em que o ATC mantem a aeronave 2000 ft abaixo do
#    nivel planejado (nao aderente)
#  - a subida em degrau planejada (F370 -> F390) ocorrendo com pequeno atraso
#    (ainda dentro da tolerancia)
#  - uma descida tardia (nivel alto mantido bem alem do inicio da referencia
#    de descida continua), tipico caso real de ineficiencia vertical
#
# Executar a partir da raiz do projeto: Rscript data-raw/generate_sample_data.R

set.seed(42)

source("R/route_geometry.R")

waypoints <- read.csv("data/waypoints.csv")
route_pts <- add_cumulative_distance(waypoints[, c("point", "lat", "lon")])
route_pts$dist_nm <- round(route_pts$dist_nm, 1)
print(route_pts[, c("point", "dist_nm")])

# keyframes (distancia NM, altitude ft) definindo o perfil real desenhado.
# Niveis planejados no FPL: F230 (23000 ft) ate UZUKA, subida em degrau para
# F250 (25000 ft) dali ate o destino -- niveis compativeis com uma rota curta
# (~186 NM), como em uma ponte-aerea.
keyframes <- data.frame(
  dist_nm = c(0, 10, 20, 30, 40, 46.3,
              60, 70, 75, 85, 95, 102.0,
              110, 120, 133.4, 138, 145, 148.4,
              155, 160, 165, 170, 175, 180, 185.6),
  altitude_ft = c(2459, 7295, 11632, 15568, 20455, 23000,
                  23000, 23000, 21500, 21000, 21000, 21000,
                  21500, 23000, 23000, 23000, 24500, 24700,
                  25300, 25000, 25000, 20000, 13000, 5000, 11)
)

# amostragem a cada ~2 NM ao longo da rota
dist_grid <- seq(0, max(route_pts$dist_nm), by = 2)
dist_grid <- sort(unique(c(dist_grid, route_pts$dist_nm)))

altitude_ft <- approx(keyframes$dist_nm, keyframes$altitude_ft, xout = dist_grid)$y
altitude_ft <- altitude_ft + rnorm(length(altitude_ft), mean = 0, sd = 60) # ruido de medicao

# velocidade media no solo (kt) por fase, para gerar os timestamps
gs_for_dist <- function(d) {
  ifelse(d <= 46.3, 300, ifelse(d <= 165, 460, 350))
}
delta_dist <- c(0, diff(dist_grid))
delta_h <- delta_dist / gs_for_dist(dist_grid) # horas
elapsed_h <- cumsum(delta_h)
eobt <- as.POSIXct("2026-07-03 12:00:00", tz = "UTC")
timestamp <- eobt + elapsed_h * 3600

# posicao lat/lon: interpola ao longo da polilinha da rota pela distancia, com
# pequeno jitter lateral para simular ruido de posicionamento do radar
interp_route_position <- function(d, route_pts) {
  n <- nrow(route_pts)
  d <- min(max(d, route_pts$dist_nm[1]), route_pts$dist_nm[n])
  seg <- max(which(route_pts$dist_nm <= d))
  seg <- min(seg, n - 1)
  frac <- (d - route_pts$dist_nm[seg]) /
    (route_pts$dist_nm[seg + 1] - route_pts$dist_nm[seg])
  lat <- route_pts$lat[seg] + frac * (route_pts$lat[seg + 1] - route_pts$lat[seg])
  lon <- route_pts$lon[seg] + frac * (route_pts$lon[seg + 1] - route_pts$lon[seg])
  c(lat = lat, lon = lon)
}

positions <- t(vapply(dist_grid, interp_route_position, numeric(2), route_pts = route_pts))
lat <- positions[, "lat"] + rnorm(length(dist_grid), 0, 0.01)
lon <- positions[, "lon"] + rnorm(length(dist_grid), 0, 0.01)

sample_radar <- data.frame(
  callsign = "TAM3202",
  timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%SZ"),
  lat = round(lat, 5),
  lon = round(lon, 5),
  altitude_ft = round(altitude_ft, 0)
)

write.csv(sample_radar, "data/sample_radar.csv", row.names = FALSE)
cat("Gerado data/sample_radar.csv com", nrow(sample_radar), "posicoes\n")
