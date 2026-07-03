# Construcao do perfil vertical planejado a partir da rota do FPL, e
# classificacao das fases de voo (subida / cruzeiro / descida).
#
# Metodologia:
#  - O FPL define niveis de cruzeiro "degrau" (step levels) por trecho da
#    rota (campo 15). Cada ponto carrega o nivel EM VIGOR A PARTIR DAQUELE
#    ponto (semantica ICAO do token "PONTO/velocidade+nivel").
#  - 1o trecho (saida -> 1o ponto): fase SUBIDA. Como o FPL nao descreve o
#    perfil de subida, usa-se uma referencia idealizada de subida continua
#    (rampa linear) da elevacao do aerodromo de partida ate o 1o nivel de
#    cruzeiro, ao longo de todo o trecho.
#  - Trechos intermediarios em que o nivel planejado NAO muda: fase
#    CRUZEIRO, com o nivel mantido constante (degrau).
#  - Quando o nivel planejado aumenta no ponto seguinte (subida em degrau,
#    ex.: UZUKA/F390 apos PIRUX/F370): em vez de tratar a mudanca como um
#    degrau instantaneo (o que penalizaria injustamente uma subida real que
#    ocorre num trecho de distancia), define-se uma JANELA DE TRANSICAO de
#    'transition_nm' milhas terminando exatamente no ponto de mudanca, onde
#    a referencia sobe linearmente do nivel antigo ao novo. Fora da janela,
#    o trecho e tratado como CRUZEIRO no nivel antigo.
#  - Ultimo trecho (ultimo ponto -> destino): fase DESCIDA. O FPL tambem nao
#    especifica perfil de descida; usa-se uma referencia idealizada de
#    descida continua (rampa linear) do ultimo nivel de cruzeiro planejado
#    ate a elevacao do aerodromo de destino, ao longo de todo o trecho.
#
# Esta e uma simplificacao deliberada (documentada) para a primeira versao
# da metodologia: ela avalia aderencia ao PLANO FILED (FPL), nao as
# autorizacoes reais de ATC (que nao fazem parte do FPL).
#
# CASO DEGENERADO -- rota direta (DCT) sem fixos intermediarios: com so 2
# pontos (ADEP, ADES), o trecho inteiro seria simultaneamente "1o trecho"
# (subida) e "ultimo trecho" (descida), e o codigo classificaria a rota
# TODA como uma unica fase (bug real, encontrado testando com um voo curto
# tipo SBCF-SBSP). insert_toc_tod_if_direct() insere um "topo de subida"
# (TOC) e "topo de descida" (TOD) sinteticos nesse caso, usando a regra
# pratica 3:1 (3 NM por 1000 ft) para estimar onde a subida/descida
# terminam, deixando cruzeiro no meio (ou nenhum, se a rota for curta
# demais para os dois).

library(geosphere)

default_transition_nm <- 15

#' Insere pontos sinteticos de topo de subida (TOC) e topo de descida (TOD)
#' quando a rota planejada tem so 2 pontos (ADEP-ADES diretos) -- ver nota no
#' cabecalho deste arquivo. Sem isso, build_planned_profile() nao consegue
#' representar subida+cruzeiro+descida com um unico segmento.
#'
#' @param route_coords rota com colunas point, level_ft, is_level_change,
#'   lat, lon, dist_nm
#' @param dep_elevation_ft,dest_elevation_ft elevacao (pes) de partida/destino
#' @param climb_gradient_nm_per_1000ft,descent_gradient_nm_per_1000ft
#'   distancia (NM) assumida por 1000 ft de subida/descida (regra pratica
#'   3:1 por padrao)
#' @return route_coords, sem alteracao se nrow != 2; com TOC/TOD inseridos
#'   (e 'seq' recalculado) caso contrario
insert_toc_tod_if_direct <- function(route_coords, dep_elevation_ft, dest_elevation_ft,
                                      climb_gradient_nm_per_1000ft = 3,
                                      descent_gradient_nm_per_1000ft = 3) {
  if (nrow(route_coords) != 2) return(route_coords)

  dist_total <- route_coords$dist_nm[2] - route_coords$dist_nm[1]
  cruise_level_ft <- route_coords$level_ft[1]

  climb_nm <- climb_gradient_nm_per_1000ft * max(cruise_level_ft - dep_elevation_ft, 0) / 1000
  descent_nm <- descent_gradient_nm_per_1000ft * max(cruise_level_ft - dest_elevation_ft, 0) / 1000

  # rota curta demais para subida+descida completas: encolhe as duas na
  # mesma proporcao, deixando pelo menos 10% da distancia para cruzeiro
  if (climb_nm + descent_nm > dist_total * 0.9) {
    fator <- (dist_total * 0.9) / (climb_nm + descent_nm)
    climb_nm <- climb_nm * fator
    descent_nm <- descent_nm * fator
  }

  p1 <- c(route_coords$lon[1], route_coords$lat[1])
  p2 <- c(route_coords$lon[2], route_coords$lat[2])
  rumo <- bearing(p1, p2)

  toc_latlon <- destPoint(p1, rumo, climb_nm * 1852)
  tod_latlon <- destPoint(p1, rumo, (dist_total - descent_nm) * 1852)

  toc <- route_coords[1, ]
  toc$point <- "TOC"; toc$is_level_change <- FALSE
  toc$lat <- toc_latlon[2]; toc$lon <- toc_latlon[1]
  toc$dist_nm <- route_coords$dist_nm[1] + climb_nm

  tod <- route_coords[1, ]
  tod$point <- "TOD"; tod$is_level_change <- FALSE
  tod$lat <- tod_latlon[2]; tod$lon <- tod_latlon[1]
  tod$dist_nm <- route_coords$dist_nm[1] + dist_total - descent_nm

  combinado <- rbind(route_coords, toc, tod)
  combinado <- combinado[order(combinado$dist_nm), ]
  combinado$seq <- seq_len(nrow(combinado))
  rownames(combinado) <- NULL
  combinado
}

#' Marca quais pontos da rota planejada representam uma subida em degrau
#' (nivel maior que o do ponto anterior), e anexa as elevacoes de
#' partida/destino usadas como referencia para as rampas de subida/descida.
#'
#' @param route_coords rota com colunas seq, point, level_ft, dist_nm
#' @param dep_elevation_ft elevacao do aerodromo de partida (pes)
#' @param dest_elevation_ft elevacao do aerodromo de destino (pes)
#' @return route_coords com colunas: is_level_step, dep_elevation_ft,
#'   dest_elevation_ft (e TOC/TOD inseridos, se a rota era direta -- ver
#'   insert_toc_tod_if_direct())
build_planned_profile <- function(route_coords, dep_elevation_ft = 0,
                                   dest_elevation_ft = 0) {
  route_coords <- insert_toc_tod_if_direct(route_coords, dep_elevation_ft, dest_elevation_ft)
  n <- nrow(route_coords)
  route_coords$is_level_step <- c(FALSE, route_coords$level_ft[-1] > route_coords$level_ft[-n])
  route_coords$dep_elevation_ft <- dep_elevation_ft
  route_coords$dest_elevation_ft <- dest_elevation_ft
  route_coords
}

#' Interpola o nivel planejado (pes) para uma distancia arbitraria ao longo
#' da rota, e retorna tambem a fase de voo correspondente (ver detalhes da
#' metodologia no cabecalho deste arquivo).
#'
#' @param dist_nm vetor de distancias (NM) ao longo da rota
#' @param planned_profile rota retornada por build_planned_profile()
#' @param transition_nm largura (NM) da janela de transicao de subida em
#'   degrau, terminando no ponto de mudanca de nivel
#' @return data.frame com colunas dist_nm, planned_alt_ft, phase
interpolate_planned_profile <- function(dist_nm, planned_profile,
                                         transition_nm = default_transition_nm) {
  n <- nrow(planned_profile)
  dep_elev <- planned_profile$dep_elevation_ft[1]
  dest_elev <- planned_profile$dest_elevation_ft[1]

  out_alt <- numeric(length(dist_nm))
  out_phase <- character(length(dist_nm))

  ramp <- function(d, d0, d1, v0, v1) {
    frac <- (d - d0) / (d1 - d0)
    v0 + frac * (v1 - v0)
  }

  for (i in seq_along(dist_nm)) {
    d <- dist_nm[i]

    if (d <= planned_profile$dist_nm[1]) {
      out_alt[i] <- dep_elev
      out_phase[i] <- "SUBIDA"
      next
    }
    if (d >= planned_profile$dist_nm[n]) {
      out_alt[i] <- dest_elev
      out_phase[i] <- "DESCIDA"
      next
    }

    seg <- max(which(planned_profile$dist_nm <= d))
    seg_next <- seg + 1
    d0 <- planned_profile$dist_nm[seg]
    d1 <- planned_profile$dist_nm[seg_next]

    if (seg == 1) {
      # 1o trecho: subida continua idealizada da elevacao de partida ate o
      # nivel em vigor ao alcancar o proximo ponto
      out_alt[i] <- ramp(d, d0, d1, dep_elev, planned_profile$level_ft[seg_next])
      out_phase[i] <- "SUBIDA"
    } else if (seg_next == n) {
      # ultimo trecho: descida continua idealizada do ultimo nivel ate a
      # elevacao do destino
      out_alt[i] <- ramp(d, d0, d1, planned_profile$level_ft[seg], dest_elev)
      out_phase[i] <- "DESCIDA"
    } else if (planned_profile$is_level_step[seg_next]) {
      # trecho com subida em degrau: janela de transicao terminando no
      # ponto de mudanca de nivel
      transition_start <- max(d1 - transition_nm, d0)
      if (d < transition_start) {
        out_alt[i] <- planned_profile$level_ft[seg]
        out_phase[i] <- "CRUZEIRO"
      } else {
        out_alt[i] <- ramp(d, transition_start, d1,
                            planned_profile$level_ft[seg],
                            planned_profile$level_ft[seg_next])
        out_phase[i] <- "SUBIDA"
      }
    } else {
      out_alt[i] <- planned_profile$level_ft[seg]
      out_phase[i] <- "CRUZEIRO"
    }
  }

  data.frame(dist_nm = dist_nm, planned_alt_ft = out_alt, phase = out_phase)
}
