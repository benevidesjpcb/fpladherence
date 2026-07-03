# Parser para mensagens de Plano de Voo ICAO (FPL)
#
# Extrai do campo 15 (rota) a sequencia de pontos com velocidade/nivel de voo
# planejados, que serao usados para construir o perfil vertical planejado.

#' Le um arquivo de FPL ICAO e retorna o texto bruto
read_fpl_file <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = " ")
}

#' Extrai um campo do FPL a partir do prefixo "-" (ex: campo 15, 16)
#' O FPL usa "-" para separar campos dentro do corpo da mensagem.
extract_fpl_fields <- function(fpl_text) {
  body <- gsub("^\\(FPL-|\\)$", "", trimws(fpl_text))
  # remove quebras de linha e espacos duplicados
  body <- gsub("\\s+", " ", body)
  fields <- strsplit(body, "-")[[1]]
  fields <- trimws(fields)
  fields[fields != ""]
}

#' Converte token de nivel ("F370", "S1130", "A045", "M082") para pes (ft)
#' F = flight level (x100 ft), A = altitude em centenas de pes (QNH),
#' S/M = niveis metricos, tratados como flight level equivalente para fins
#' deste protótipo (poderia ser convertido para pes com fator 3.28084).
level_token_to_ft <- function(level_token) {
  if (is.na(level_token) || level_token == "") return(NA_real_)
  code <- substr(level_token, 1, 1)
  value <- suppressWarnings(as.numeric(substr(level_token, 2, nchar(level_token))))
  if (is.na(value)) return(NA_real_)
  if (code == "F") return(value * 100)
  if (code == "A") return(value * 100)
  if (code %in% c("S", "M")) return(round(value * 3.28084))
  NA_real_
}

#' Parseia um token de rota (campo 15) que pode ser:
#'  - velocidade+nivel inicial: "N0450F370"
#'  - "DCT" (rota direta, sem aerovia)
#'  - designador de aerovia: "UL725"
#'  - ponto: "PIRUX"
#'  - ponto com mudanca de veloc/nivel: "PIRUX/N0450F370"
parse_route_token <- function(token) {
  if (grepl("/", token)) {
    parts <- strsplit(token, "/")[[1]]
    point <- parts[1]
    spd_lvl <- parts[2]
    level_token <- sub("^N?[0-9]*", "", spd_lvl) # remove prefixo de velocidade
    list(point = point, level_token = level_token, is_change = TRUE)
  } else {
    list(point = NA_character_, level_token = NA_character_, is_change = FALSE)
  }
}

#' Parseia a string de rota do campo 15 e retorna um tibble com a sequencia
#' de pontos e o nivel planejado (em pes) vigente a partir de cada ponto.
#'
#' @param route_string string do campo 15, ex:
#'   "N0450F370 DCT PONTA/N0450F370 DCT PIRUX/N0450F370 DCT UZUKA/N0450F390 DCT"
#' @return tibble com colunas: seq, point, level_ft, is_level_change
parse_fpl_route <- function(route_string) {
  tokens <- strsplit(trimws(route_string), "\\s+")[[1]]
  tokens <- tokens[tokens != ""]

  first_level_token <- sub("^N?[0-9]*", "", tokens[1])
  current_level_ft <- level_token_to_ft(first_level_token)

  rows <- list()
  seq_i <- 1
  rows[[seq_i]] <- data.frame(
    seq = seq_i, point = "DEP", level_ft = current_level_ft,
    is_level_change = TRUE, stringsAsFactors = FALSE
  )

  for (token in tokens[-1]) {
    if (token %in% c("DCT") || grepl("^[A-Z][0-9]+$", token)) next # DCT / aerovia: ignora
    parsed <- parse_route_token(token)
    if (!is.na(parsed$point)) {
      seq_i <- seq_i + 1
      if (parsed$is_change && nchar(parsed$level_token) > 0) {
        lvl <- level_token_to_ft(parsed$level_token)
        if (!is.na(lvl)) current_level_ft <- lvl
      }
      rows[[seq_i]] <- data.frame(
        seq = seq_i, point = parsed$point, level_ft = current_level_ft,
        is_level_change = parsed$is_change, stringsAsFactors = FALSE
      )
    } else {
      # ponto puro sem "/", ex.: "PONTA"
      seq_i <- seq_i + 1
      rows[[seq_i]] <- data.frame(
        seq = seq_i, point = token, level_ft = current_level_ft,
        is_level_change = FALSE, stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

#' Parseia a mensagem FPL completa e retorna metadados + rota planejada
#'
#' @param fpl_text texto bruto da mensagem FPL (formato ICAO)
#' @return list(callsign, dep_icao, dest_icao, route = tibble)
parse_fpl <- function(fpl_text) {
  fields <- extract_fpl_fields(fpl_text)

  callsign <- fields[1]
  dep_field <- fields[grepl("^[A-Z]{4}[0-9]{4}$", fields)][1]
  dep_icao <- substr(dep_field, 1, 4)
  eobt <- substr(dep_field, 5, 8)

  route_field <- fields[grepl("^N[0-9]{4}F", fields)][1]

  dest_field <- fields[grepl("^[A-Z]{4}[0-9]{4} ", fields)][1]
  dest_icao <- substr(dest_field, 1, 4)

  route <- parse_fpl_route(route_field)
  route$point[1] <- dep_icao

  # o campo 15 (rota) descreve apenas o trajeto ate o ultimo ponto
  # significativo antes do destino -- o aerodromo de destino (campo 16) e
  # sempre o ponto final da rota voada e precisa ser anexado explicitamente,
  # senao o ultimo trecho (chegada/descida) fica sem ponto de chegada.
  last_level_ft <- route$level_ft[nrow(route)]
  route <- rbind(route, data.frame(
    seq = nrow(route) + 1, point = dest_icao, level_ft = last_level_ft,
    is_level_change = FALSE, stringsAsFactors = FALSE
  ))

  list(
    callsign = callsign,
    dep_icao = dep_icao,
    eobt = eobt,
    dest_icao = dest_icao,
    route = route
  )
}
