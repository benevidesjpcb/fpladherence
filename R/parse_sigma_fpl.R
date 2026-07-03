# Adaptador para o formato REAL de exportacao de plano de voo do DECEA/SIGMA
# (ex.: sigma_flight_plan_2025_12_10.csv), como alternativa ao parser de
# texto ICAO cru em R/parse_fpl.R.
#
# Este arquivo e um LOG DE MENSAGENS (uma linha por evento), nao um plano por
# voo. Colunas relevantes observadas na amostra real (separador ";"):
#   id, gufi, type, msg_id, msg_type, receipt_cais, receipt_application,
#   msg_payload, indicative, adep, ades, eobd, eobt, atod, atot, airline,
#   ssr, aircraft_model, equip, speed, lvl, route, other_info, ...,
#   day_of_week, state, ..., flight_rule, ats_type, id_tmpsq
#
# IMPORTANTE: 'type' NAO e o discriminador de evento (na amostra observada,
# vem sempre "RPL", provavelmente a classificacao ICAO do plano). Quem
# discrimina o evento e 'msg_type': CHG (plano arquivado/alterado), DEP
# (confirmacao real de decolagem), ARR (confirmacao real de pouso).
#
# ATENCAO -- suposicoes inferidas de uma amostra pequena (3 mensagens de 1
# unico voo), a validar com mais dados reais:
#  - 'eobd'/'atod' parecem carregar a DATA (hora zerada) e 'eobt'/'atot'
#    carregam apenas a HORA (com uma data-placeholder fixa, ex. 1980-02-01).
#    Por isso combinamos data+hora de colunas diferentes.
#  - 'atot' e um atributo FIXO DO VOO (hora real de decolagem), e NAO muda
#    entre as linhas CHG/DEP/ARR de um mesmo gufi -- ou seja, nao serve para
#    obter a hora real de POUSO. Para o horario real de pouso, extraimos os
#    4 digitos finais (HHMM) do 'msg_payload' da mensagem ARR (ex.:
#    "(ARRSBSV/SBRJ365-GLO2036-SBGL-SBSV0152)" -> pousou as 01:52).
#  - o campo 'route' nesta amostra nao trouxe anotacao de mudanca de nivel
#    por ponto (formato "PONTO/N0000F000"), diferente do que o campo 15 do
#    FPL ICAO cru permite. Por isso o perfil planejado extraido daqui e
#    tratado como NIVEL DE CRUZEIRO UNICO (sem degraus), o que tambem reflete
#    melhor a realidade: mudancas de nivel em rota sao autorizacao do ATC,
#    nao parte do plano arquivado.

#' Le o log de mensagens de FPL exportado do SIGMA
#'
#' @param path caminho do CSV (separador ";")
#' @return data.frame com as colunas do arquivo original, mais as datas/horas
#'   já convertidas para POSIXct (event_date, event_time_of_day nao expostos
#'   individualmente -- ver combine_sigma_datetime())
read_sigma_fpl_log <- function(path) {
  log <- read.csv(path, sep = ";", stringsAsFactors = FALSE,
                   fileEncoding = "UTF-8", check.names = TRUE)
  log
}

#' Combina a data de uma coluna com a hora-do-dia de outra, reconstruindo um
#' timestamp completo. Usado porque o export do SIGMA separa data e hora em
#' colunas distintas (ver nota no cabecalho deste arquivo).
combine_sigma_datetime <- function(date_col, time_col) {
  date_part <- as.Date(date_col)
  time_part <- format(as.POSIXct(time_col, tz = "UTC"), "%H:%M:%S")
  as.POSIXct(paste(date_part, time_part), tz = "UTC")
}

#' Seleciona, para cada voo (gufi), a versao mais recente do plano arquivado
#' (msg_type RPL* ou CHG) -- ou seja, o plano efetivamente em vigor.
#'
#' @param sigma_log data.frame retornado por read_sigma_fpl_log()
#' @return data.frame com uma linha por gufi: gufi, indicative, adep, ades,
#'   speed, lvl, route, eobt_full (POSIXct), aircraft_model
select_filed_plan <- function(sigma_log) {
  filed <- sigma_log[sigma_log$msg_type %in% c("FPL", "RPL", "CHG"), ]
  filed$receipt_application <- as.POSIXct(filed$receipt_application, tz = "UTC")

  filed <- filed[order(filed$gufi, filed$receipt_application), ]
  latest <- filed[!duplicated(filed$gufi, fromLast = TRUE), ]

  latest$eobt_full <- combine_sigma_datetime(latest$eobd, latest$eobt)

  latest[, c("gufi", "indicative", "adep", "ades", "speed", "lvl", "route",
             "aircraft_model", "ssr", "eobt_full")]
}

#' Extrai os horarios reais de decolagem (DEP) e pouso (ARR) por voo, quando
#' disponiveis no log (mensagens confirmadas pelo ATC).
#'
#' A hora real de decolagem vem das colunas atod/atot (fixas por voo). A
#' hora real de pouso NAO esta em nenhuma coluna estruturada nesta amostra --
#' e extraida do texto de 'msg_payload' da mensagem ARR (4 digitos finais no
#' formato HHMM, ex. "...SBSV0152)" -> 01:52). Ver ressalva no cabecalho.
#'
#' @param sigma_log data.frame retornado por read_sigma_fpl_log()
#' @return data.frame com colunas: gufi, actual_dep (POSIXct ou NA),
#'   actual_arr (POSIXct ou NA)
extract_actual_times <- function(sigma_log) {
  dep_rows <- sigma_log[sigma_log$msg_type == "DEP", ]
  dep_rows$actual_dep <- combine_sigma_datetime(dep_rows$atod, dep_rows$atot)

  arr_rows <- sigma_log[sigma_log$msg_type == "ARR", ]
  hhmm <- sub(".*[A-Z]{4}([0-9]{4}).*", "\\1", arr_rows$msg_payload)
  arr_date <- as.Date(arr_rows$atod)
  arr_rows$actual_arr <- as.POSIXct(
    paste0(arr_date, " ", substr(hhmm, 1, 2), ":", substr(hhmm, 3, 4), ":00"),
    tz = "UTC"
  )

  merge(dep_rows[, c("gufi", "actual_dep")],
        arr_rows[, c("gufi", "actual_arr")], by = "gufi", all = TRUE)
}

#' Converte o nivel filed (token tipo "F230") para pes -- reaproveita
#' level_token_to_ft() de R/parse_fpl.R
#'
#' Constroi a rota planejada de um voo no MESMO FORMATO produzido por
#' parse_fpl_route() (colunas seq, point, level_ft, is_level_change), a
#' partir dos campos separados do SIGMA (adep/route/ades/lvl). Como o campo
#' 'route' aqui nao anota mudancas de nivel por ponto, o nivel e constante
#' (nivel de cruzeiro filed) ao longo de toda a rota -- ver ressalva no
#' cabecalho deste arquivo.
#'
#' @param flight_plan uma linha de select_filed_plan()
#' @return data.frame no formato de rota usado pelo restante do pipeline
#'   (route_geometry.R, vertical_profile.R, vertical_adherence.R)
sigma_route_to_route_df <- function(flight_plan) {
  level_ft <- level_token_to_ft(flight_plan$lvl)

  tokens <- strsplit(trimws(flight_plan$route), "\\s+")[[1]]
  is_airway <- grepl("^[A-Z]{1,3}[0-9]+$", tokens)
  points <- tokens[tokens != "DCT" & !is_airway & tokens != ""]
  points <- c(flight_plan$adep, points, flight_plan$ades)

  data.frame(
    seq = seq_along(points), point = points, level_ft = level_ft,
    is_level_change = FALSE, stringsAsFactors = FALSE
  )
}
