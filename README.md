# fpladherence

Metodologia e implementação (R) para medir a **aderência ao plano de voo
(FPL)**, comparando o que foi arquivado (FPL) com o que foi realmente voado
(RADAR). Primeira fase: **aderência vertical** (perfil de altitude). A
aderência horizontal (rota lateral) é a próxima fase, reaproveitando a mesma
base de geometria de rota.

Extensão do trabalho de eficiência horizontal de voo (*Horizontal Flight
Efficiency*), adicionando o FPL como referência de plano e uma metodologia
própria de aderência vertical.

## Estrutura

```
R/                          funções da metodologia
  parse_fpl.R                parser de FPL em texto ICAO cru (campo 15)
  parse_sigma_fpl.R           adaptador para o export real do SIGMA/DECEA (FPL)
  parse_radar.R               leitor de radar em formato canônico (CSV simples)
  parse_sigma_radar.R         adaptador para o export real do SIGMA/DECEA (RADAR)
  route_geometry.R            distância acumulada e projeção do radar sobre a rota
  vertical_profile.R          perfil vertical planejado (subida/cruzeiro/descida) -- exige navdata
  vertical_adherence.R        aderência vertical (com navdata/rota resolvida)
  vertical_adherence_radar_only.R  aderência vertical SEM navdata (fase pela taxa de subida/descida do radar)
  horizontal_efficiency.R     aderência/eficiência horizontal (distância voada x direta ADEP-ADES)
  plot_vertical.R              visualização vertical (planejado x realizado)
  plot_horizontal.R            visualização lateral (rota FPL x trajetória RADAR)

data/                        dados sintéticos de exemplo (comitados)
data/airports_br.csv          ~2877 aeródromos brasileiros (icao, lat, lon, elevação)
data/waypoints_br.csv         ~7932 fixos/waypoints reais, fonte oficial AISWEB/DECEA
data/waypoint_aisweb.xlsx     planilha original da AISWEB (fonte de waypoints_br.csv)
data/local/                  dados reais (NÃO comitado -- ver .gitignore)
data-raw/generate_sample_data.R  gera os dados sintéticos de data/
scripts/explore_real_flight.R    script pronto para testar com um voo real (FPL + RADAR do SIGMA)
tests/                       testes de fumaça (fixtures sintéticas)
analysis/vertical_adherence.qmd  documento com a metodologia completa + exemplo
```

## Como rodar

```r
# dados sintéticos (já comitados em data/)
source("R/parse_fpl.R"); source("R/route_geometry.R")
source("R/vertical_profile.R"); source("R/vertical_adherence.R")
source("R/plot_vertical.R")

fpl <- parse_fpl(read_fpl_file("data/sample_fpl.txt"))
waypoints_db <- read.csv("data/waypoints.csv")
route_coords <- add_cumulative_distance(resolve_route_coords(fpl$route, waypoints_db))
planned <- build_planned_profile(route_coords, dep_elevation_ft = 2459, dest_elevation_ft = 11)

radar <- project_radar_onto_route(read_radar_track("data/sample_radar.csv"), route_coords)
matched <- compute_vertical_deviation(radar, planned)

summarise_vertical_adherence(matched)
plot_vertical_adherence(matched)
```

Testes: `Rscript tests/test_vertical_adherence.R`

Documento completo (metodologia + exemplo executável):
`quarto render analysis/vertical_adherence.qmd`

## Dados reais (SIGMA/DECEA)

Os arquivos reais de FPL (`sigma_flight_plan_*.csv`) e RADAR
(`radar_*.csv`) não são versionados (grandes e operacionais). Mantenha-os
localmente (ex.: `data/local/`, já no `.gitignore`) e use os adaptadores
`R/parse_sigma_fpl.R` e `R/parse_sigma_radar.R`, descritos na seção 6 de
`analysis/vertical_adherence.qmd`.

## Status

- [x] Aderência vertical: metodologia + implementação + exemplo sintético
- [x] Adaptador para FPL real (SIGMA/DECEA)
- [x] Adaptador para RADAR real (SIGMA/DECEA)
- [x] Aderência vertical sem navdata (fase pela taxa de subida/descida) -- testado com voo real
- [x] Visualização/eficiência horizontal (distância voada x direta ADEP-ADES)
- [x] Base oficial de waypoints (AISWEB/DECEA) -- resolução de rota completa com fixos reais
- [ ] Validação em escala com dados reais de um dia inteiro
- [ ] Aderência horizontal completa (desvio lateral da rota, não só distância)
