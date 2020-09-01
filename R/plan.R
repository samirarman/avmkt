# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  traffic_raw_data = download.file(
    file_in(
    "https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/Dados_Estatisticos.zip"),
    destfile = "data.zip"
  ),
  clean_traffic_data = clean_data(raw_data),
  market_plots <- make_market_plots(clean_traffic_data),
  code_tables <- make_tables(clean_traffic_data),
  site = rmarkdown::render_site()
)
