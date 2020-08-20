library(tidyverse)

download.file("https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/Dados_Estatisticos.zip",
              destfile = "data.zip")

rmarkdown::render_site()
