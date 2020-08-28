library(tidyverse)
library(zoo)
library(DT)
library(dygraphs)
library(xts)

# Data processing -----------
download.file(
  "https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/Dados_Estatisticos.zip",
  destfile = "data.zip"
)

raw_data <- read_delim(
  "data.zip",
  ";",
  escape_double = FALSE,
  locale = locale(
    date_names = "pt",
    decimal_mark = ",",
    grouping_mark = ".",
    encoding = "WINDOWS-1252"
  ),
  trim_ws = TRUE,
  col_types = cols(
    .default = col_double(),
    `EMPRESA (SIGLA)` = col_character(),
    `EMPRESA (NOME)` = col_character(),
    `EMPRESA (NACIONALIDADE)` = col_character(),
    `AEROPORTO DE ORIGEM (SIGLA)` = col_character(),
    `AEROPORTO DE ORIGEM (NOME)` = col_character(),
    `AEROPORTO DE ORIGEM (UF)` = col_character(),
    `AEROPORTO DE ORIGEM (REGIÃO)` = col_character(),
    `AEROPORTO DE ORIGEM (PAÍS)` = col_character(),
    `AEROPORTO DE ORIGEM (CONTINENTE)` = col_character(),
    `AEROPORTO DE DESTINO (SIGLA)` = col_character(),
    `AEROPORTO DE DESTINO (NOME)` = col_character(),
    `AEROPORTO DE DESTINO (UF)` = col_character(),
    `AEROPORTO DE DESTINO (REGIÃO)` = col_character(),
    `AEROPORTO DE DESTINO (PAÍS)` = col_character(),
    `AEROPORTO DE DESTINO (CONTINENTE)` = col_character(),
    NATUREZA = col_character(),
    `GRUPO DE VOO` = col_character()
  )
)

data <-
  raw_data %>%
  select(1:5, 18:20, 22, 25:26, 31:32)

names(data) <- c(
  "company",
  "company_name",
  "company_nationality",
  "year",
  "month",
  "market",
  "group",
  "pax",
  "cargo",
  "ask",
  "rpk",
  "departures",
  "rck"
)

data$year_month <- as.yearmon(paste0(data$year,
                                     "-",
                                     data$month))

agg_if_numeric <- function(data) {
  data %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
}

monthly_summaries <-
  data %>%
  group_by(year_month, market) %>%
  agg_if_numeric() %>%
  mutate(market = ifelse(market == "DOMÉSTICA", "Doméstico", "Internacional")) %>%
  bind_rows(data %>%
              group_by(year_month) %>%
              agg_if_numeric() %>%
              mutate(market = "Todos")) %>%
  select(-c(year, month)) %>%
  mutate(
    pax = pax / 1e6,
    cargo = cargo / 1e3,
    ask = ask / 1e9,
    rpk = rpk / 1e9,
    rck = rck / 1e9,
    load_factor = rpk / ask * 100
  )


# Index page -------------------

# Custom plotting function for market comparison
make_market_graph <-
  function(variables,
           yearly = FALSE) {
    
    title <- "Série mensal"
    if (yearly) {
      title <- "Série anual"
    }
    
    data <- monthly_summaries %>%
      select(year_month, market, {{variables}}) %>%
      pivot_wider(
        id_cols = c(year_month, market),
        names_from = "market",
        values_from = {{variables}}
      )
    
    series <-
      as.xts(data[, 2:4], order.by = data$year_month)
    
    if (yearly) {
      if (variables == "load_factor") {
        series <- apply.yearly(series, mean)
      } else {
        series <- apply.yearly(series, colSums)
      }
    }
    
    series %>%
      dygraph(main = title, group = "market") %>%
      dyOptions(colors = c("#F39C12", "#E74C3C", "#2C3E50")) %>%
      dyRangeSelector(height = 30)
  }



pax_monthly <-
  make_market_graph("pax")

deps_monthly <-
  make_market_graph("departures")

rpk_monthly <-
  make_market_graph("rpk")

load_monthly <-
  make_market_graph("load_factor")

cargo_monthly <-
  make_market_graph("cargo")

rck_monthly <-
  make_market_graph("rck")

# Market yearly ---------------------------------


pax_yearly <-
  make_market_graph("pax", TRUE)

deps_yearly <-
  make_market_graph("departures", TRUE)

rpk_yearly <-
  make_market_graph("rpk", TRUE)

load_yearly <-
  make_market_graph("load_factor", TRUE)

cargo_yearly <-
  make_market_graph("cargo", TRUE)

rck_yearly <-
  make_market_graph("rck", TRUE)

# Company dictionary page -----------------------

company_dictionary <-
  unique(data[, c("company", "company_name")]) %>%
  arrange(company) %>%
  rename("Sigla ICAO" = company,
         "Nome da empresa" = company_name) %>%
  datatable()


rmarkdown::render_site()
