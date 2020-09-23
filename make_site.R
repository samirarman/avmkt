library(tidyverse)
library(zoo)
library(DT)
library(dygraphs)
library(xts)
library(RColorBrewer)

# Data processing -----------
download.file(
  "https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/Dados_Estatisticos.zip",
  destfile = "data.zip"
)

fares <- readRDS("fares_summary.rds")

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

# Custom plotting function for market comparison
make_market_graph <-
  function(variables,
           yearly = FALSE) {
    title <- "Série mensal"
    if (yearly) {
      title <- "Série anual"
    }

    data <- monthly_summaries %>%
      select(year_month, market, {
        {
          variables
        }
      }) %>%
      pivot_wider(
        id_cols = c(year_month, market),
        names_from = "market",
        values_from = {
          {
            variables
          }
        }
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

metrics <-
  list(
    pax = "pax",
    departures = "departures",
    rpk = "rpk",
    ask = "ask",
    load_factor = "load_factor",
    cargo = "cargo",
    rck = "rck"
  )

monthly_plots <-
  metrics %>%
  map(make_market_graph)

yearly_plots <-
  pmap(list(metrics, rep(TRUE, length(metrics))), make_market_graph)

companies_monthly_summaries <-
  data %>%
  select(year_month, company, market, rpk, rck) %>%
  group_by(year_month, company, market) %>%
  summarise(rpk = sum(rpk, na.rm = TRUE),
            rck = sum(rck, na.rm = TRUE)) %>%
  group_by(year_month, market) %>%
  mutate(
    mkt_rpk = sum(rpk, na.rm = TRUE),
    mkt_rck = sum(rck, na.rm = TRUE),
    pax_share = 100 * rpk / mkt_rpk,
    cargo_share = 100 * rck / mkt_rck
  )

# Finds the top n filtered by a variable
find_top <- function(market, variable, rank) {
  companies_monthly_summaries %>%
    filter(market == {
      {
        market
      }
    }) %>%
    group_by(company) %>%
    summarise(var  = sum({
      {
        variable
      }
    }, na.rm = TRUE)) %>%
    slice_max(var, n = rank) %>%
    pull(company)
}

top_companies <-
  list(
    dom_pax = find_top("DOMÉSTICA", rpk, 10),
    dom_cargo = find_top("INTERNACIONAL", rck, 10),
    intl_pax = find_top("DOMÉSTICA", rpk, 10),
    intl_cargo = find_top("INTERNACIONAL", rck, 10)
  ) %>%
  unlist %>%
  unique

# Custom plotting function for market-share
make_share_plot <- function(market, variable, yearly = FALSE) {
  title <-
    ifelse(market == "INTERNACIONAL", "Internacional", "Nacional")

  data <- companies_monthly_summaries %>%
    filter(market == {
      {
        market
      }
    } &
      company %in% top_companies) %>%
    select(year_month, company, {{variable}}) %>%
    pivot_wider(names_from = company, values_from = {{variable}})

  series <- as.xts(data[, -1], order.by = data$year_month)

  if (yearly) {
    series <- apply.yearly(series, mean)
  }

  series %>%
    dygraph(main = title, group = "market") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(12, "Paired")) %>%
    dyRangeSelector(height = 30)
}

# Market-share plots
share_plots <-
  expand.grid(
    market = c("DOMÉSTICA", "INTERNACIONAL"),
    variable = c("pax_share", "cargo_share"),
    yearly = TRUE
  ) %>%
  pmap(make_share_plot)

names(share_plots) <-
  c("dom_pax", "intl_pax", "dom_cargo", "intl_cargo")


company_dictionary <-
  unique(data[, c("company", "company_name")]) %>%
  arrange(company) %>%
  rename("Sigla ICAO" = company,
         "Nome da empresa" = company_name) %>%
  datatable()


# Custom plotting function for fare data
make_fare_plots <- function(variable, yearly = FALSE) {
  title <- "Série mensal"
  if (yearly) {
    title <- "Série anual"
  }

  data <-
    fares %>%
    select(year_month, company, {{variable}}) %>%
    pivot_wider(names_from = company, values_from = {{variable}})


  series <- xts(data[, -1], order.by = data$year_month)


  if (yearly) {
    if (variable == "mean_ticket") {
      series <- apply.yearly(series, mean)
    } else {
      series <- apply.yearly(series, colSums)
    }
  }

  series %>%
    dygraph %>%
    dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2")) %>%
    dyRangeSelector(height = 30)
}

variables <- list(yield = "yield",
                  seats = "seats",
                  mean_ticket = "mean_ticket")

fare_plots <-
  variables %>%
  map(make_fare_plots)

rmarkdown::render_site()
