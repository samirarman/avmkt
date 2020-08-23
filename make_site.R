library(tidyverse)
library(zoo)
library(DT)

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
  select(1:5, 18:20, 22, 25:26, 32)

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
  "rck"
)

data$year_month <- as.yearmon(paste0(data$year,
                                     "-",
                                     data$month))

make_summary <- function(data, var) {
  data %>%
    group_by(year_month = data[[var]]) %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    select(-c(year, month))
}



all_mkt_summary <-
  data %>%
  make_summary("year_month")

domestic_mkt_summary <-
  data %>%
  filter(market == "DOMÉSTICA") %>%
  make_summary("year_month")

intl_mkt_summary <-
  data %>%
  filter(market == "INTERNACIONAL") %>%
  make_summary("year_month")

summaries <-
  bind_rows(
    list(
      "Todos" = all_mkt_summary,
      "Doméstico" = domestic_mkt_summary,
      "Internacional" = intl_mkt_summary
    ),
    .id = "market"
  ) %>%
  mutate(load_factor = rpk / ask)

theme_set(
  theme_light() +
    theme(
      legend.position = "top",

      strip.background = element_rect(fill = "#2C3E50"),
      strip.text = element_text(color = "white",
                                face = "bold")
    )
)


base_plot <- function() {
  summaries %>%
    ggplot(aes(x = year_month)) +
    facet_wrap(facets = vars(market),
               nrow = length(levels(as.factor(summaries$market)))) +
    labs(x = "",
         color = "") +
    ylim(0, NA)
}

pax_graph <-
  base_plot() +
  geom_line(aes(y = pax / 1e6)) +
  labs(y = "Passageiros (em milhões)")

rpk_graph <-
  base_plot() +
  geom_line(aes(y = rpk / 1e9, color = "RPK")) +
  geom_line(aes(y = ask / 1e9, color = "ASK")) +
  scale_color_manual(values = c("gray70", "black")) +
  labs(y = "RPK (em bilhões de assentos-km)",
       colo = "Helo")

load_graph <-
  base_plot() +
  geom_line(aes(y = load_factor * 100)) +
  labs(y = "Load factor (em %)") +
  theme(legend.position = "none") +
  facet_wrap(facets = vars(market),
             nrow = 3) +
  ylim(0, 100)

cargo_graph <-
  base_plot() +
  geom_line(aes(y = cargo / 1e3)) +
  labs(y = "Carga paga (em toneladas)")

rtk_graph <-
  base_plot() +
  geom_line(aes(y = rck / 1e9)) +
  labs(y = "RTK (em milhões de toneladas-km)")

company_dictionary <-
  unique(data[,c("company", "company_name")]) %>%
  arrange(company) %>%
  rename("Sigla ICAO" = company,
        "Nome da empresa" = company_name) %>%
  datatable()


rmarkdown::render_site()
