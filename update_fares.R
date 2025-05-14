# Summarises the ticket sales data from locally
# saved files to build the dataset for the fares in the site

library(tidyverse)
library(zoo)
library(readr)
library(data.table)

summarise_data <- function(data) {
  data %>%
    mutate(yield = fare * seats,
           year_month = as.yearmon(paste0(year, "-", month))) %>%
    group_by(year_month, company) %>%
    summarise(
      yield = sum(yield),
      seats = sum(seats),
      mean_ticket = yield / seats,
      .groups = "keep"
    ) %>%
    mutate(yield = yield / 1e6,
           seats = seats / 1e3)
}

read_custom <- function(file) {

  print(paste0("Processing file ", file))

  # fread takes care of different csv separators used
  # ANAC files
  data <- fread(file = file)
  data <- as_tibble(data)


  if (ncol(data) == 8) {
    data[[1]] = NULL
  }

  names(data) <- c("year", "month", "company", "dep", "arr", "fare", "seats")

  if (class(data$fare) == "character") {
    fares <- gsub("\\,",".", data$fare)
    # print(fares[1:10])
    data$fare <- as.numeric(fares)
  }

  data <- summarise_data(data)
  data
}

files <- list.files("./fares_data/", full.names = T)
df <- map_dfr(files, read_custom)
df

# Checks data quality
ggplot(df, aes(x = year_month, y = mean_ticket, color = company, group = company)) + geom_line()

df %>%
  saveRDS("fares_summary.rds")

