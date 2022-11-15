#----------
# title: page metrics
# author: Maria Granell Ruiz
# date: 15 nov 2022
# ---------

# packages -------------
library(tidyverse)
library(jsonlite)

# ally-scores -------------------------------------------------------------

get_df <- function(url){
  raw_list <- fromJSON(url, simplifyVector = FALSE)

  tibble(data = url) |>
    unnest_wider(data) |>
    mutate(
      measure = fs::path_file(url) |> tools::file_path_sans_ext(),
      .before = 1)
}

ally_scores_url <- "https://cdn.httparchive.org/reports/a11yScores.json"
color_contrast_url <- "https://cdn.httparchive.org/reports/a11yColorContrast.json"
image_alt_url <- "https://cdn.httparchive.org/reports/a11yImageAlt.json"


ally_scores <- get_df(ally_scores_url)
color_contrast <- get_df(color_contrast_url)
image_alt <- get_df(image_alt_url)

ally_scores |> write_csv("2022/2022-11-15/ally_scores.csv")
color_contrast |> write_csv("2022/2022-11-15/color_contrast.csv")
image_alt |> write_csv("2022/2022-11-15/image_alt.csv")

# speed-size --------------------------------------------------------------

bytes_total_url <- "https://cdn.httparchive.org/reports/bytesTotal.json"
speed_index_url <- "https://cdn.httparchive.org/reports/speedIndex.json"

bytes_total <- get_df(bytes_total_url)
speed_index <- get_df(speed_index_url)

bytes_total |> write_csv("2022/2022-11-15/bytes_total.csv")
speed_index |> write_csv("2022/2022-11-15/speed_index.csv")