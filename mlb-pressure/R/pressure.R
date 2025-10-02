suppressPackageStartupMessages({ library(tidyverse); library(lubridate) })

make_free_agent_table <- function() {
  fa_dir <- raw_path("free_agents")
  files  <- list.files(fa_dir, pattern = "\\.xlsx$", full.names = TRUE)
  bind_rows(lapply(files, function(f) {
    yr <- readr::parse_number(basename(f))
    readxl::read_excel(f) %>% janitor::clean_names() %>%
      split_name_multilast(name_col = "name") %>%
      mutate(year = yr, free_agent_status = 1L)
  })) %>% distinct(pfirst_name, plast_name, year, free_agent_status)
}

make_standings_long <- function() {
  st_path <- raw_path("standings", "2015-2018 MLB Standings Data.xlsx")
  df      <- readxl::read_excel(st_path)
  months  <- c("may","june","july","august","september","october","lt5gb-sep15")
  month_lookup <- tibble(month_name = months, month = c(5,6,7,8,9,10,9))
  
  df %>% janitor::clean_names() %>%
    pivot_longer(-c(team, year), names_to = "temp", values_to = "value") %>%
    mutate(month_name = substr(temp, 3, nchar(temp)), variable = substr(temp, 1, 2)) %>%
    left_join(month_lookup, by = "month_name") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    rename_with(~ gsub("^gb", "gamesback", .x)) %>%
    rename_with(~ gsub("^gu", "gamesup", .x)) %>%
    filter(!(is.na(gamesup) & is.na(gamesback) & is.na(wc))) %>%
    group_by(team, year, month) %>%
    summarise(gamesup = na.omit(gamesup)[1], gamesback = na.omit(gamesback)[1], wc = na.omit(wc)[1], .groups = "drop")
}

make_pressure_flag <- function(df) {
  df %>% mutate(
    ScoreDiff = p_score - b_score,
    pressure_situation = dplyr::case_when(
      free_agent_status == 1 & month %in% c(8,9,10) ~ 1L,
      inning %in% c(8,9) & dplyr::between(ScoreDiff, -2, 2) & (on_2b == 1 | on_3b == 1) ~ 1L,
      (on_2b == 1 | on_3b == 1) & dplyr::between(ScoreDiff, -2, 2) & (gamesback <= 5 | gamesup <= 5 | wc == 1) & month %in% c(8,9,10) ~ 1L,
      month == 10 & (gamesback <= 2 | gamesup <= 2) & dplyr::between(ScoreDiff, -3, 3) ~ 1L,
      (on_1b == 1 & on_2b == 1 & on_3b == 1) & dplyr::between(ScoreDiff, -2, 4) ~ 1L,
      (on_2b == 1 | on_3b == 1) & ScoreDiff == 0 & inning %in% c(6,7,8,9) ~ 1L,
      ScoreDiff >= 1 & inning %in% c(8,9) ~ 1L,
      TRUE ~ 0L
    )
  )
}
