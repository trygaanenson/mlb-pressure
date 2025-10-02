suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(janitor)
})

# Paths
proj_path <- function(...) file.path(getwd(), ...)
raw_path  <- function(...) proj_path("data", "raw", ...)
out_path  <- function(...) proj_path("outputs", ...)

dir_create <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

safe_read_csv <- function(path) {
  message("Reading ", path)
  readr::read_csv(path, show_col_types = FALSE) %>% janitor::clean_names()
}

# Weather/wind parsing (robust)
parse_weather <- function(df) {
  df %>%
    tidyr::separate(weather, into = c("temp","clouds"), sep = ",\\s+", extra = "merge", fill = "right", remove = FALSE) %>%
    tidyr::separate(wind,    into = c("windspeed","winddirection"), sep = ",\\s+", extra = "merge", fill = "right", remove = FALSE) %>%
    mutate(windspeed = readr::parse_number(windspeed), temp = readr::parse_number(temp))
}

# Fastball flag
flag_fastball <- function(x) as.integer(x %in% c("FC","FF","FT","SI"))

# Pitch count within pitcher+game
add_pitch_count <- function(df) {
  df %>% arrange(pitcher_id, g_id, pitch_num) %>% group_by(pitcher_id, g_id) %>%
    mutate(pitch_count = row_number()) %>% ungroup()
}

# ---- Name cleaning for FA ----
split_name_multilast <- function(df, name_col = "Name") {
  stopifnot(name_col %in% names(df))
  suffix_rx <- "(Jr\\.?|Sr\\.?|II|III|IV|V)$"
  df %>%
    mutate(
      .name_raw   = .data[[name_col]] %>% as.character(),
      .name_clean = .name_raw %>%
        str_replace_all("[\\u00A0]", " ") %>%
        str_squish() %>%
        str_remove(regex(paste0(",?\\s*", suffix_rx), ignore_case = TRUE)) %>%
        str_replace_all("\\s*,\\s*", ", ") %>%
        str_remove("^,\\s*|\\s*,$") %>%
        stringi::stri_trans_general("Latin-ASCII"),
      has_comma   = str_detect(.name_clean, ",")
    ) %>%
    mutate(
      .name_fl = if_else(
        has_comma,
        { parts <- str_split_fixed(.name_clean, ",", 2); paste(str_squish(parts[,2]), str_squish(parts[,1])) },
        .name_clean
      ) %>% str_squish()
    ) %>%
    tidyr::separate_wider_delim(
      .name_fl, delim = " ",
      names = c("pfirst_name", "plast_name"),
      too_few = "align_start", too_many = "merge"
    )
}

# ---- Last-k helpers (your additions) ----
lastk_sum  <- function(x, k = 5, min_k = 1) {
  mats <- lapply(1:k, function(i) dplyr::lag(x, i)); M <- do.call(cbind, mats)
  n <- rowSums(!is.na(M)); out <- rowSums(M, na.rm = TRUE); out[n < min_k] <- NA_real_; out
}
lastk_mean <- function(x, k = 5, min_k = 1) {
  mats <- lapply(1:k, function(i) dplyr::lag(x, i)); M <- do.call(cbind, mats)
  n <- rowSums(!is.na(M)); out <- rowMeans(M, na.rm = TRUE); out[n < min_k] <- NA_real_; out
}
lastk_used <- function(x, k = 5) { mats <- lapply(1:k, function(i) dplyr::lag(x, i)); rowSums(!is.na(do.call(cbind, mats))) }

smooth_lastk <- function(ind, k = 5, alpha = 1, beta = 1) {
  mats <- lapply(1:k, function(i) dplyr::lag(ind, i)); M <- do.call(cbind, mats)
  cnt <- rowSums(M, na.rm = TRUE); n_used <- rowSums(!is.na(M)); (cnt + alpha) / (n_used + alpha + beta)
}
