suppressPackageStartupMessages({ library(tidyverse); library(broom) })
source("R/utils.R")

full <- readr::read_csv(out_path("derived/fellowship_training_clean.csv"), show_col_types = FALSE)

pids <- unique(full$pitcher_id)
res <- vector("list", length(pids))
flag <- pressure_coef <- rep(NA_real_, length(pids))

for (i in seq_along(pids)) {
  d <- dplyr::filter(full, pitcher_id == pids[i])
  if (length(unique(d$pressure_situation)) > 1) {
    m <- lm(fastball ~ pressure_situation + attendance + temp + windspeed + free_agent_status +
              pitch_num + pitch_count + p_home + pressure_situation:free_agent_status, data = d)
    res[[i]] <- broom::tidy(m)
    if ("pressure_situation" %in% res[[i]]$term) {
      row <- res[[i]] %>% filter(term == "pressure_situation")
      pressure_coef[i] <- row$estimate[1]
      flag[i] <- as.numeric(row$p.value[1] < 0.05)
    } else { flag[i] <- 0 }
  } else { flag[i] <- 0 }
}

summary_tbl <- tibble(pitcher_id = pids, Pressure_Situation_Coefficient = pressure_coef, Significant_Effect = flag) %>%
  left_join(player_names, by = c("pitcher_id" = "id"))
readr::write_csv(summary_tbl, out_path("derived/per_pitcher_pressure_coeffs.csv"))
print(summary_tbl %>% summarise(total_pitchers = n(), sig_pitchers = sum(Significant_Effect, na.rm = TRUE), share = sig_pitchers/total_pitchers))

