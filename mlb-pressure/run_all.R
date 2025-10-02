source("scripts/01_load_data.R")
source("scripts/02_feature_engineering.R")
source("scripts/03_model_ols_logit_plm.R")
source("scripts/04_per_pitcher_loops.R")

# Export a small figure (for Squarespace)
source("R/utils.R")
full <- readr::read_csv(out_path("derived/fellowship_training_clean.csv"), show_col_types = FALSE)

p1 <- full %>%
  count(pressure_situation, name = "pitches") %>%
  ggplot2::ggplot(ggplot2::aes(pressure_situation, pitches)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "Pressure Situation", y = "Pitches", title = "Pitch Count by Pressure Flag")

ggplot2::ggsave(out_path("figures/pitch_count_by_pressure.png"), p1, width = 7, height = 4, dpi = 300)
svglite::svglite(out_path("figures/pitch_count_by_pressure.svg"), width = 7, height = 4); print(p1); grDevices::dev.off()
