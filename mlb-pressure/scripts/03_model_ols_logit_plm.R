suppressPackageStartupMessages({ library(tidyverse); library(tidymodels); library(plm); library(broom) })
source("R/utils.R"); source("R/modeling.R")

FAST_MODE <- FALSE
N_SAMPLES <- 150000
CV_FOLDS  <- 5

full <- readr::read_csv(out_path("derived/fellowship_training_clean.csv"), show_col_types = FALSE)
full <- full %>% mutate(
  fastball = as.numeric(fastball),
  pressure_situation = as.numeric(pressure_situation),
  free_agent_status = coalesce(as.numeric(free_agent_status), 0),
  attendance = coalesce(attendance, 0),
  temp = coalesce(temp, median(temp, na.rm = TRUE)),
  windspeed = coalesce(windspeed, 0),
  gamesup = coalesce(gamesup, 0),
  gamesback = coalesce(gamesback, 0),
  p_home = coalesce(p_home, 0),
  pitch_count = coalesce(pitch_count, 0)
)

if (FAST_MODE) {
  set.seed(123); full <- dplyr::sample_n(full, size = min(N_SAMPLES, nrow(full)))
  message("FAST_MODE ON — using ", nrow(full), " rows")
} else {
  message("FAST_MODE OFF — using all ", nrow(full), " rows")
}

# Align with original variable name
if (!"ScoreDiff" %in% names(full) && "score_diff" %in% names(full)) full <- full %>% mutate(ScoreDiff = score_diff)

# Original sequence names preserved
lm.1  <- lm(fastball ~ pressure_situation, data = full); print(summary(lm.1))
lm.1a <- glm(fastball ~ pressure_situation, family = binomial("logit"), data = full); print(summary(lm.1a))

lm.2 <- lm(fastball ~ pressure_situation + ScoreDiff + inning + b_score + b_count + s_count + outs +
             pitch_num + on_1b + on_2b + on_3b + free_agent_status, data = full); print(summary(lm.2))

lm.3 <- lm(fastball ~ pressure_situation * free_agent_status, data = full); print(summary(lm.3))

lm.3a <- glm(fastball ~ pressure_situation * free_agent_status, family = binomial("logit"), data = full); print(summary(lm.3a))

plm_fit <- fit_plm(full); print(summary(plm_fit))

# Lightweight tidymodels (no leakage: exclude pitch_type/type/event)
split <- build_splits(full)
train <- training(split); test <- testing(split)
wf <- logit_workflow(train)
rs <- fit_resamples(wf, vfold_cv(train, v = CV_FOLDS)); print(collect_metrics(rs))
final <- last_fit(wf, split); print(collect_metrics(final))
readr::write_csv(collect_predictions(final), out_path("derived/predictions_logit.csv"))
