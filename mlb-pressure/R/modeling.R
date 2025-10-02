suppressPackageStartupMessages({ library(tidyverse); library(tidymodels); library(plm); library(broom) })

build_splits <- function(df) {
  df %>% mutate(pressure_situation = factor(pressure_situation), fastball = factor(fastball)) %>%
    initial_split(prop = 0.8, strata = pressure_situation)
}

logit_workflow <- function(train_df) {
  rec <- recipe(
    fastball ~ pressure_situation + ScoreDiff + inning + b_score + b_count + s_count + outs +
      pitch_num + on_1b + on_2b + on_3b + free_agent_status + gamesup + gamesback +
      temp + windspeed + attendance + p_home + pitch_count,
    data = train_df
  ) %>%
    step_impute_knn(all_predictors()) %>%
    step_dummy(all_nominal_predictors())
  
  mod <- logistic_reg() %>% set_engine("glm")
  workflow() %>% add_recipe(rec) %>% add_model(mod)
}

fit_plm <- function(df) {
  df_plm <- df %>%
    transmute(
      pitcher_id, year,
      fastball = as.numeric(fastball),
      pressure_situation,
      attendance = coalesce(attendance, 0),
      temp       = coalesce(temp, median(temp, na.rm = TRUE)),
      windspeed  = coalesce(windspeed, 0),
      free_agent_status = coalesce(as.numeric(free_agent_status), 0),
      pitch_num,
      pitch_count = coalesce(pitch_count, 0),
      p_home = coalesce(p_home, 0)
    ) %>%
    tidyr::drop_na(fastball, pressure_situation, pitch_num)
  
  plm(
    fastball ~ pressure_situation + attendance + temp + windspeed +
      free_agent_status + pitch_num + pitch_count + p_home,
    data = df_plm, index = "pitcher_id", model = "within", effect = "individual"
  )
}
