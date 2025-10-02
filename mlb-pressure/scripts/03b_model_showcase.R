# scripts/03b_models_showcase.R
# Runs several themed regressions + one kitchen-sink model.
# Works with your canonical pack outputs.

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(plm)
})

source("R/utils.R")     # for out_path()

# ---- Load engineered data ----
full <- readr::read_csv(out_path("derived/fellowship_training_clean.csv"), show_col_types = FALSE)

# Minimal type / NA safety to mirror 03_model_ols_logit_plm.R
full <- full %>%
  mutate(
    fastball            = as.numeric(fastball),          # 0/1 outcome
    pressure_situation  = as.numeric(pressure_situation),
    free_agent_status   = coalesce(as.numeric(free_agent_status), 0),
    attendance          = coalesce(attendance, 0),
    temp                = coalesce(temp, median(temp, na.rm = TRUE)),
    windspeed           = coalesce(windspeed, 0),
    gamesup             = coalesce(gamesup, 0),
    gamesback           = coalesce(gamesback, 0),
    wc                  = coalesce(wc, 0),
    p_home              = coalesce(p_home, 0),
    pitch_count         = coalesce(pitch_count, 0)
  )

# Alias to match earlier naming if needed
if (!"ScoreDiff" %in% names(full) && "score_diff" %in% names(full)) {
  full <- full %>% mutate(ScoreDiff = score_diff)
}

# Helper to print compact summaries
show <- function(model, title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
  print(summary(model))
}
tidy_show <- function(model, keep = NULL, add_title = NULL) {
  tt <- broom::tidy(model, conf.int = TRUE)
  if (!is.null(keep)) tt <- dplyr::filter(tt, term %in% keep)
  if (!is.null(add_title)) cat("\n-- ", add_title, " --\n", sep = "")
  print(tt)
}

# =============================================================================
# THEMED MODELS (website-friendly)
# =============================================================================

# 1) Baseline: Does pressure shift pitch mix?
#    Interpretation: coefficient on pressure_situation is the %-point change
#    in fastball probability under pressure (linear probability model).
m_baseline <- lm(fastball ~ pressure_situation, data = full)
show(m_baseline, "Model 1 — Baseline (Pressure only)")
tidy_show(m_baseline, keep = c("pressure_situation"), add_title = "Key term(s)")

# 2) Pressure × Free Agent (contract incentive channel)
#    Interpretation: 'pressure_situation' = effect for non-FA;
#    'free_agent_status' = baseline FA vs non-FA;
#    'pressure_situation:free_agent_status' = *extra* pressure effect among FAs.
m_contract <- lm(fastball ~ pressure_situation * free_agent_status, data = full)
show(m_contract, "Model 2 — Pressure × Free Agent (Interaction)")
tidy_show(m_contract, keep = c("pressure_situation",
                               "free_agent_status",
                               "pressure_situation:free_agent_status"),
          add_title = "Key term(s)")

# 3) Context controls: score state, inning, base runners, fatigue
#    Interpretation: pressure effect net of obvious situational levers.
m_context <- lm(
  fastball ~ pressure_situation + ScoreDiff + inning + outs +
    on_1b + on_2b + on_3b + pitch_count,
  data = full
)
show(m_context, "Model 3 — Context Controls (score/inning/baserunners/fatigue)")
tidy_show(m_context, keep = c("pressure_situation"), add_title = "Key term(s)")

# 4) Team playoff context (division race / wildcard)
#    Interpretation: checks whether playoff race variables explain away pressure.
m_team <- lm(
  fastball ~ pressure_situation + gamesup + gamesback + wc,
  data = full
)
show(m_team, "Model 4 — Team Context (gamesup/gamesback/wc)")
tidy_show(m_team, keep = c("pressure_situation","gamesup","gamesback","wc"),
          add_title = "Key term(s)")

# 5) Recent behavior controls (rolling/smoothed ‘last-5’ tendencies)
#    Interpretation: isolates pressure beyond short-run momentum/habits.
has_roll <- c("last5_fastball_share_sm","last5_strike_rate_sm","last5_ab_hit_rate_sm") %in% names(full)
if (all(has_roll)) {
  m_recent <- lm(
    fastball ~ pressure_situation +
      last5_fastball_share_sm + last5_strike_rate_sm + last5_ab_hit_rate_sm,
    data = full
  )
  show(m_recent, "Model 5 — Recent Behavior Controls (smoothed last-5)")
  tidy_show(m_recent, keep = c("pressure_situation",
                               "last5_fastball_share_sm","last5_strike_rate_sm","last5_ab_hit_rate_sm"),
            add_title = "Key term(s)")
} else {
  message("Model 5 skipped: rolling/smoothed features not present.")
}

# 6) Pitcher Fixed-Effects (within-pitcher)
#    Interpretation: uses only *within-pitcher* deviations; controls for all
#    time-invariant pitcher traits (baseline repertoire, velocity, style).
#    Caveat: needs variation within pitcher; coefficients read as partial LPM effects.
df_plm <- full %>%
  transmute(
    pitcher_id,
    fastball,
    pressure_situation,
    pitch_count,
    free_agent_status,
    p_home,
    ScoreDiff,
    inning,
    outs,
    on_1b,
    on_2b,
    on_3b,
  ) %>% tidyr::drop_na(fastball, pressure_situation)

m_fe <- plm(
  fastball ~ pressure_situation + pitch_count + free_agent_status + p_home + ScoreDiff + inning + outs +
    on_1b + on_2b + on_3b ,
  data = df_plm, index = "pitcher_id", model = "within", effect = "individual"
)
cat("\n", strrep("=", 78), "\nModel 6 — Pitcher Fixed-Effects (within)\n", strrep("=", 78), "\n", sep = "")
print(summary(m_fe))

# 7) Kitchen Sink (broad controls, no leakage)
#    Interpretation: pressure effect after conditioning on a wide set.
#    Caveats:
#      - Harder to interpret each coefficient.
#      - Risk of collinearity / overfitting if too many factors.
#      - Excludes leakage variables (pitch_type, type, event).

# Build a safe formula dynamically to avoid referencing missing columns
all_terms <- c(
  # pressure & key context
  "pressure_situation","ScoreDiff","inning","outs","on_1b","on_2b","on_3b",
  "pitch_num","pitch_count","attendance","temp","windspeed","p_home",
  # team race
  "gamesup","gamesback","wc",
  # recent behavior (only if present)
  intersect(c("last5_fastball_share_sm","last5_strike_rate_sm","last5_ab_hit_rate_sm"), names(full)),
  # ages / batter handedness if present (cached ages optional)
  intersect(c("current_agep","current_ageb","R_binary","L_binary","S_binary"), names(full))
) %>% unlist() %>% unique()

# Build formula: fastball ~ (all_terms collapsed with +)
rhs <- paste(all_terms, collapse = " + ")
fml_kitchen <- as.formula(paste0("fastball ~ ", rhs))

m_kitchen <- lm(fml_kitchen, data = full)
show(m_kitchen, "Model 7 — Kitchen Sink (broad controls, no leakage)")
tidy_show(m_kitchen, keep = c("pressure_situation"), add_title = "Key term(s)")
