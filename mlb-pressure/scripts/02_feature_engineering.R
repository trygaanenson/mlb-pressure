source("R/utils.R"); source("R/pressure.R")
ONLY_2018 <- FALSE  # toggle for deck parity; set FALSE to use all years

full <- readr::read_csv(out_path("derived/fellowship_training_base.csv"), show_col_types = FALSE)
if (ONLY_2018) full <- dplyr::filter(full, year == 2018)

# pitcher/batter teams
full <- full %>% mutate(
  pitcher_team = if_else(top == TRUE | top == "True", home_team, away_team),
  batter_team  = if_else(top == TRUE | top == "True", away_team, home_team)
)

# Free agents & standings
fa_tbl <- make_free_agent_table()
st_tbl <- make_standings_long() %>% dplyr::filter(!ONLY_2018 | year == 2018) %>% mutate(month = ifelse(is.na(wc), month, 9))

full <- full %>%
  left_join(fa_tbl, by = c("pfirst_name","plast_name","year")) %>%
  mutate(free_agent_status = tidyr::replace_na(free_agent_status, 0L)) %>%
  left_join(st_tbl, by = c("pitcher_team" = "team","year","month"))

# Weather & wind
full <- parse_weather(full)

# Optional cached ages (avoid network). Load if present.
age_csv <- raw_path("agedata.csv")
if (file.exists(age_csv)) {
  agedata <- readr::read_csv(age_csv, show_col_types = FALSE) %>% janitor::clean_names()
  if (all(c("player_id","current_age","year") %in% names(agedata))) {
    full <- full %>%
      left_join(select(agedata, player_id, current_age, year), by = c("pitcher_id" = "player_id","year")) %>%
      rename(current_agep = current_age) %>%
      left_join(select(agedata, player_id, current_age, bat_side_code, year), by = c("batter_id" = "player_id","year")) %>%
      rename(current_ageb = current_age)
    full <- full %>% mutate(
      r_binary = as.integer(bat_side_code == "R"),
      l_binary = as.integer(bat_side_code == "L"),
      s_binary = as.integer(bat_side_code == "S")
    )
  }
}

# Home indicator & pitch count
full <- full %>% mutate(p_home = as.integer(pitcher_team == home_team))
full <- add_pitch_count(full)

# Pressure flag
full <- make_pressure_flag(full)

# ---- Pitch-level interpretable flags ----
full <- full %>% mutate(
  is_fastball = pitch_type %in% c("FF","FT","FC","SI"),
  is_strike   = type %in% c("S","C","F","M","T"),
  is_ball     = type %in% c("B"),
  is_inplay   = type %in% c("X")
) %>%
  arrange(g_id, pitcher_id, ab_id, pitch_num) %>%
  group_by(g_id, pitcher_id) %>%
  mutate(
    last5_fastball_cnt = lastk_sum(as.integer(is_fastball), 5),
    last5_strike_cnt   = lastk_sum(as.integer(is_strike),   5),
    last5_ball_cnt     = lastk_sum(as.integer(is_ball),     5),
    last5_inplay_cnt   = lastk_sum(as.integer(is_inplay),   5),
    last5_fastball_share = lastk_mean(as.numeric(is_fastball), 5, min_k = 3),
    last5_strike_rate    = lastk_mean(as.numeric(is_strike),   5, min_k = 3),
    last5_ball_rate      = lastk_mean(as.numeric(is_ball),     5, min_k = 3),
    last5_inplay_rate    = lastk_mean(as.numeric(is_inplay),   5, min_k = 3)
  ) %>% ungroup()

# Smoothed last-5 using data-informed priors
KAPPA <- 2
fb_mu     <- mean(full$pitch_type %in% c("FF","FT","FC","SI"), na.rm = TRUE)
strike_mu <- mean(full$type %in% c("S","C","F","M","T"),       na.rm = TRUE)
ball_mu   <- mean(full$type %in% c("B"),                              na.rm = TRUE)
inplay_mu <- mean(full$type %in% c("X"),                              na.rm = TRUE)
alphaFB <- fb_mu*KAPPA; betaFB <- (1-fb_mu)*KAPPA
alphaS  <- strike_mu*KAPPA; betaS <- (1-strike_mu)*KAPPA
alphaB  <- ball_mu*KAPPA;   betaB <- (1-ball_mu)*KAPPA
alphaX  <- inplay_mu*KAPPA; betaX <- (1-inplay_mu)*KAPPA

full <- full %>% mutate(
  is_fastball = as.integer(is_fastball),
  is_strike   = as.integer(is_strike),
  is_ball     = as.integer(is_ball),
  is_inplay   = as.integer(is_inplay)
) %>%
  arrange(g_id, pitcher_id, ab_id, pitch_num) %>% group_by(g_id, pitcher_id) %>%
  mutate(
    last5_fastball_share_sm = smooth_lastk(is_fastball, 5, alphaFB, betaFB),
    last5_strike_rate_sm    = smooth_lastk(is_strike,   5, alphaS,  betaS),
    last5_ball_rate_sm      = smooth_lastk(is_ball,     5, alphaB,  betaB),
    last5_inplay_rate_sm    = smooth_lastk(is_inplay,   5, alphaX,  betaX),
    last5_n_used            = lastk_used(is_fastball,   5)
  ) %>% ungroup()

# AB-level smoothed features
ab_base <- full %>% arrange(g_id, ab_id, pitch_num) %>%
  summarise(pitcher_id = dplyr::first(pitcher_id), ab_event = dplyr::last(na.omit(event)), .by = c(g_id, ab_id)) %>%
  mutate(
    ab_hit    = ab_event %in% c("Single","Double","Triple","Home Run"),
    ab_bb     = ab_event %in% c("Walk","Intent Walk"),
    ab_so     = ab_event %in% c("Strikeout"),
    ab_onbase = ab_event %in% c("Field Error","Fielders Choice","Hit By Pitch"),
    ab_o      = ab_event %in% c("Bunt Groundout","Double Play","Fielders Choice Out","Flyout","Forceout",
                                "Grounded Into DP","Groundout","Lineout","Pop Out","Runner Out","Sac Bunt","Sac Fly")
  )

KAPPA_AB <- 2
ab_priors <- ab_base %>% summarise(
  mu_hit = mean(ab_hit, na.rm = TRUE), mu_bb = mean(ab_bb, na.rm = TRUE), mu_so = mean(ab_so, na.rm = TRUE),
  mu_o = mean(ab_o, na.rm = TRUE), mu_onbase = mean(ab_onbase, na.rm = TRUE)
)
alphaH <- ab_priors$mu_hit*KAPPA_AB; betaH <- (1 - ab_priors$mu_hit)*KAPPA_AB
alphaBB <- ab_priors$mu_bb*KAPPA_AB;  betaBB<- (1 - ab_priors$mu_bb)*KAPPA_AB
alphaS2 <- ab_priors$mu_so*KAPPA_AB;  betaS2<- (1 - ab_priors$mu_so)*KAPPA_AB
alphaO <- ab_priors$mu_o*KAPPA_AB;    betaO <- (1 - ab_priors$mu_o)*KAPPA_AB
alphaOB<- ab_priors$mu_onbase*KAPPA_AB; betaOB<- (1 - ab_priors$mu_onbase)*KAPPA_AB

ab_tbl <- ab_base %>% arrange(g_id, pitcher_id, ab_id) %>% group_by(g_id, pitcher_id) %>% mutate(
  last5_ab_hit_rate     = lastk_mean(as.numeric(ab_hit),    5, min_k = 2),
  last5_ab_bb_rate      = lastk_mean(as.numeric(ab_bb),     5, min_k = 2),
  last5_ab_so_rate      = lastk_mean(as.numeric(ab_so),     5, min_k = 2),
  last5_ab_o_rate       = lastk_mean(as.numeric(ab_o),      5, min_k = 2),
  last5_ab_onbase_rate  = lastk_mean(as.numeric(ab_onbase), 5, min_k = 2),
  last5_ab_hit_cnt      = lastk_sum(as.numeric(ab_hit),     5),
  last5_ab_bb_cnt       = lastk_sum(as.numeric(ab_bb),      5),
  last5_ab_so_cnt       = lastk_sum(as.numeric(ab_so),      5),
  last5_ab_o_cnt        = lastk_sum(as.numeric(ab_o),       5),
  last5_ab_onbase_cnt   = lastk_sum(as.numeric(ab_onbase),  5),
  last5_ab_hit_rate_sm     = smooth_lastk(as.numeric(ab_hit),    5, alphaH,  betaH),
  last5_ab_bb_rate_sm      = smooth_lastk(as.numeric(ab_bb),     5, alphaBB, betaBB),
  last5_ab_so_rate_sm      = smooth_lastk(as.numeric(ab_so),     5, alphaS2, betaS2),
  last5_ab_o_rate_sm       = smooth_lastk(as.numeric(ab_o),      5, alphaO,  betaO),
  last5_ab_onbase_rate_sm  = smooth_lastk(as.numeric(ab_onbase), 5, alphaOB, betaOB),
  last5_ab_n_used          = lastk_used(as.numeric(ab_hit), 5)
) %>% ungroup() %>% select(g_id, ab_id, starts_with("last5_ab_"))

full <- full %>% left_join(ab_tbl, by = c("g_id","ab_id"))

# Final flags expected elsewhere
full <- full %>% mutate(fastball = flag_fastball(pitch_type))

readr::write_csv(full, out_path("derived/fellowship_training_clean.csv"))
