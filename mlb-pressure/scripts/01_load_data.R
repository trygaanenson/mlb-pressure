source("R/utils.R")
dir_create(out_path("derived"))

keep <- c("type","pitch_type","event_num","b_score","ab_id","b_count","s_count","outs","pitch_num",
          "on_1b","on_2b","on_3b","batter_id","event","g_id","inning","o","p_score","p_throws",
          "pitcher_id","stand","top")

pitches      <- safe_read_csv(raw_path("pitches.csv"))
atbats       <- safe_read_csv(raw_path("atbats.csv"))
games        <- safe_read_csv(raw_path("games.csv"))
player_names <- safe_read_csv(raw_path("player_names.csv"))

base <- pitches %>% left_join(atbats, by = "ab_id") %>% select(any_of(keep)) %>%
  left_join(games, by = "g_id") %>%
  left_join(player_names, by = c("batter_id" = "id")) %>% rename(bfirst_name = first_name, blast_name = last_name) %>%
  left_join(player_names, by = c("pitcher_id" = "id"), suffix = c("_b","_p")) %>% rename(pfirst_name = first_name, plast_name = last_name) %>%
  mutate(date = lubridate::ymd(date), year = year(date), month = month(date), day = day(date))

readr::write_csv(base, out_path("derived/fellowship_training_base.csv"))
