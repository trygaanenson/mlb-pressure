# scripts/05_figures.R
# Creates two website-quality charts:
#   Fig 1: Faceted pie charts (Pressure vs Non-Pressure) showing fastball vs offspeed mix
#   Fig 2: Density of per-pitcher pressure coefficients (with significance shading)

suppressPackageStartupMessages({
  library(tidyverse)
  library(svglite)
})

source("R/utils.R")  # for out_path()

# -------------------------------------------------------------------
# Load engineered per-pitch data
# -------------------------------------------------------------------
full <- readr::read_csv(out_path("derived/fellowship_training_clean.csv"), show_col_types = FALSE)

# Ensure expected columns exist/types
full <- full %>%
  mutate(
    fastball = as.integer(fastball),
    pressure_situation = as.integer(pressure_situation)
  )

# -------------------------------------------------------------------
# FIGURE 1: Faceted pie charts of pitch mix under Pressure vs Non-Pressure
# -------------------------------------------------------------------

fig1_df <- full %>%
  mutate(
    pressure_label = if_else(pressure_situation == 1L, "Pressure", "Non-Pressure"),
    pitch_mix = if_else(fastball == 1L, "Fastball", "Offspeed/Other")
  ) %>%
  count(pressure_label, pitch_mix, name = "n") %>%
  group_by(pressure_label) %>%
  mutate(share = n / sum(n)) %>%
  ungroup()

fig1 <- ggplot(fig1_df, aes(x = "", y = share, fill = pitch_mix)) +
  geom_col(width = 1, color = "white", size = 0.4) +
  coord_polar(theta = "y") +
  facet_wrap(~ pressure_label, nrow = 1) +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  guides(fill = guide_legend(title = NULL)) +
  labs(
    title = "Pitch Mix Under Pressure vs Non-Pressure",
    subtitle = "Share of fastball vs offspeed/other within each context",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


# Save (PNG + SVG for crisp web embedding)
dir_create(out_path("figures"))
ggsave(out_path("figures/fig1_pie_pitch_mix_pressure_vs_nonpressure.png"),
       plot = fig1, width = 8, height = 4, dpi = 300)
svglite::svglite(out_path("figures/fig1_pie_pitch_mix_pressure_vs_nonpressure.svg"),
                 width = 8, height = 4); print(fig1); dev.off()

# -------------------------------------------------------------------
# FIGURE 2: Density of per-pitcher pressure coefficients
# -------------------------------------------------------------------
# Input produced by scripts/04_per_pitcher_loops.R
pp_path <- out_path("derived/per_pitcher_pressure_coeffs.csv")
if (!file.exists(pp_path)) {
  stop("Missing per-pitcher file: ", pp_path,
       "\nRun scripts/04_per_pitcher_loops.R first.")
}

pp <- readr::read_csv(pp_path, show_col_types = FALSE) %>%
  mutate(
    Significant_Effect = as.integer(Significant_Effect),
    sig_label = if_else(Significant_Effect == 1L, "Statistically Significant Effect", "Insignificant Effect")
  )

# Density plot of coefficients; shade by significance, mark zero with a vertical line
fig2 <- ggplot(pp, aes(x = Pressure_Situation_Coefficient, fill = sig_label)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Pitcher-Specific Pressure Effects",
    subtitle = "Per-pitcher OLS coefficient on pressure_situation (fastball as 0/1)",
    x = "Pressure effect on fastball probability (percentage points)",
    y = "Density",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(out_path("figures/fig2_density_per_pitcher_pressure_coeffs.png"),
       plot = fig2, width = 7, height = 4.5, dpi = 300)
svglite::svglite(out_path("figures/fig2_density_per_pitcher_pressure_coeffs.svg"),
                 width = 7, height = 4.5); print(fig2); dev.off()

# -------------------------------------------------------------------
# FIGURE 3: Jittered scatter — one point per pitcher
# -------------------------------------------------------------------
# Requirements: outputs/derived/per_pitcher_pressure_coeffs.csv
# Tip: run scripts/04_per_pitcher_loops.R first.

pp_path <- out_path("derived/per_pitcher_pressure_coeffs.csv")
if (!file.exists(pp_path)) {
  stop("Missing per-pitcher file: ", pp_path,
       "\nRun scripts/04_per_pitcher_loops.R first.")
}

pp <- readr::read_csv(pp_path, show_col_types = FALSE) %>%
  mutate(
    Significant_Effect = as.integer(Significant_Effect),
    sig_label = if_else(Significant_Effect == 1L, "p < 0.05", "ns or NA")
  )

fig3 <- ggplot(pp, aes(x = Pressure_Situation_Coefficient, y = 0, color = sig_label)) +
  geom_jitter(height = 0.2, alpha = 0.6, size = 1.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Per-Pitcher Pressure Effects (Scatter)",
    subtitle = "Each dot is a pitcher’s OLS coefficient on pressure_situation",
    x = "Pressure effect on fastball probability (percentage points)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  )

ggsave(out_path("figures/fig3_scatter_pitcher_effects_jitter.png"),
       plot = fig3, width = 7.5, height = 4.2, dpi = 300)
svglite::svglite(out_path("figures/fig3_scatter_pitcher_effects_jitter.svg"),
                 width = 7.5, height = 4.2); print(fig3); dev.off()


# -------------------------------------------------------------------
# FIGURE 4: Top 20 lollipop — largest |effects|, labeled (if names available)
# -------------------------------------------------------------------
# Try to attach names if raw player_names.csv is present; otherwise use IDs.

top20 <- pp %>%
  arrange(desc(abs(Pressure_Situation_Coefficient))) %>%
  slice_head(n = 20) %>%
  mutate(
    sig_label = if_else(Significant_Effect == 1L, "p < 0.05", "ns or NA"),
    label = forcats::fct_reorder(sig_label, Pressure_Situation_Coefficient)
  )

fig4 <- ggplot(top20, aes(x = Pressure_Situation_Coefficient, y = label)) +
  geom_segment(aes(x = 0, xend = Pressure_Situation_Coefficient,
                   y = label, yend = label),
               alpha = 0.4) +
  geom_point(aes(color = sig_label), size = 2.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Top 20 Pitchers by Pressure Effect (Lollipop)",
    subtitle = "Largest absolute OLS coefficients on pressure_situation",
    x = "Pressure effect on fastball probability (percentage points)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(out_path("figures/fig4_lollipop_top20_pitcher_effects.png"),
       plot = fig4, width = 7.5, height = 7.5, dpi = 300)
svglite::svglite(out_path("figures/fig4_lollipop_top20_pitcher_effects.svg"),
                 width = 7.5, height = 7.5); print(fig4); dev.off()

message("Figures saved to: ", out_path("figures"))
