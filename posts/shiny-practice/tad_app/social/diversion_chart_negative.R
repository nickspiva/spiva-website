# ============================================================
# social/diversion_chart_negative.R
#
# Variant of diversion_chart.R with a downward (negative) y-axis
# to reinforce that this revenue is being TAKEN from APS.
# Lines fall toward -$5B rather than rising toward +$5B.
#
# HOW TO RUN from project root in Positron / RStudio:
#   withr::with_dir(
#     "posts/shiny-practice/tad_app",
#     source("social/diversion_chart_negative.R")
#   )
#
# Output: posts/shiny-practice/tad_app/social/diversion_chart_negative.png
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(geomtextpath)

source("R/data.R")

# ── Data prep ─────────────────────────────────────────────
# Negate cumulative so lines fall downward (revenue lost from APS)
dd <- diversion_list[["tad"]] |>
  filter(year >= 2030) |>
  group_by(scenario) |>
  mutate(cumulative = -cumsum(annual)) |>
  ungroup() |>
  filter(grepl("Current Plan|Updated NRI", as.character(scenario))) |>
  mutate(scenario = as.character(scenario))

# Wide form for ribbon
# cp  = Current Plan   (less negative → upper line)
# nri = Updated NRI    (more negative → lower line)
ribbon_df <- dd |>
  select(year, scenario, cumulative) |>
  pivot_wider(names_from = scenario, values_from = cumulative) |>
  rename(cp = `Current Plan`, nri = `Mayor's Updated NRI`)

# Gap: cp - nri is positive (extra revenue taken under Updated NRI)
gap_2055 <- ribbon_df |>
  filter(year == 2055) |>
  mutate(gap = cp - nri) |>
  pull(gap)

gap_label <- dollar(gap_2055, scale = 1e-9, suffix = "B", accuracy = 0.1)

# Annotation anchor at 2050
ann_year <- 2050
ann_row <- ribbon_df |> filter(year == ann_year)
ann_mid <- (ann_row$cp + ann_row$nri) / 2
ann_height <- ann_row$cp - ann_row$nri # ribbon height (positive)

# End-of-line labels — display as positive values
labels_2055 <- dd |>
  filter(year == 2055) |>
  mutate(
    lab = dollar(abs(cumulative), scale = 1e-9, suffix = "B", accuracy = 0.1)
  )

# ── Colors ────────────────────────────────────────────────
col_cp <- "#888888"
col_nri <- "#c0392b"
col_ribbon <- "#e74c3c"

# ── Chart ─────────────────────────────────────────────────
p <- ggplot() +

  # Ribbon: nri is lower (more negative), cp is upper (less negative)
  geom_ribbon(
    data = ribbon_df,
    aes(x = year, ymin = nri, ymax = cp),
    fill = col_ribbon,
    alpha = 0.15
  ) +

  # Current Plan — upper line, label above
  geom_textline(
    data = filter(dd, grepl("Current Plan", scenario)),
    aes(x = year, y = cumulative, label = "Current Plan"),
    color = col_cp,
    linewidth = 1.6,
    size = 4.2,
    fontface = "bold",
    hjust = 0.68,
    gap = FALSE,
    text_smoothing = 20,
    offset = unit(7, "pt") # above the line
  ) +

  # Updated NRI — lower line, label below
  geom_textline(
    data = filter(dd, grepl("Updated NRI", scenario)),
    aes(x = year, y = cumulative, label = "Mayor's Updated NRI"),
    color = col_nri,
    linewidth = 1.6,
    size = 4.2,
    fontface = "bold",
    hjust = 0.68,
    gap = FALSE,
    text_smoothing = 20,
    offset = unit(-16, "pt") # below the line
  ) +

  # End-of-line totals at 2055
  geom_text(
    data = labels_2055,
    aes(x = year, y = cumulative, label = lab, color = scenario),
    hjust = -0.15,
    vjust = 0.5,
    size = 4.8,
    fontface = "bold",
    show.legend = FALSE
  ) +

  # ── Ribbon annotation ─────────────────────────────────
  annotate(
    "text",
    x = ann_year,
    y = ann_mid + ann_height * 0.22,
    label = paste0("+", gap_label),
    color = col_nri,
    size = 9,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  annotate(
    "text",
    x = ann_year,
    y = -1.90 * 1000000000,
    label = "more diverted from APS\nunder the Updated NRI Plan",
    color = col_nri,
    size = 3.5,
    hjust = 0.5,
    vjust = 1,
    lineheight = 1.1
  ) +

  scale_color_manual(
    values = c("Current Plan" = col_cp, "Mayor's Updated NRI" = col_nri),
    name = NULL
  ) +
  scale_y_continuous(
    # Display as positive dollar amounts even though data is negative
    labels = \(x) {
      ifelse(
        x == 0,
        "",
        label_dollar(scale = 1e-9, suffix = "B", accuracy = 1)(x)
      )
    },
    limits = c(NA, 0),
    expand = expansion(mult = c(0.1, 0)) # breathing room at bottom, flush at top
  ) +
  scale_x_continuous(
    breaks = seq(2030, 2055, by = 5),
    expand = expansion(add = c(0.5, 4.5))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Mayor's New Development Plan\nTakes Billions from APS",
    subtitle = "Cumulative APS property tax revenue redirected to Invest Atlanta, 2030–2055",
    caption = "Source: Invest Atlanta / Fulton County tax digest  ·  Analysis: nickspiva.shinyapps.io/tad_app/",
    y = ""
  ) +
  theme_tad() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 22,
      face = "bold",
      lineheight = 1.15,
      color = "#1a1a1a",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#555555",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#aaaaaa",
      hjust = 0,
      margin = margin(t = 14)
    ),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#eeeeee"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(28, 44, 18, 22)
  )

# ── Export ────────────────────────────────────────────────
out_path <- "social/diversion_chart_negative.png"

ggsave(
  out_path,
  plot = p,
  width = 7.5,
  height = 7.5,
  dpi = 144,
  bg = "white"
)

message("Saved to: ", normalizePath(out_path))
