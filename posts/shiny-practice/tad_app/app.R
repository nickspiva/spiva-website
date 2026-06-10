# ============================================================
# Atlanta TADs & APS Funding — Interactive Shiny App
# posts/shiny-practice/tad_app/app.R
#
# HOW THIS APP IS STRUCTURED
# ─────────────────────────────────────────────────────────────
# A Shiny app has two halves:
#
#   ui     — Defines what the user sees: layout, input widgets
#             (sliders, buttons), and output *placeholders* where
#             charts and text will appear.
#
#   server — Defines the R logic. Everything here is *reactive*:
#             when an input changes, only the outputs that depend
#             on it re-run automatically. You never manually wire
#             up "on change" callbacks — Shiny's reactive graph
#             handles the plumbing.
#
# KEY PACKAGE: {ggiraph}
# ─────────────────────────────────────────────────────────────
# Makes ggplot2 objects interactive inside Shiny.
# - Replace geom_line()   with geom_line_interactive()
# - Replace geom_sf()     with geom_sf_interactive()
# - Add  data_id = ...  and  tooltip = ...  aesthetics
# - Use  girafeOutput()   instead of plotOutput()
# - Use  renderGirafe()   instead of renderPlot()
#
# Clicking a shape with data_id = "Beltline" sets
# input$<outputId>_selected = "Beltline" in the server,
# which your reactive code can read like any other input.
#
# CENTRAL STATE STORE
# ─────────────────────────────────────────────────────────────
# All variable state lives in one reactiveValues() store in the
# server, with per-key dependency tracking:
#   state$selected_tad  — highlighted TAD (NULL = all)
#   state$closure_years — per-TAD closure year
#   state$pilot_pcts    — per-TAD PILOT participation
#   state$growth        — projection method + per-TAD rates
# Widgets (sliders, presets, chart clicks) WRITE into the store;
# reactives and outputs READ from it, each invalidating only on
# the keys it reads. Selection highlighting is applied client-side
# by ggiraph CSS (opts_selection / opts_selection_inv), so clicking
# a TAD re-renders nothing — the server just relays the selection
# to the other charts via "<outputId>_set" messages.
# ============================================================

library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(sf)
library(ggiraph)
library(scales)
library(geomtextpath)

source("R/data.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
