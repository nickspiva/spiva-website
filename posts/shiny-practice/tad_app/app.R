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
# CROSS-FILTERING
# ─────────────────────────────────────────────────────────────
# A single reactiveVal called `selected_tad` stores the
# currently highlighted TAD (or NULL for "all").
# Map clicks → update selected_tad
# Chart clicks → update selected_tad
# All three charts listen to selected_tad and re-render
# whenever it changes — no manual wiring needed.
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(ggiraph)
library(scales)
library(geomtextpath)

source("R/data.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)
