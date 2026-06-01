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


# ════════════════════════════════════════════════════════════
# § 1  CONSTANTS ----
# ════════════════════════════════════════════════════════════

# APS millage rate (mills = dollars per $1,000 of assessed value).
# The *increment* — assessed value above the TAD baseline — currently
# flows to Invest Atlanta while the TAD is open.  Once a TAD closes,
# APS can collect: increment × (APS_MILLAGE / 1000) each year.
APS_MILLAGE <- 20.74
PROJ_END <- 2055
BUY_REF_YEAR <- 2035 # reference year for "what could this fund?" panel

# One color per TAD — kept consistent across ALL graphics so the
# reader can cross-reference without a legend on every chart.
TAD_PALETTE <- c(
  "Beltline" = "#E63946",
  "Westside" = "#457B9D",
  "Perry Bolton" = "#2A9D8F",
  "Eastside" = "#E9C46A",
  "Campbellton" = "#F4A261",
  "Metropolitan" = "#264653",
  "Stadium" = "#6A4C93",
  "Atlantic Station" = "#A8DADC",
  "Princeton Lake" = "#95D5B2",
  "Hollowell" = "#B5838D"
)

# Items for the "What Could This Fund?" panel.
# cost_label controls the "@ X / unit" display line — lets us format large
# costs like the pre-K program as "$78.2M" rather than the default k-scale.
unit_costs <- tribble(
  ~id           , ~label                                                   , ~cost    , ~unit          , ~cost_label          ,
  "prek"        , "Universal Pre-K teaching staff\n(668 educators, 3K+4K)" , 78222968 , "full program" , "$78.2M / program"   ,
  "lunches"     , "Free school lunches (1 yr)"                             ,     1000 , "student"      , "$1k / student"      ,
  "buses"       , "Electric school buses"                                  ,   400000 , "bus"          , "$400k / bus"        ,
  "playgrounds" , "New playgrounds"                                        ,   100000 , "playground"   , "$100k / playground" ,
  "schools"     , "Neighborhood schools kept open"                         ,  1500000 , "school/yr"    , "$1.5M / school/yr"
)


# ════════════════════════════════════════════════════════════
# § 2  DATA LOADING & WRANGLING ----
# ════════════════════════════════════════════════════════════
# The CSV has a *transposed* layout:
#   Row 1  = field labels / TAD full names
#   Row 2  = short names
#   Rows 3–20 = annual property values (2007–2024)
#   Row 21 = Baseline (value at TAD creation)
#   Row 22 = Year Created
#   Row 23 = Current planned Year End
#   Row 24 = Mayor's Original NRI proposal
#   Row 25 = Mayor's Updated NRI proposal
#   Last 2 cols = COA + All-TADs aggregates (excluded below)

raw <- read_csv(
  "../TAD Basics.csv",
  col_names = FALSE,
  show_col_types = FALSE
)

# Columns 2 through (n-2) are the individual TADs; last 2 are aggregates
tad_cols <- 2:(ncol(raw) - 2)

# Fix the "Atlantaic Station" typo in the source data
clean_str <- function(x) {
  str_trim(as.character(x)) |> str_replace("Atlantaic", "Atlantic")
}

# unlist() converts a single tibble row into a plain vector before clean_str / as.numeric
full_names <- clean_str(unlist(raw[1, tad_cols], use.names = FALSE))
short_names <- clean_str(unlist(raw[2, tad_cols], use.names = FALSE))

parse_meta <- function(row_num) {
  suppressWarnings(as.numeric(unlist(
    raw[row_num, tad_cols],
    use.names = FALSE
  )))
}

tad_meta <- tibble(
  tad_id = short_names,
  full_name = full_names,
  baseline = parse_meta(21),
  year_created = parse_meta(22),
  year_end_current = parse_meta(23),
  year_end_mayor1 = parse_meta(24), # Mayor's Original NRI
  year_end_mayor2 = parse_meta(25), # Mayor's Updated NRI
) |>
  mutate(
    # Atlantic Station (2024) and Princeton Lake (2023) have already closed
    already_closed = !is.na(year_end_current) & year_end_current <= 2024
  )

# Annual property values → tidy long format
years <- suppressWarnings(as.integer(raw[[1]][3:20]))

hist_data <- map_dfr(seq_along(short_names), \(i) {
  tibble(
    year = years,
    tad_id = short_names[i],
    value = suppressWarnings(
      parse_number(
        as.character(raw[3:20, tad_cols[i]][[1]]),
        na = c("#N/A", "NA", "")
      )
    )
  )
}) |>
  filter(!is.na(value), !is.na(year)) |>
  left_join(tad_meta |> select(tad_id, baseline, already_closed), by = "tad_id")

#fancier version of using pivot-longer to handle some non-standard things - e.g. dollar value strings, etc.

# ════════════════════════════════════════════════════════════
# § 3  PROJECTION MODEL ----
# ════════════════════════════════════════════════════════════
# For each TAD we compute a Compound Annual Growth Rate (CAGR)
# from its first to last observed value, then extrapolate forward.
#
# Three selectable scenarios:
#   tad        — each TAD uses its own historical CAGR
#   city       — every TAD grows at Atlanta's overall CAGR
#   optimistic — every TAD grows at the 75th-percentile CAGR

growth_rates <- hist_data |>
  filter(!is.na(value)) |>
  group_by(tad_id) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    first_val = value[which.min(year)],
    last_val = value[which.max(year)],
    .groups = "drop"
  ) |>
  # cagr: TAD-specific rate from first available data year (2007) to 2024
  mutate(cagr = (last_val / first_val)^(1 / (last_year - first_year)) - 1) |>
  # Join baseline value and creation year from tad_meta so we can compute
  # the baseline-to-2024 CAGR, which reaches back to each TAD's founding
  # year rather than just 2007
  left_join(
    tad_meta |> select(tad_id, baseline, year_created),
    by = "tad_id"
  ) |>
  mutate(
    # cagr_baseline: uses the TAD's assessed value at creation as the
    # starting point. For TADs created before 2007 (Westside 1998,
    # Atlantic Station 1999, Princeton Lake / Perry Bolton 2002, Eastside
    # 2003, Beltline 2005) this gives a longer, fuller growth history.
    cagr_baseline = (last_val / baseline)^(1 / (last_year - year_created)) - 1
  )

glimpse(growth_rates)

glimpse(hist_data) # opens the data viewer tab

atl_col <- which(as.character(raw[1, ]) == "ATLANTA")
if (length(atl_col) == 0) {
  stop("Could not find 'ATLANTA' column in TAD Basics.csv")
}

atl_series <- tibble(
  year = years,
  value = suppressWarnings(
    parse_number(
      as.character(raw[3:20, atl_col][[1]]),
      na = c("#N/A", "NA", "")
    )
  )
) |>
  filter(!is.na(value))

citywide_cagr <- (last(atl_series$value) / first(atl_series$value))^(1 /
  (last(atl_series$year) - first(atl_series$year))) -
  1

optimistic_cagr <- quantile(growth_rates$cagr, 0.75, na.rm = TRUE)

# Build a projection tibble.
# rate_override: a single numeric rate applied to ALL TADs (used for citywide
#   and optimistic scenarios). NULL means use a per-TAD rate from growth_rates.
# rate_col: which column of growth_rates to use as the per-TAD rate when
#   rate_override is NULL. Defaults to "cagr" (2007–2024); pass "cagr_baseline"
#   for the creation-year-to-2024 rates.
build_projections <- function(rate_override = NULL, rate_col = "cagr") {
  map_dfr(seq_len(nrow(growth_rates)), \(i) {
    g <- growth_rates[i, ]
    r <- if (!is.null(rate_override)) rate_override else g[[rate_col]]
    tibble(
      year = (g$last_year + 1):PROJ_END,
      tad_id = g$tad_id,
      value = g$last_val * (1 + r)^seq_len(PROJ_END - g$last_year)
    )
  }) |>
    left_join(tad_meta |> select(tad_id, baseline), by = "tad_id") |>
    mutate(
      increment = pmax(value - baseline, 0), # can't be negative
      aps_annual_revenue = increment * APS_MILLAGE / 1000
    )
}

# Pre-compute all four so we only run the model once at startup
proj_list <- list(
  tad = build_projections(),
  tad_baseline = build_projections(rate_col = "cagr_baseline"),
  city = build_projections(citywide_cagr),
  optimistic = build_projections(optimistic_cagr)
)

# ── Diversion chart helpers ────────────────────────────────────────────────
# Fixed closure year tables for each of the three named scenarios.
# coalesce() handles TADs with NA in mayor columns (already-closed TADs like
# Atlantic Station and Princeton Lake) by falling back to year_end_current.
diversion_scenarios <- list(
  "Current Plan" = tad_meta |>
    transmute(tad_id, closure_year = year_end_current),
  "Mayor's Original NRI" = tad_meta |>
    transmute(
      tad_id,
      closure_year = coalesce(year_end_mayor1, year_end_current)
    ),
  "Mayor's Updated NRI" = tad_meta |>
    transmute(
      tad_id,
      closure_year = coalesce(year_end_mayor2, year_end_current)
    )
)

# Compute cumulative APS revenue DIVERTED to Invest Atlanta under one scenario.
# While a TAD is OPEN (year < closure_year), its annual APS increment revenue
# goes to Invest Atlanta instead of schools. Summing and cumsumming gives the
# running total of what schools have missed.
compute_diverted <- function(proj_df, closure_df) {
  result <- proj_df |>
    left_join(closure_df, by = "tad_id") |>
    filter(year < closure_year) |>   # open = still diverting
    group_by(year) |>
    summarise(annual = sum(aps_annual_revenue, na.rm = TRUE), .groups = "drop") |>
    arrange(year) |>
    mutate(cumulative = cumsum(annual))

  # Once all TADs have closed the cumulative total stops growing but doesn't
  # disappear — extend the line flat to PROJ_END so all scenarios run to the
  # same x-axis endpoint and the final diverted total remains visible.
  last_year <- max(result$year)
  if (last_year < PROJ_END) {
    result <- bind_rows(
      result,
      tibble(
        year       = (last_year + 1):PROJ_END,
        annual     = 0,
        cumulative = last(result$cumulative)
      )
    )
  }

  result
}

SCENARIO_COLORS <- c(
  "Current Plan" = "#4a90d9",
  "Mayor's Original NRI" = "#e8a020",
  "Mayor's Updated NRI" = "#e74c3c"
)


# ════════════════════════════════════════════════════════════
# § 4  SHAPEFILES ----
# ════════════════════════════════════════════════════════════

tad_sf <- st_read("../TAD_shapefiles/Tax_Allocation_District.shp", quiet = TRUE)

roads_sf <- bind_rows(
  st_read("../Road_shapefiles/tl_2023_13121_roads.shp", quiet = TRUE),
  st_read("../Road_shapefiles/tl_2023_13089_roads.shp", quiet = TRUE)
) |>
  filter(MTFCC %in% c("S1100", "S1200")) |> # S1100 = highways, S1200 = major roads
  st_transform(4326)

# TAD names are in ZONEDESC (ZONENAME just says "TAD" for every row).
# Actual ZONEDESC values: "Beltline", "Westside", "Perry/Bolton",
# "Eastside", "Atlantic Station", "Princeton Lakes", "Metropolitan Pkwy",
# "Stadium", "Hollowell / Martin Luther King", "Campbellton"
tad_sf <- tad_sf |>
  rename(shp_name = ZONEDESC) |>
  mutate(
    tad_id = case_when(
      str_detect(toupper(shp_name), "BELTLINE") ~ "Beltline",
      str_detect(toupper(shp_name), "WESTSIDE") ~ "Westside",
      str_detect(toupper(shp_name), "PERRY") ~ "Perry Bolton", # "Perry/Bolton"
      str_detect(toupper(shp_name), "EASTSIDE") ~ "Eastside",
      str_detect(toupper(shp_name), "CAMPBELLTON") ~ "Campbellton",
      str_detect(toupper(shp_name), "METROPOL") ~ "Metropolitan", # "Metropolitan Pkwy"
      str_detect(toupper(shp_name), "STADIUM") ~ "Stadium",
      str_detect(toupper(shp_name), "ATLANTIC") ~ "Atlantic Station",
      str_detect(toupper(shp_name), "PRINCETON") ~ "Princeton Lake", # "Princeton Lakes"
      str_detect(toupper(shp_name), "HOLLOWELL") ~ "Hollowell", # "Hollowell / MLK"
      TRUE ~ NA_character_
    )
  ) |>
  left_join(tad_meta |> select(tad_id, already_closed), by = "tad_id")

# Transform to WGS84
tad_sf <- st_transform(tad_sf, 4326)

# City of Atlanta boundary — used as a mid-gray polygon under the TADs
city_sf <- st_read(
  "../City_shapefile/Official_Atlanta_City_Limits_-_Open_Data.shp",
  quiet = TRUE
) |>
  st_transform(4326)

# Label positions: st_point_on_surface() guarantees the point lies *on* the
# polygon, unlike st_centroid() which can fall outside ring-shaped TADs
# (e.g. Beltline). We project to UTM first (EPSG:26916) so the geometry
# operation is done in Cartesian space (avoids the lon/lat warning), then
# transform the result back to WGS84 to match the map tiles.
tad_labels <- tad_sf |>
  filter(!is.na(tad_id)) |>
  st_transform(26916) |>
  st_point_on_surface() |>
  st_transform(4326) |>
  mutate(label = str_wrap(tad_id, width = 10))

# Manual label position overrides for two problem cases:
#
# Beltline — ring-shaped polygon: st_point_on_surface lands on the ring arc,
# usually somewhere in the SW. Move it to the NE arc near Virginia-Highland /
# Ponce City Market where there's open map space to read the label.
#
# Atlantic Station — very small polygon (~1.4 km²): the auto-label completely
# covers the polygon. Nudge it slightly north so the polygon stays visible.
label_overrides <- list(
  "Beltline" = c(-84.363, 33.769),
  "Atlantic Station" = c(-84.401, 33.797)
)
for (tid in names(label_overrides)) {
  idx <- which(tad_labels$tad_id == tid)
  if (length(idx) > 0) {
    sf::st_geometry(tad_labels)[idx] <-
      sf::st_sfc(sf::st_point(label_overrides[[tid]]), crs = 4326)
  }
}


# ════════════════════════════════════════════════════════════
# § 5  SHARED GGPLOT THEME ----
# ════════════════════════════════════════════════════════════
# Centralizing your theme means one change updates all charts.
# This function extends theme_minimal() with project-specific tweaks.

theme_tad <- function(...) {
  theme_minimal(base_size = 12, ...) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 12, 8, 8)
    )
}

# ════════════════════════════════════════════════════════════
# § 6  UI ----
# ════════════════════════════════════════════════════════════
# page_fluid()       — standard responsive scrollable page
# layout_columns()   — Bootstrap 12-column grid; col_widths controls splits
# card()             — visual container with optional header/footer
# girafeOutput()     — placeholder for a ggiraph interactive chart
# uiOutput()         — placeholder for UI elements built dynamically in server

ui <- page_sidebar(
  title = "Atlanta TADs & Public School Funding",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE, # let cards take their natural height; main area scrolls

  # ── JS: background-click deselect ────────────────────────
  tags$script(HTML(
    "
    $(document).on('click', function(e) {
      var container = $(e.target).closest('.girafe_container_std');
      if (container.length > 0 && !$(e.target).attr('data-id')) {
        Shiny.setInputValue('bg_click', Math.random(), {priority: 'event'});
      }
    });
  "
  )),

  # ════════════════════════════════════════════════════════
  # SIDEBAR — sticky scenario controls
  # page_sidebar() keeps this panel fixed while the user
  # scrolls through the charts on the right.
  # ════════════════════════════════════════════════════════
  sidebar = sidebar(
    width = 290,
    open = "desktop", # open on desktop, collapsible on mobile

    h6("When will TADs close?", class = "fw-bold mb-1 mt-1"),

    actionButton(
      "btn_current",
      "Current Planned",
      class = "btn btn-outline-primary btn-sm w-100 mb-1"
    ),
    actionButton(
      "btn_mayor1",
      "Mayor's Original NRI",
      class = "btn btn-outline-warning btn-sm w-100 mb-1"
    ),
    actionButton(
      "btn_mayor2",
      "Mayor's Updated NRI",
      class = "btn btn-outline-danger btn-sm w-100"
    ),

    hr(class = "my-2"),

    h6("How fast will property values grow?", class = "fw-bold mb-1"),
    radioButtons(
      inputId = "proj_method",
      label = NULL,
      choices = c(
        "Historic TAD growth (2007–2024)" = "tad",
        "TAD growth since inception" = "tad_baseline",
        "Citywide average growth" = "city",
        "Optimistic (high-growth TADs)" = "optimistic"
      ),
      selected = "tad"
    ),

    hr(class = "my-2"),

    # Per-TAD sliders live in a collapsed accordion so they don't
    # overwhelm the sidebar — most users will use the presets above
    accordion(
      open = FALSE,
      accordion_panel(
        "Customize individual TAD closure years",
        uiOutput("tad_sliders")
      )
    )
  ),

  # ════════════════════════════════════════════════════════
  # MAIN CONTENT — charts scroll past the sticky sidebar
  # ════════════════════════════════════════════════════════

  p(
    "Atlanta's Tax Allocation Districts redirect property tax growth from ",
    "schools to fund development. While a TAD is open, all revenue on growth ",
    "above the original baseline goes to ",
    strong("Invest Atlanta"),
    " — not Atlanta Public Schools, the City, or County. ",
    "Use the controls on the left to explore different closure scenarios.",
    class = "text-muted mb-3"
  ),

  # ── Revenue impact — tabbed card ─────────────────────────
  # navset_card_tab() puts Bootstrap tab buttons in the card header,
  # letting users flip between the two complementary views of TAD revenue.
  navset_card_tab(
    nav_panel(
      "Revenue Diverted from Schools & Kids",
      p(
        "Cumulative APS property tax revenue redirected to Invest Atlanta while ",
        "TADs remain open, under each scenario. The gap between lines shows the ",
        "additional diversion under the Mayor's NRI proposals.",
        class = "text-muted small px-3 pt-1 mt-1"
      ),
      girafeOutput("diversion_chart", height = "360px")
    ),

    nav_panel(
      "Projected APS Revenue Coming from Closed TADs",
      p(
        "Revenue begins flowing to APS the year a TAD closes. Dashed vertical ",
        "lines mark each TAD's closure year under the current scenario. ",
        "Adjust the controls on the left to simulate different timelines.",
        class = "text-muted small px-3 pt-1 mt-1"
      ),
      girafeOutput("proj_chart", height = "380px")
    )
  ),

  br(),

  # ── What could this fund? ────────────────────────────────
  card(
    card_header(paste0(
      "What Could This Fund?  ·  Annual APS Revenue from Closed TADs in ",
      BUY_REF_YEAR
    )),
    p(
      paste0(
        "Based on estimated annual APS revenue in ",
        BUY_REF_YEAR,
        " under the current planned TAD closure timeline."
      ),
      class = "text-muted small px-3 pt-1"
    ),
    uiOutput("buy_panel"),
    accordion(
      open = FALSE,
      class = "mt-3 mx-2 mb-2",
      accordion_panel(
        "How is the Pre-K estimate calculated?",
        p(
          strong("Scope:"),
          " Annual staffing costs only — salaries plus employer benefits. Does not include capital costs, curriculum, materials, or transportation."
        ),
        p(strong("Seat gap:")),
        tags$ul(
          tags$li(
            "APS kindergarten enrollment (2025–26): 3,620 — proxy for each age cohort"
          ),
          tags$li(
            "Total seats needed for universal 3K + 4K: 7,240 (3,620 × 2 cohorts)"
          ),
          tags$li(
            "Existing APS pre-K seats: 1,234 (GADOE via APS Insights; does not break out by age and may not fully reflect Head Start seats serving 3-year-olds)"
          ),
          tags$li(strong("Gap: 6,006 additional seats"))
        ),
        p(strong("Staffing:")),
        tags$ul(
          tags$li(
            "Class size: 18 (state cap is 20; 18 for inclusion classrooms)"
          ),
          tags$li("Classrooms needed: ⌈6,006 ÷ 18⌉ = 334"),
          tags$li(
            "334 lead teachers + 334 assistant teachers = 668 total new staff"
          )
        ),
        p(strong("Annual employer cost per employee:")),
        tags$ul(
          tags$li(
            "Lead teacher: $100k salary + $22,620 health insurance + $21,910 pension (21.91%, TRS of Georgia) = $144,530"
          ),
          tags$li(
            "Assistant teacher: $55k salary + $22,620 health insurance + $12,050 pension = $89,670"
          ),
          tags$li(
            "Health insurance: $1,885/month × 12 = $22,620 — employer share (individual plan; family coverage would be higher)"
          )
        ),
        p(strong("Total: 334 × $144,530 + 334 × $89,670 ≈ $78.2M/year")),
        p(
          class = "text-muted small mt-2 mb-1",
          "Note: pension rises to 22.32% in 2028 (TRS of Georgia). Health insurance and salaries will grow over time. Capital costs for 334 new classrooms are not included."
        ),
        p(
          class = "text-muted small mb-0",
          tags$a(
            "APS Insights",
            href = "https://apsinsights.org/2026/02/23/aps-enrollment-1994-2026/",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "TRS of Georgia",
            href = "https://www.trsga.com/employer/contribution-rates/",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "GBPI FY2027 K-12 overview",
            href = "https://gbpi.org/overview-2027-fiscal-year-budget-for-k-12-education/",
            target = "_blank"
          )
        )
      )
    )
  ),

  br(),

  # ── Map + Historic chart ─────────────────────────────────
  layout_columns(
    col_widths = c(5, 7),
    gap = "1rem",
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center w-100",
          span("TAD Boundaries  ·  Click a district to highlight"),
          actionLink(
            "clear_sel",
            "× Show all",
            class = "small text-muted text-decoration-none"
          )
        )
      ),
      girafeOutput("tad_map", height = "460px")
    ),
    card(
      card_header("Historic Property Values  ·  2007–2024"),
      girafeOutput("historic_chart", height = "380px")
    )
  ),

  br()
)


# ════════════════════════════════════════════════════════════
# § 7  SERVER ----
# ════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  # ── 7a. Shared state: which TAD is selected ──────────────
  # reactiveVal() stores a single mutable value.
  # Any reactive that reads selected_tad() automatically
  # re-runs when it changes.
  selected_tad <- reactiveVal(NULL)

  # ignoreNULL = FALSE ensures these fire even when the selection becomes
  # empty (character(0)), which happens when the user clicks blank space
  # in a ggiraph chart. Without this, the observeEvent silently ignores
  # the deselect event and the highlight never clears.
  observeEvent(input$tad_map_selected, ignoreNULL = FALSE, {
    sel <- input$tad_map_selected
    selected_tad(if (length(sel) == 0) NULL else sel[1])
  })

  observeEvent(input$historic_chart_selected, ignoreNULL = FALSE, {
    sel <- input$historic_chart_selected
    selected_tad(if (length(sel) == 0) NULL else sel[1])
  })

  observeEvent(input$proj_chart_selected, ignoreNULL = FALSE, {
    sel <- input$proj_chart_selected
    selected_tad(if (length(sel) == 0) NULL else sel[1])
  })

  # Explicit "Show all" button — always reliable
  observeEvent(input$clear_sel, {
    selected_tad(NULL)
  })

  # Blank-space click inside any ggiraph container (fired by the JS above)
  observeEvent(input$bg_click, {
    selected_tad(NULL)
  })

  # ── 7b. Dynamic sliders ───────────────────────────────────
  # Active TADs: open, not yet closed, and with a known end year
  active_tads <- tad_meta |> filter(!already_closed, !is.na(year_end_current))

  output$tad_sliders <- renderUI({
    SLIDER_MIN <- 2025
    SLIDER_MAX <- 2060
    pct <- function(yr) {
      if (is.na(yr)) {
        NULL
      } else {
        (yr - SLIDER_MIN) / (SLIDER_MAX - SLIDER_MIN) * 100
      }
    }

    # Build one slider per active TAD, then arrange in a grid
    sliders <- map(active_tads$tad_id, \(tid) {
      meta <- filter(tad_meta, tad_id == tid)
      div(
        style = "min-width: 130px;",
        tags$p(strong(tid), class = "mb-0 small text-center"),
        sliderInput(
          inputId = paste0("cl_", make.names(tid)),
          label = NULL,
          min = SLIDER_MIN,
          max = SLIDER_MAX,
          value = meta$year_end_current,
          step = 1,
          sep = "",
          ticks = FALSE
        )
      )
    })

    # Build JS calls to place tick marks for each TAD.
    # Three ticks per slider, matching the preset button colors:
    #   blue  = Current planned closure year
    #   amber = Mayor's original NRI proposal
    #   red   = Mayor's updated NRI proposal
    tick_calls <- map_chr(active_tads$tad_id, \(tid) {
      meta <- filter(tad_meta, tad_id == tid)
      sid <- paste0("cl_", make.names(tid))
      calls <- c(
        if (!is.na(meta$year_end_current)) {
          sprintf(
            'addSliderTick("%s", %.2f, "#4a90d9", "Current: %d");',
            sid,
            pct(meta$year_end_current),
            meta$year_end_current
          )
        },
        if (!is.na(meta$year_end_mayor1)) {
          sprintf(
            'addSliderTick("%s", %.2f, "#e8a020", "Mayor orig. NRI: %d");',
            sid,
            pct(meta$year_end_mayor1),
            meta$year_end_mayor1
          )
        },
        if (!is.na(meta$year_end_mayor2)) {
          sprintf(
            'addSliderTick("%s", %.2f, "#e74c3c", "Mayor updated NRI: %d");',
            sid,
            pct(meta$year_end_mayor2),
            meta$year_end_mayor2
          )
        }
      )
      paste(calls, collapse = "\n")
    })

    # setTimeout(, 150) waits for ion.rangeSlider to finish rendering the
    # .irs-line elements before we try to inject the tick marks into them.
    tick_script <- tags$script(HTML(sprintf(
      "setTimeout(function() {\n%s\n}, 150);",
      paste(tick_calls, collapse = "\n")
    )))

    tagList(
      # width = 1 stacks sliders one per row — fits the narrow sidebar
      do.call(layout_column_wrap, c(list(width = 1), sliders)),
      tick_script
    )
  })

  # ── 7c. Preset buttons ────────────────────────────────────
  # observeEvent runs exactly once each time the button is clicked.
  # updateSliderInput() programmatically sets a slider's value.

  apply_preset <- function(col) {
    walk(active_tads$tad_id, \(tid) {
      meta <- filter(tad_meta, tad_id == tid)
      val <- meta[[col]]
      if (is.na(val)) {
        val <- meta$year_end_current
      }
      updateSliderInput(session, paste0("cl_", make.names(tid)), value = val)
    })
  }

  observeEvent(input$btn_current, apply_preset("year_end_current"))
  observeEvent(input$btn_mayor1, apply_preset("year_end_mayor1"))
  observeEvent(input$btn_mayor2, apply_preset("year_end_mayor2"))

  # ── 7d. Reactive derived data ─────────────────────────────
  # reactive() creates a lazily-evaluated, cached expression.
  # Multiple outputs can read the same reactive without re-running it.

  # Which projection dataset to use
  proj_data <- reactive({
    proj_list[[input$proj_method]]
  })

  # Closure year for each TAD under the current slider/preset state
  closure_years <- reactive({
    # Already-closed TADs: use their actual historical end year
    closed <- tad_meta |>
      filter(already_closed) |>
      transmute(tad_id, closure_year = year_end_current)

    # Active TADs: read from sliders (use fallback if slider not yet initialized)
    open <- map_dfr(active_tads$tad_id, \(tid) {
      val <- input[[paste0("cl_", make.names(tid))]]
      tibble(
        tad_id = tid,
        closure_year = if (is.null(val)) {
          tad_meta$year_end_current[tad_meta$tad_id == tid]
        } else {
          val
        }
      )
    })

    bind_rows(closed, open)
  })

  # Annual APS revenue summed across all TADs, accounting for closure years
  aps_revenue <- reactive({
    proj_data() |>
      left_join(closure_years(), by = "tad_id") |>
      filter(year >= closure_year) |> # revenue only flows AFTER closure
      group_by(year) |>
      summarise(
        total_rev = sum(aps_annual_revenue, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # Cumulative diversion data: re-computes when the projection method changes
  # (the three scenario closure dates are fixed, not driven by sliders)
  diversion_data <- reactive({
    pd <- proj_data()
    map_dfr(names(diversion_scenarios), \(nm) {
      compute_diverted(pd, diversion_scenarios[[nm]]) |>
        mutate(scenario = nm)
    }) |>
      mutate(scenario = factor(scenario, levels = names(diversion_scenarios)))
  })

  # ── 7e-b. Diversion comparison chart ─────────────────────
  # Shows cumulative revenue diverted AWAY from APS under each of the three
  # fixed scenarios. Unlike the projection chart, closure dates here are fixed
  # (not driven by sliders) so the three lines are always directly comparable.
  output$diversion_chart <- renderGirafe({
    dd <- diversion_data()

    # End-of-range annotation: total diverted by 2055 per scenario
    labels_2055 <- dd |>
      filter(year == max(year)) |>
      mutate(
        lab = paste0(
          scenario,
          "  ",
          dollar(cumulative, scale = 1e-9, suffix = "B", accuracy = 0.1)
        )
      )

    p <- ggplot(
      dd,
      aes(x = year, y = cumulative, color = scenario, group = scenario)
    ) +
      geom_line_interactive(
        aes(data_id = scenario, tooltip = scenario),
        linewidth = 1.3
      ) +
      geom_point_interactive(
        aes(
          data_id = scenario,
          size = 1.4,
          tooltip = paste0(
            "<b>",
            scenario,
            "</b><br>",
            year,
            "<br>",
            "Cumulative diverted: ",
            dollar(cumulative, scale = 1e-9, suffix = "B", accuracy = 0.1)
          )
        )
      ) +
      scale_size_identity() +
      scale_color_manual(values = SCENARIO_COLORS, name = NULL) +
      scale_y_continuous(
        labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(y = "Cumulative Revenue Diverted") +
      theme_tad()

    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 4,
      options = list(
        opts_selection(type = "none"),
        opts_hover(css = "cursor:default; opacity:1; stroke-width:2.5px;"),
        opts_tooltip(
          css = "background:white; border:1px solid #ccc;
                            padding:6px 10px; border-radius:4px; font-size:13px;"
        ),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  # ── 7e. Graphic 1: Historic chart ────────────────────────
  output$historic_chart <- renderGirafe({
    sel <- selected_tad()

    # Use alpha to dim non-selected lines; 1 = full, 0.12 = faded
    p <- hist_data |>
      mutate(
        # if (is.null(sel)) returns TRUE (scalar that recycles), avoiding
        # the logical(0) issue that tad_id == NULL would produce
        is_sel = if (is.null(sel)) TRUE else tad_id == sel,
        line_a = if_else(is_sel, 1, 0.12),
        line_w = if_else(is_sel, 1.1, 0.45),
        pt_size = if_else(is_sel, 1.8, 0.8)
      ) |>
      ggplot(aes(x = year, y = value, color = tad_id, group = tad_id)) +
      # geom_line_interactive handles click-to-select (data_id) but its tooltip
      # attaches to the whole SVG path, so it only ever shows one value.
      # We give it a minimal tooltip; per-year values come from the points below.
      geom_line_interactive(
        aes(
          alpha = line_a,
          linewidth = line_w,
          data_id = tad_id,
          tooltip = tad_id # simple label — points give the year+value detail
        )
      ) +
      # geom_point_interactive creates one SVG circle per row, so each point
      # gets its own tooltip with the correct year and value.
      geom_point_interactive(
        aes(
          alpha = line_a,
          size = pt_size,
          data_id = tad_id,
          tooltip = paste0(
            "<b>",
            tad_id,
            "</b><br>",
            year,
            ":  ",
            dollar(value, scale = 1e-6, suffix = "M", accuracy = 1)
          )
        )
      ) +
      scale_size_identity() +
      scale_color_manual(values = TAD_PALETTE, na.value = "grey70") +
      scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B")) +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      theme_tad()

    girafe(
      ggobj = p,
      width_svg = 7,
      height_svg = 4,
      options = list(
        opts_selection(type = "single", css = "stroke-width:3px; opacity:1;"),
        opts_hover(css = "cursor:pointer; opacity:0.9;"),
        opts_tooltip(
          css = "background:white; border:1px solid #ccc;
                              padding:6px 10px; border-radius:4px; font-size:13px;"
        ),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  # ── 7f. Graphic 2: Map ────────────────────────────────────
  # Programmatic basemap — no tile downloads, no raster dependency, instant.
  #
  # Layer order (back to front):
  #   panel.background fill  → surrounding area (#e4e4e4 light gray)
  #   geom_sf city_sf        → City of Atlanta (#d0d0d0 + border)
  #   geom_sf_interactive    → TAD colored polygons
  #   geom_sf_label          → TAD name labels
  #
  # The panel.background color matches the "surrounding area" gray, so any
  # aspect-ratio padding ggplot adds around the map panel is invisible.
  output$tad_map <- renderGirafe({
    sel <- selected_tad()

    map_data <- tad_sf |>
      mutate(
        fill_a = if (is.null(sel)) {
          0.70
        } else {
          if_else(!is.na(tad_id) & tad_id == sel, 0.88, 0.15)
        }
      )

    p <- ggplot() +
      # City of Atlanta boundary
      geom_sf(
        data = city_sf,
        fill = "#d0d0d0",
        color = "#a0a0a0",
        linewidth = 0.5
      ) +
      # Major roads (S1100 = interstates, S1200 = primary roads)
      geom_sf(
        data = roads_sf,
        color = "white",
        linewidth = 0.4,
        alpha = 0.8
      ) +
      # TAD polygons (interactive)
      geom_sf_interactive(
        data = map_data,
        aes(
          fill = tad_id,
          alpha = fill_a,
          data_id = tad_id,
          tooltip = paste0(
            coalesce(tad_id, as.character(shp_name)),
            if_else(!is.na(already_closed) & already_closed, " (closed)", "")
          )
        ),
        color = "white",
        linewidth = 0.6
      ) +
      # TAD name labels
      geom_sf_label(
        data = tad_labels,
        aes(label = label),
        size = 2.3,
        color = "grey10",
        fill = "white",
        alpha = 0.80,
        fontface = "bold",
        lineheight = 0.9,
        label.size = 0,
        label.padding = unit(0.12, "lines")
      ) +
      scale_fill_manual(
        values = TAD_PALETTE,
        na.value = "grey80",
        guide = "none"
      ) +
      scale_alpha_identity() +
      coord_sf(
        xlim = c(-84.62, -84.27),
        ylim = c(33.62, 33.91),
        crs = 4326,
        expand = FALSE
      ) +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "#e4e4e4", color = NA),
        plot.background = element_rect(fill = "#e4e4e4", color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )

    girafe(
      ggobj = p,
      width_svg = 5,
      height_svg = 5,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_selection(
          type = "single",
          css = "stroke:black; stroke-width:2.5px; opacity:1;"
        ),
        opts_hover(css = "cursor:pointer; opacity:0.85;"),
        opts_tooltip(
          css = "background:white; border:1px solid #ccc;
                              padding:6px 10px; border-radius:4px; font-size:13px;"
        ),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  # ── 7g. Graphic 3: Projection chart ──────────────────────
  output$proj_chart <- renderGirafe({
    sel <- selected_tad()
    cy <- closure_years()
    pd <- proj_data()

    per_tad <- pd |>
      left_join(cy, by = "tad_id") |>
      filter(year >= closure_year) |>
      mutate(
        line_a = if (is.null(sel)) 0.8 else if_else(tad_id == sel, 1, 0.12),
        line_w = if (is.null(sel)) 0.8 else if_else(tad_id == sel, 1.4, 0.45)
      )

    # Dashed vertical line at each TAD's closure year
    vlines <- cy |> filter(closure_year >= 2025, closure_year <= PROJ_END)

    p <- ggplot(
      per_tad,
      aes(x = year, y = aps_annual_revenue, color = tad_id, group = tad_id)
    ) +
      geom_vline(
        data = vlines,
        aes(xintercept = closure_year, color = tad_id),
        linetype = "dashed",
        linewidth = 0.35,
        alpha = 0.45
      ) +
      geom_line_interactive(
        aes(
          alpha = line_a,
          linewidth = line_w,
          data_id = tad_id,
          tooltip = tad_id # line tooltip is simple; points give year detail
        )
      ) +
      geom_point_interactive(
        aes(
          alpha = line_a,
          size = line_w, # scale with selection state like lines do
          data_id = tad_id,
          tooltip = paste0(
            "<b>",
            tad_id,
            "</b><br>",
            year,
            "<br>",
            "APS revenue: ",
            dollar(
              aps_annual_revenue,
              scale = 1e-6,
              suffix = "M",
              accuracy = 0.1
            )
          )
        )
      ) +
      scale_color_manual(values = TAD_PALETTE, na.value = "grey70") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_size_identity() +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      labs(y = "Annual APS Revenue") +
      theme_tad() +
      theme(axis.title.y = element_text(size = 10, color = "grey40"))

    girafe(
      ggobj = p,
      width_svg = 10,
      height_svg = 4.5,
      options = list(
        opts_selection(type = "single"),
        opts_hover(css = "cursor:pointer; opacity:1; stroke-width:2px;"),
        opts_tooltip(
          css = "background:white; border:1px solid #ccc;
                              padding:6px 10px; border-radius:4px; font-size:13px;"
        ),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  # ── 7h. Graphic 4: What could this fund? ─────────────────
  # Always uses the *current planned* closure dates (not the sliders) so this
  # panel answers "what do we get if we stay on the existing timeline?" as a
  # fixed reference point rather than a simulation.

  ref_revenue <- reactive({
    cy_current <- tad_meta |>
      transmute(tad_id, closure_year = year_end_current)

    rv <- proj_data() |>
      left_join(cy_current, by = "tad_id") |>
      filter(year >= closure_year, year == BUY_REF_YEAR) |>
      summarise(total = sum(aps_annual_revenue, na.rm = TRUE)) |>
      pull(total)

    if (length(rv) == 0 || is.na(rv[1])) 0 else rv[1]
  })

  output$buy_panel <- renderUI({
    rev <- ref_revenue()

    boxes <- map(seq_len(nrow(unit_costs)), \(i) {
      item <- unit_costs[i, ]
      n <- floor(rev / item$cost)

      # Label may contain \n for line breaks — convert to HTML
      label_html <- HTML(gsub("\n", "<br>", item$label))

      card(
        class = "text-center border-0 bg-light h-100",
        div(
          class = "py-3",
          div(
            style = "font-size:2rem; font-weight:700; color:#E63946; line-height:1;",
            format(n, big.mark = ",")
          ),
          tags$p(item$unit, class = "small text-muted mb-1"),
          tags$hr(class = "mx-4 my-1"),
          tags$p(label_html, class = "small fw-semibold mb-0"),
          tags$p(item$cost_label, class = "small text-muted")
        )
      )
    })

    tagList(
      div(
        class = "px-3 pb-1",
        tags$span(
          class = "fw-bold",
          paste0("Annual APS revenue in ", BUY_REF_YEAR, " (current plan):  "),
          dollar(rev, scale = 1e-6, suffix = "M", accuracy = 0.1)
        )
      ),
      do.call(layout_column_wrap, c(list(width = 1 / 5, gap = "0.5rem"), boxes))
    )
  })
}


# ════════════════════════════════════════════════════════════
# Launch ----
# ════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
