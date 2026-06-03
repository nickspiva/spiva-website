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


# ════════════════════════════════════════════════════════════
# § 1  CONSTANTS ----
# ════════════════════════════════════════════════════════════

# APS millage rate (mills = dollars per $1,000 of assessed value).
# The *increment* — assessed value above the TAD baseline — currently
# flows to Invest Atlanta while the TAD is open.  Once a TAD closes,
# APS can collect: increment × (APS_MILLAGE / 1000) each year.
APS_MILLAGE <- 20.74
PROJ_END <- 2055
BUY_REF_YEAR <- 2030 # reference year for "what becomes possible" panel

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

# Static vision cards for the "What Becomes Possible" section.
# Cards are goal-oriented, not reactive to growth scenarios.
vision_items <- list(
  list(
    title = "Universal Pre-K",
    subtitle = "for 3 & 4-Year Olds",
    cost = "$78.2M / year",
    note = "APS Strategic Plan · 2030 Goal"
  ),
  list(
    title = "Free MARTA",
    subtitle = "for all APS students, K–12",
    cost = "$27.7M / year",
    note = "Modeled on DC Kids Ride Free Program"
  ),
  list(
    title = "$100K Average",
    subtitle = "Teacher Salary",
    cost = "$34.6M / year",
    note = "APS Strategic Plan · 2030 Goal"
  )
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
    filter(year < closure_year) |> # open = still diverting
    group_by(year) |>
    summarise(
      annual = sum(aps_annual_revenue, na.rm = TRUE),
      .groups = "drop"
    ) |>
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
        year = (last_year + 1):PROJ_END,
        annual = 0,
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

# Last year any TAD closes under the current plan — used to annotate the
# point where the Current Plan line goes flat on the diversion chart
LAST_CLOSURE_CURRENT <- max(
  diversion_scenarios[["Current Plan"]]$closure_year,
  na.rm = TRUE
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

  # ── JS: preset button active-state toggling ───────────────
  # Receives a preset key ('current', 'mayor1', 'mayor2', or null)
  # and swaps each button between its outline and filled class.
  tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('setActivePreset', function(preset) {
      var btns = {
        'current': { id: 'btn_current', active: 'btn-primary',  outline: 'btn-outline-primary' },
        'mayor1':  { id: 'btn_mayor1',  active: 'btn-warning',  outline: 'btn-outline-warning'  },
        'mayor2':  { id: 'btn_mayor2',  active: 'btn-danger',   outline: 'btn-outline-danger'   }
      };
      Object.keys(btns).forEach(function(key) {
        var el = document.getElementById(btns[key].id);
        if (!el) return;
        if (key === preset) {
          el.classList.remove(btns[key].outline);
          el.classList.add(btns[key].active);
          el.style.color = 'white';
        } else {
          el.classList.remove(btns[key].active);
          el.classList.add(btns[key].outline);
          el.style.color = '';
        }
      });
    });
  "
  )),

  # ── CSS: inline year select ───────────────────────────────────────────────
  tags$style(HTML(
    "
    select.inline-year-sel {
      -webkit-appearance: auto;
      appearance: auto;
      background: transparent;
      border: none;
      border-bottom: 1.5px solid #6c757d;
      font-size: inherit;
      font-weight: 700;
      color: inherit;
      cursor: pointer;
      padding: 0 2px;
      vertical-align: baseline;
      display: inline;
    }
    select.inline-year-sel:focus { outline: none; border-bottom-color: #0d6efd; }
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
      "Current Plan in Place",
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
    "schools to fund development. While a TAD is open, all tax revenue on property value ",
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
      uiOutput("diversion_subheader"),
      girafeOutput("diversion_chart", height = "360px")
    ),

    nav_panel(
      "Projected APS Revenue Coming from Closed TADs",
      uiOutput("proj_subheader"),
      girafeOutput("proj_chart", height = "380px")
    )
  ),

  br(),

  # ── What could this fund? ────────────────────────────────
  card(
    card_header("Why TAD Closures Matter for APS Schools"),
    uiOutput("buy_panel"),
    uiOutput("challenge_panel"),
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
      ),
      accordion_panel(
        "How is the free MARTA estimate calculated?",
        p(
          strong("Scope:"),
          " Annual cost of providing year-round, unlimited MARTA access to all APS students K–12.",
          " Modeled on DC's Kids Ride Free program, which covers all students ages 5–21 on all Metro/bus service."
        ),
        p(strong("Student count:")),
        tags$ul(
          tags$li("Total APS K–12 enrollment (October 2024): 44,876 students")
        ),
        p(strong("Cost calculation:")),
        tags$ul(
          tags$li(
            "Base rate: MARTA UPass monthly unlimited pass at $68.50/month"
          ),
          tags$li(
            "Duration: 12 months (year-round — school year, summer school, summer jobs, etc.)"
          ),
          tags$li(
            "Bulk discount: 25% reduction assumed, reflecting negotiating leverage for a district-wide recurring contract of this scale"
          ),
          tags$li("44,876 × $68.50 × 12 × 0.75 ≈ $27.7M/year")
        ),
        p(strong("Limitations:")),
        tags$ul(
          tags$li(
            "The 25% bulk discount is an assumption; actual negotiated rate could be higher or lower"
          ),
          tags$li(
            "UPass is a university product — APS would need a comparable institutional agreement with MARTA"
          ),
          tags$li(
            "Uses 2024 enrollment data; does not project future student population"
          ),
          tags$li("Not all APS students live near high-frequency MARTA service")
        ),
        p(strong("Comparable programs:")),
        tags$ul(
          tags$li(
            tags$a(
              "DC Kids Ride Free",
              href = "https://ddot.dc.gov/page/kids-ride-free-program",
              target = "_blank"
            ),
            ": all students ages 5–21 ride free on all Metro/bus service, city-funded"
          ),
          tags$li("San Francisco Muni: free for all riders 18 and under"),
          tags$li("Seattle King County Metro: free for all riders 18 and under")
        ),
        p(
          class = "text-muted small mt-2 mb-0",
          tags$a(
            "APS Insights — Enrollment Data 1994–2024",
            href = "https://apsinsights.org/2025/03/13/aps-enrollment-data-1994-2024/",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "MARTA University Pass Program",
            href = "https://itsmarta.com/university-program.aspx",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "DC Kids Ride Free",
            href = "https://ddot.dc.gov/page/kids-ride-free-program",
            target = "_blank"
          )
        )
      ),
      accordion_panel(
        "How is the teacher salary estimate calculated?",
        p(
          strong("Scope:"),
          " Additional annual employer cost of raising the average APS teacher salary from its current level to $100,000,",
          " including the corresponding increase in employer pension contributions."
        ),
        p(strong("Inputs:")),
        tags$ul(
          tags$li(
            "Current average APS teacher salary: $90,470 (APS Back to Basics 2030 KPI Dashboard)"
          ),
          tags$li(
            "Total APS classroom teachers: 2,976 (APS Back to Basics 2030 KPI Dashboard)"
          ),
          tags$li("Salary gap: $100,000 − $90,470 = $9,530 per teacher"),
          tags$li("TRS of Georgia employer pension contribution rate: 21.91%")
        ),
        p(strong("Calculation:")),
        tags$ul(
          tags$li("Additional salary cost: 2,976 × $9,530 = $28,361,280"),
          tags$li("Additional pension cost: $28,361,280 × 21.91% = $6,213,896"),
          tags$li(strong("Total: ~$34.6M per year"))
        ),
        p(strong("Assumptions & limitations:")),
        tags$ul(
          tags$li(
            "Uses the average gap — assumes a uniform raise across all teachers.",
            " In practice, teachers already above $100K need no raise, so actual cost may be modestly lower."
          ),
          tags$li(
            "Does not include health insurance (no new hires; existing staff already covered)"
          ),
          tags$li(
            "Assumes flat teacher headcount — cost rises if APS grows enrollment and adds staff"
          ),
          tags$li("Pension rate rises to 22.32% in 2028 (TRS of Georgia)")
        ),
        p(
          class = "text-muted small mt-2 mb-0",
          tags$a(
            "APS Back to Basics 2030 KPI Dashboard",
            href = "https://www.atlantapublicschools.us/about/strategic-plan/key-performance-indicators",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "TRS of Georgia — Employer Contribution Rates",
            href = "https://www.trsga.com/employer/contribution-rates/",
            target = "_blank"
          ),
          " · ",
          tags$a(
            "GBPI — Retirement in Georgia's Public Schools",
            href = "https://gbpi.org/retirement-in-georgias-public-schools/",
            target = "_blank"
          )
        )
      )
    ),
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
  active_preset <- reactiveVal("current") # matches app's initial slider state

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

  observeEvent(input$btn_current, {
    apply_preset("year_end_current")
    active_preset("current")
  })
  observeEvent(input$btn_mayor1, {
    apply_preset("year_end_mayor1")
    active_preset("mayor1")
  })
  observeEvent(input$btn_mayor2, {
    apply_preset("year_end_mayor2")
    active_preset("mayor2")
  })

  # ── Slider drift: clear active preset when any slider moves off its preset value
  observe({
    # Read all sliders (establishes reactive dependency on each one)
    slider_vals <- map(active_tads$tad_id, \(tid) {
      input[[paste0("cl_", make.names(tid))]]
    })

    current <- isolate(active_preset())
    if (is.null(current)) {
      return()
    }

    col <- c(
      current = "year_end_current",
      mayor1 = "year_end_mayor1",
      mayor2 = "year_end_mayor2"
    )[[current]]

    still_matches <- every(seq_along(active_tads$tad_id), \(i) {
      meta <- filter(tad_meta, tad_id == active_tads$tad_id[i])
      expected <- meta[[col]]
      if (is.na(expected)) {
        expected <- meta$year_end_current
      }
      val <- slider_vals[[i]]
      is.null(val) || val == expected
    })

    if (!still_matches) active_preset(NULL)
  })

  # ── Send active-preset key to JS whenever it changes
  observe({
    session$sendCustomMessage("setActivePreset", active_preset() %||% "none")
  })

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

    # When sliders haven't been rendered yet (accordion still closed), fall back
    # to the active preset's column so preset buttons work before the accordion
    # is ever opened.
    preset_col <- switch(
      active_preset() %||% "current",
      "current" = "year_end_current",
      "mayor1" = "year_end_mayor1",
      "mayor2" = "year_end_mayor2",
      "year_end_current"
    )

    # Active TADs: prefer the rendered slider value; fall back to preset column
    open <- map_dfr(active_tads$tad_id, \(tid) {
      val <- input[[paste0("cl_", make.names(tid))]]
      tibble(
        tad_id = tid,
        closure_year = if (is.null(val)) {
          meta <- filter(tad_meta, tad_id == tid)
          fb <- meta[[preset_col]]
          if (is.na(fb)) meta$year_end_current else fb
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

  # ── 7e-b. Diversion chart subheader (reactive) ───────────────────────────
  # Rebuilds when the growth-rate assumption changes so the gap dollar amount
  # and assumption name always match what's shown in the chart.
  output$diversion_subheader <- renderUI({
    proj_labels <- c(
      "tad" = "individualized historic TAD growth (2007–2024)",
      "tad_baseline" = "individualized TAD growth since creation of each TAD",
      "city" = "citywide average growth (2007–2024)",
      "optimistic" = "optimistic (average of high-growth TADs - Beltline, Eastside, & Atlantic Station)"
    )
    growth_name <- proj_labels[[input$proj_method]]
    ref_year_div <- as.integer(input$ref_year_div %||% 2035)

    dd <- diversion_data()
    is_cp <- grepl("Current Plan", as.character(dd$scenario), fixed = TRUE)
    is_m2 <- grepl("Updated NRI", as.character(dd$scenario), fixed = TRUE)

    cp_val <- dd$cumulative[is_cp & dd$year == PROJ_END]
    m2_val <- dd$cumulative[is_m2 & dd$year == PROJ_END]
    ann_ref <- dd$annual[is_m2 & dd$year == ref_year_div]
    ann_2055 <- dd$annual[is_m2 & dd$year == PROJ_END]

    fmt_amt <- function(x) {
      if (!length(x) || !is.finite(x)) {
        return("")
      }
      if (x >= 1e9) {
        dollar(x, scale = 1e-9, suffix = "B", accuracy = 0.1)
      } else {
        dollar(x, scale = 1e-6, suffix = "M", accuracy = 0.1)
      }
    }

    gap_fmt <- fmt_amt(m2_val - cp_val)
    ann_ref_fmt <- fmt_amt(ann_ref)
    ann_2055_fmt <- fmt_amt(ann_2055)

    # Inline year picker (same native <select> pattern as projected revenue card)
    options_html <- paste(
      vapply(
        2025:(PROJ_END - 1),
        function(yr) {
          sprintf(
            '<option value="%d"%s>%d</option>',
            yr,
            if (yr == ref_year_div) " selected" else "",
            yr
          )
        },
        character(1)
      ),
      collapse = ""
    )
    picker_html <- sprintf(
      '<select class="inline-year-sel" onchange="Shiny.setInputValue(\'ref_year_div\', parseInt(this.value), {priority:\'event\'})">%s</select>',
      options_html
    )

    HTML(sprintf(
      '<p class="text-muted small px-3 pt-1 mt-1">%s<br><br>%s</p>',
      sprintf(
        "This chart projects the <strong>cumulative APS property tax revenue redirected to Invest Atlanta</strong> from 2025 onward, while TADs remain open under each scenario. Under the current growth assumption — based on <strong>%s</strong> — the Mayor's Updated NRI proposal would divert an additional <strong>%s</strong> more than the current plan.",
        growth_name,
        gap_fmt
      ),
      sprintf(
        "On just an annual basis, the Mayor's Updated NRI proposal would divert approximately <strong>%s</strong> from APS in %s, ballooning to <strong>%s</strong> per year by 2055.",
        ann_ref_fmt,
        picker_html,
        ann_2055_fmt
      )
    ))
  })

  # ── 7e-c. Diversion comparison chart ─────────────────────
  # Shows cumulative revenue diverted AWAY from APS under each of the three
  # fixed scenarios. Unlike the projection chart, closure dates here are fixed
  # (not driven by sliders) so the three lines are always directly comparable.
  output$diversion_chart <- renderGirafe({
    dd <- diversion_data()

    # Bold dollar labels at the 2055 endpoint of each line
    labels_2055 <- dd |>
      filter(year == PROJ_END) |>
      mutate(
        lab = dollar(cumulative, scale = 1e-9, suffix = "B", accuracy = 0.1)
      )

    p <- ggplot(
      dd,
      aes(x = year, y = cumulative, color = scenario, group = scenario)
    ) +
      # ── Vertical dotted line: last TAD closes under Current Plan ──────────
      geom_vline(
        xintercept = LAST_CLOSURE_CURRENT,
        color = "#4a90d9",
        linetype = "dotted",
        linewidth = 0.6,
        alpha = 0.8
      ) +
      annotate(
        "text",
        x = LAST_CLOSURE_CURRENT - 0.4,
        y = Inf,
        label = paste0(
          "All TADs closed\n(Current Plan, ",
          LAST_CLOSURE_CURRENT,
          ")"
        ),
        hjust = 1,
        vjust = 1.3,
        size = 2.6,
        color = "#4a90d9"
      ) +
      # ── Lines with on-curve scenario labels (geomtextpath) ───────────────
      # geom_textline draws the line AND places the label along the curve.
      # hjust controls where along the line the text sits (0=start, 1=end).
      # gap = TRUE cuts the line behind the text for readability.
      # A thin invisible geom_line_interactive sits on top for hover events.
      # Current Plan label sits below its line
      geom_textline(
        data = ~ filter(.x, grepl("Current Plan", scenario)),
        aes(label = scenario),
        linewidth = 1.3,
        size = 2.6,
        fontface = "bold",
        hjust = 0.72,
        gap = FALSE,
        text_smoothing = 20,
        offset = unit(-12, "pt")
      ) +
      # NRI scenario labels sit above their lines
      geom_textline(
        data = ~ filter(.x, !grepl("Current Plan", scenario)),
        aes(label = scenario),
        linewidth = 1.3,
        size = 2.6,
        fontface = "bold",
        hjust = 0.72,
        gap = FALSE,
        text_smoothing = 20,
        offset = unit(5, "pt")
      ) +
      geom_line_interactive(
        aes(data_id = scenario, tooltip = scenario),
        linewidth = 1.3,
        alpha = 0
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
      # ── End-of-line labels at 2055 ────────────────────────────────────────
      # hjust = -0.15 nudges text just past the last point; clip = "off" below
      # lets the text overflow the panel edge without being cropped.
      geom_text(
        data = labels_2055,
        aes(label = lab),
        hjust = -0.15,
        vjust = 0.5,
        size = 2.8,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_size_identity() +
      scale_color_manual(values = SCENARIO_COLORS, name = NULL) +
      scale_y_continuous(
        labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 0.1),
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.12))
      ) +
      scale_x_continuous(
        breaks = seq(2025, 2055, by = 5),
        expand = expansion(add = c(0, 3))
      ) +
      coord_cartesian(clip = "off") +
      labs(y = "") +
      theme_tad() +
      theme(legend.position = "none")

    girafe(
      ggobj = p,
      width_svg = 9,
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
            if_else(
              value >= 1e9,
              dollar(value, scale = 1e-9, suffix = "B", accuracy = 0.1),
              dollar(value, scale = 1e-6, suffix = "M", accuracy = 1)
            )
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

  # ── 7g-a. Projection chart subheader ─────────────────────
  output$proj_subheader <- renderUI({
    cy <- closure_years()
    pd <- proj_data()
    ref_year <- as.integer(input$ref_year %||% 2035)

    growth_name <- c(
      "tad" = "Historic TAD growth (2007–2024)",
      "tad_baseline" = "TAD growth since inception",
      "city" = "Citywide average growth",
      "optimistic" = "Optimistic (high-growth TADs)"
    )[[input$proj_method]]

    beltline_closure <- cy$closure_year[cy$tad_id == "Beltline"]

    # Native <select> built as an HTML string — onchange pushes value to Shiny.
    # This is the most reliable cross-browser inline picker: no custom JS
    # dropdown logic, no Bootstrap init dependency, always works in renderUI.
    options_html <- paste(
      vapply(
        2030:2055,
        function(yr) {
          sprintf(
            '<option value="%d"%s>%d</option>',
            yr,
            if (yr == ref_year) " selected" else "",
            yr
          )
        },
        character(1)
      ),
      collapse = ""
    )
    picker_html <- sprintf(
      '<select class="inline-year-sel" onchange="Shiny.setInputValue(\'ref_year\', parseInt(this.value), {priority:\'event\'})">%s</select>',
      options_html
    )

    example_html <- if (
      length(beltline_closure) > 0 &&
        !is.na(beltline_closure) &&
        beltline_closure <= ref_year
    ) {
      rev_val <- pd$aps_annual_revenue[
        pd$tad_id == "Beltline" & pd$year == ref_year
      ]
      rev_fmt <- if_else(
        rev_val >= 1e9,
        dollar(rev_val, scale = 1e-9, suffix = "B", accuracy = 0.1),
        dollar(rev_val, scale = 1e-6, suffix = "M", accuracy = 0.1)
      )
      sprintf(
        " For example, in %s, tax on property in the former Beltline TAD area will generate <strong>%s</strong> of annual revenue for schools, using the <strong>%s</strong> growth assumption.",
        picker_html,
        rev_fmt,
        growth_name
      )
    } else {
      sprintf(
        " For example, in %s: Beltline TAD hasn&#39;t closed yet under this scenario.",
        picker_html
      )
    }

    HTML(sprintf(
      '<p class="text-muted small px-3 pt-1 mt-1">%s%s</p>',
      "Hover over dots on the TAD revenue lines to see how much money APS will receive annually in property tax following the closure of each TAD.",
      example_html
    ))
  })

  # ── 7g. Graphic 3: Projection chart ──────────────────────
  output$proj_chart <- renderGirafe({
    sel <- selected_tad()
    cy <- closure_years()
    pd <- proj_data()

    # Atlantic Station and Princeton Lake are confirmed closed; Stadium is active.
    # Explicit list avoids any data-quirk ambiguity with the already_closed flag.
    closed_tad_ids <- c("Atlantic Station", "Princeton Lake")
    active_tad_ids <- tad_meta |>
      filter(!tad_id %in% closed_tad_ids) |>
      pull(tad_id)

    # TAD groupings for labeling
    corridor_ids <- c("Campbellton", "Metropolitan", "Stadium", "Hollowell")
    individual_ids <- c("Beltline", "Eastside", "Westside", "Perry Bolton")

    per_tad <- pd |>
      left_join(cy, by = "tad_id") |>
      filter(tad_id %in% active_tad_ids, year >= closure_year) |>
      mutate(
        line_a = if (is.null(sel)) 0.8 else if_else(tad_id == sel, 1, 0.12),
        line_w = if (is.null(sel)) 0.8 else if_else(tad_id == sel, 1.4, 0.45)
      )

    # ── Empty-state guard ─────────────────────────────────────────────────
    # Under scenarios where all TADs close after 2055, per_tad has no rows.
    # Return a blank chart with proper axes rather than letting ggplot error.
    if (nrow(per_tad) == 0) {
      p_empty <- ggplot(
        data.frame(year = c(2025, PROJ_END), rev = c(0, 0)),
        aes(x = year, y = rev)
      ) +
        annotate(
          "text",
          x = mean(c(2025, PROJ_END)),
          y = 0.5,
          label = "No active TADs close before 2055 under this scenario.\nRevenue to APS begins after 2055.",
          hjust = 0.5,
          vjust = 0.5,
          size = 3.2,
          color = "grey50"
        ) +
        scale_x_continuous(breaks = seq(2025, PROJ_END, by = 5)) +
        scale_y_continuous(
          labels = label_dollar(scale = 1e-6, suffix = "M"),
          limits = c(0, 1)
        ) +
        labs(y = "Annual APS Revenue") +
        theme_tad() +
        theme(
          axis.title.y = element_text(size = 10, color = "grey40"),
          legend.position = "none"
        )
      return(girafe(
        ggobj = p_empty,
        width_svg = 10,
        height_svg = 4.5,
        options = list(opts_toolbar(saveaspng = FALSE))
      ))
    }

    # Dashed vertical line at each TAD's closure year (active TADs only)
    vlines <- cy |>
      filter(
        tad_id %in% active_tad_ids,
        closure_year >= 2025,
        closure_year <= PROJ_END
      )

    # ── Start-of-line labels ───────────────────────────────────────────────
    # Individual TADs: label at first data point, stagger same-year pairs
    labels_individual <- per_tad |>
      filter(tad_id %in% individual_ids) |>
      group_by(tad_id) |>
      slice_min(year, n = 1) |>
      ungroup() |>
      arrange(year, aps_annual_revenue) |>
      group_by(year) |>
      mutate(rank_in_yr = row_number()) |>
      ungroup() |>
      mutate(
        lbl_vjust = if_else(rank_in_yr %% 2 == 1, -0.7, 1.6),
        lbl_alpha = line_a
      )

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
      # Individual TAD names at the start of each line; dim with selection
      geom_text(
        data = labels_individual,
        aes(label = tad_id, vjust = lbl_vjust, alpha = lbl_alpha),
        hjust = 0.5,
        size = 2.4,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_color_manual(values = TAD_PALETTE, na.value = "grey70") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_size_identity() +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      labs(y = "Annual APS Revenue") +
      theme_tad() +
      theme(
        axis.title.y = element_text(size = 10, color = "grey40"),
        legend.position = "none"
      )

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

    boxes <- map(vision_items, \(item) {
      card(
        class = "text-center border-0 bg-light h-100",
        div(
          class = "py-3 px-2",
          div(
            style = "font-size:1.4rem; font-weight:700; color:#2A9D8F; line-height:1.25;",
            item$title
          ),
          div(
            style = "font-size:0.9rem; color:#555; margin-bottom:0.5rem; line-height:1.3;",
            item$subtitle
          ),
          tags$hr(class = "mx-4 my-2"),
          tags$p(item$cost, class = "fw-semibold small mb-1"),
          tags$p(item$note, class = "small text-muted mb-0")
        )
      )
    })

    tagList(
      div(
        class = "px-3 pt-3 pb-1",
        div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span("✨", style = "font-size:1.1rem; color:#2A9D8F;"),
          tags$span(
            "What Becomes Possible",
            class = "fw-bold",
            style = "color:#2A9D8F; font-size:1rem;"
          )
        ),
        tags$p(
          HTML(sprintf(
            "APS has committed to these goals by 2030 as part of its Back to Basics strategic plan. TADs closing on schedule will generate an estimated <strong>%s</strong> in annual revenue for schools by %d — putting all three within reach.",
            dollar(rev, scale = 1e-6, suffix = "M", accuracy = 0.1),
            BUY_REF_YEAR
          )),
          class = "text-muted small mb-0"
        )
      ),
      do.call(
        layout_column_wrap,
        c(list(width = 1 / 3, gap = "0.5rem", class = "px-2 pb-2"), boxes)
      )
    )
  })

  # ── 7i. Challenge panel ───────────────────────────────────
  # Structural financial pressures APS faces regardless of TAD timelines.
  # Cards are placeholder-styled (amber); stats filled in as research lands.
  output$challenge_panel <- renderUI({
    challenge_items <- list(
      list(
        title = "Benefits Costs Up 78%",
        stat = "+$127M",
        stat_lbl = "increase in annual benefits costs since FY2016",
        desc = paste0(
          "Employee healthcare and pension costs have grown dramatically and are largely outside APS control. ",
          "The State Health Benefit Plan employer rate doubled from $11,340 to $22,620 between FY2021 and FY2026. ",
          "Teacher Retirement System rates rose from 14% to 22% over the past decade. ",
          "Neither is under APS's control, and both are projected to keep rising. ",
          "Also, Georgia stopped providing funding in 2012 for healthcare costs of essential workers such as ",
          " bus drivers, janitors, and food service staff, ",
          "further burdening local school districts."
        )
      ),
      list(
        title = "Property Tax Revenue at Risk",
        stat = "~3%",
        stat_lbl = "cap on annual property assessment increases",
        desc = paste0(
          "Georgia's legislature passed Senate Bill 33 this spring capping annual property assessment increases ",
          "at the rate of inflation — currently around 3%. ",
          "Property taxes are APS's primary revenue source, and legally capping revenue growth near inflation ",
          "while underlying costs consistently grow faster creates a structural deficit that compounds over time. ",
          "APS may have limited flexibility through millage rate increases, given that its rate has historically ",
          "exceeded the state's 20-mill cap — but this legislation is part of a broader pattern of constraints ",
          "on local districts' ability to fund public education."
        )
      ),
      list(
        title = "Declining Enrollment",
        stat = "2,398",
        stat_lbl = "projected student decline by 2030",
        desc = paste0(
          "APS enrollment is projected to fall from 49,944 students in 2024–25 to 47,546 in 2029–30. ",
          "The district cited this trend in its decision last fall to close 16 neighborhood schools — ",
          "including high-academic-growth schools like Dunbar. ",
          "TAD revenue could change the calculus on which schools can stay open and fully resourced."
        )
      )
    )

    challenge_cards <- map(challenge_items, \(item) {
      card(
        class = "text-center border-0 h-100",
        style = "background-color:#fff8e1;",
        div(
          class = "py-3 px-2",
          div(
            style = "font-size:2rem; font-weight:700; color:#e8a020; line-height:1;",
            item$stat
          ),
          tags$p(item$stat_lbl, class = "small text-muted mb-1"),
          tags$hr(class = "mx-4 my-1"),
          tags$p(item$title, class = "small fw-semibold mb-1"),
          tags$p(
            item$desc,
            class = "text-muted",
            style = "font-size:0.75rem; line-height:1.4;"
          )
        )
      )
    })

    tagList(
      tags$hr(class = "mx-3 mt-2 mb-0"),
      div(
        class = "px-3 pt-3 pb-1",
        div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span("⚠", style = "font-size:1.1rem; color:#e8a020;"),
          tags$span(
            "What's Already at Risk",
            class = "fw-bold",
            style = "color:#e8a020; font-size:1rem;"
          )
        ),
        tags$p(
          paste0(
            "TAD revenue arriving on time doesn't just unlock a wish list — ",
            "it helps APS navigate structural financial pressures that threaten the system regardless."
          ),
          class = "text-muted small mb-0"
        )
      ),
      do.call(
        layout_column_wrap,
        c(
          list(width = 1 / 3, gap = "0.5rem", class = "px-2 pb-3"),
          challenge_cards
        )
      )
    )
  })
}


# ════════════════════════════════════════════════════════════
# Launch ----
# ════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
