# ── Data loading, projection model, shapefiles, theme ────────────────────────
# Sourced by app.R before ui.R and server.R

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
    title = "Universal Pre-K Staffing",
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
  "TAD Basics.csv",
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

tad_sf <- st_read("TAD_shapefiles/Tax_Allocation_District.shp", quiet = TRUE)

roads_sf <- bind_rows(
  st_read("Road_shapefiles/tl_2023_13121_roads.shp", quiet = TRUE), # Fulton
  st_read("Road_shapefiles/tl_2023_13089_roads.shp", quiet = TRUE), # DeKalb
  st_read("Road_shapefiles/tl_2023_13067_roads.shp", quiet = TRUE), # Cobb
  st_read("Road_shapefiles/tl_2023_13151_roads.shp", quiet = TRUE), # Henry
  st_read("Road_shapefiles/tl_2023_13097_roads.shp", quiet = TRUE), # Douglas
  st_read("Road_shapefiles/tl_2023_13063_roads.shp", quiet = TRUE), # Clayton
  st_read("Road_shapefiles/tl_2023_13051_roads.shp", quiet = TRUE)  # Cherokee
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
  "City_shapefile/Official_Atlanta_City_Limits_-_Open_Data.shp",
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
