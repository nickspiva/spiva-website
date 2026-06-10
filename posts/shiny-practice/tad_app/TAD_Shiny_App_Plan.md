# TAD Shiny App — Project Plan

*Last updated: June 2026*

## Overview

An interactive R/Shiny application visualizing Tax Allocation District (TAD) property values and their fiscal impact on Atlanta Public Schools (APS). The app connects five linked graphics enabling users to explore how TAD closure timelines affect school funding, and what that revenue could otherwise pay for.

**Audience:** Public advocacy and portfolio demonstration. Design and copy should be accessible to a general public audience, not just policy wonks.

------------------------------------------------------------------------

## Background: How TAD Revenue Works

A TAD freezes the property tax base ("the base") at the moment it's created. All property tax revenue generated *above* that base — the **increment** — flows to Invest Atlanta (the City's development arm) for the life of the TAD, rather than to APS, the City, or Fulton County. Only when a TAD closes does the full increment return to the general tax digest, at which point APS receives its share (roughly half, with the remainder split between the City and County).

**Exception — Eastside TAD:** Although not yet closed, APS currently receives the full increment from the Eastside TAD via PILOT (Payment in Lieu of Taxes) payments. This is noted in the data but not yet fully reflected in the projection model.

This dynamic — and how long TADs stay open — is the central policy question the app explores.

------------------------------------------------------------------------

## Current App Layout (as built)

Cards appear in this order, top to bottom:

### 1. Revenue Diverted from Schools & Kids / Projected APS Revenue — tabbed card ✅

A `navset_card_tab` card with two tabs:

**Tab A — Revenue Diverted from Schools & Kids** Cumulative line chart showing the total APS property tax revenue redirected to Invest Atlanta over time, under each of three fixed scenarios: Current Plan, Mayor's Original NRI, Mayor's Updated NRI. Three colored lines. Does not respond to sliders — always shows the three named scenarios for direct comparison. Scenario names are labeled directly on the curves using `geomtextpath::geom_textline()` (no legend). End-of-line labels at 2055 show each scenario's total cumulative cost. Dynamic subheader shows the gap between Updated NRI and Current Plan, plus an annual diversion figure at a user-selectable year.

**Tab B — Projected APS Revenue Coming from Closed TADs** Per-TAD line chart showing projected annual APS revenue starting from each TAD's closure year. Responds to sliders and projection method. Cross-filters with map and historic chart. Start-of-line labels for four individually named TADs (Beltline, Eastside, Westside, Perry Bolton); the four corridor TADs (Campbellton, Metropolitan, Stadium, Hollowell) are unlabeled (hover-only). Dynamic subheader shows a Beltline revenue example at a user-selectable year.

### 2. What Could This Fund? ✅ *(unit costs need updating — see outstanding items)*

Cards showing what the projected annual APS revenue from closed TADs (in 2035) could purchase: pre-K teachers, school lunches, electric buses, playgrounds, neighborhood schools saved. Responds to the scenario sliders.

### 3. Map + Historic Property Values (side by side) ✅ *(map still has items outstanding)*

- **Left:** TAD boundary map of Atlanta. Clicking a TAD cross-filters both charts. City of Atlanta boundary shown as mid-gray polygon. TAD polygons color-coded. Labels use `st_point_on_surface` with manual overrides for Beltline (NE arc) and Atlantic Station (nudged north). Major roads (interstates + primary) shown in white from TIGER/Line shapefiles.
- **Right:** Historic assessed property values by TAD, 2007–2024. Per-year tooltips via `geom_point_interactive`. Tooltips display in billions when value ≥ \$1B, millions otherwise.

------------------------------------------------------------------------

## What's Been Built / Checked Off

**Infrastructure** - \[x\] Shiny app scaffolded with `{bslib}` layout - \[x\] Real data loaded from `TAD Basics.csv` (historic property values 2007–2024, baselines, closure years for all three scenarios) - \[x\] TAD boundary shapefiles sourced and integrated - \[x\] City of Atlanta boundary shapefile sourced and integrated - \[x\] TIGER/Line road shapefiles (Fulton, DeKalb, Cobb, Clayton, Henry, Douglas, Cherokee) sourced and integrated - \[x\] Consistent TAD color palette applied across all graphics - \[x\] Custom `theme_tad()` ggplot2 theme function - \[x\] `reactiveVal` cross-filtering wired across all charts and map - \[x\] Blank-space click to deselect (JavaScript + Shiny input) - \[x\] "× Show all" deselect link in map card header - \[x\] `precompute.R` one-time script generates RDS cache files for expensive startup objects (`roads_sf`, `proj_list`, `diversion_list`); `data.R` loads from cache with `file.exists()` fallback for cold start

**Scenario Controls** - \[x\] Per-TAD closure year sliders with tick marks showing preset positions - \[x\] Three preset buttons with active-state highlighting (filled when selected, outlined when not) - \[x\] Preset buttons reactive even before the custom slider accordion is opened (fallback logic in `closure_years`) - \[x\] Projection methodology radio buttons (4 options including TAD baseline CAGR) - \[x\] Closure years for all three scenarios in `tad_meta` - \[x\] "About These Plans" collapsible sidebar section explaining the three scenarios (what each one proposes and why they differ) - \[x\] Eastside PILOT exception modeled: per-TAD APS participation % — hardcoded per scenario (Eastside = 100% under Current Plan and Original NRI, 0% under Updated NRI); custom mode exposes per-TAD sliders; `compute_diverted()` and `proj_chart` both reflect PILOT rates

**Charts** - \[x\] Historic property value chart with smart B/M tooltip formatting - \[x\] TAD map with city boundary, roads, colored polygons, and labels - \[x\] Projected APS revenue chart — active TADs only, start-of-line labels for 4 named TADs, empty-state fallback when no TADs close before 2055 - \[x\] Revenue diverted chart — on-curve labels via `geomtextpath`, end-of-line cumulative totals, no legend - \[x\] "What Could This Fund?" panel - \[x\] Both revenue charts share a `navset_card_tab` card - \[x\] Both charts start at 2030 (no pre-2030 display); cumulative diversion resets to zero at 2030 - \[x\] Diversion chart suppresses `$0B` y-axis label to avoid overlap with the 2030 x-axis label - \[x\] Lighter pre-closure line segments on projection chart for Eastside PILOT period

**Dynamic Subheaders** - \[x\] Diversion chart: growth assumption name, cumulative gap vs. current plan (from 2030–2055), annual diversion at user-selectable year with inline picker - \[x\] Projected revenue chart: Beltline 2035 (or user-selected year) revenue example with inline picker, falls back gracefully when Beltline hasn't closed yet under the active scenario.

**Relevant Links tab** - \[x\] NRI Information subsection with 4 links: Mayor's Updated NRI Legislation, Original NRI Legislation, Draft NRIC Final Report, NRI Website (Atlanta Neighborhoods) - \[x\] APS data, Georgia Education Finance, Geospatial data, and Research subsections (duplicate Georgia Education Finance divs removed)

------------------------------------------------------------------------

## Outstanding Items

- [x] **Additional county road shapefiles** — added Cobb, Clayton, Henry, Douglas, and Cherokee TIGER/Line files. All 7 counties now loaded, filtered to S1100 + S1200, merged and reprojected. Replaced `st_read()` calls with RDS cache (`roads_sf.rds`) for fast startup.
- [x] **Map fills container** — remove white space between the gray map background and the card border so the map bleeds edge-to-edge within its card.
- [x] **Map card header polish** — "Click a district to highlight" subtext feels cluttered; consider removing or making it smaller/muted.

### Growth Assumption & Projection Methodology

- [x] **Fix the citywide growth rate** — now uses the **Atlanta column** in `TAD Basics.csv` (total City of Atlanta property value from the Fulton County tax digest) rather than the TAD aggregate.
- [x] **Redefine the "Optimistic" scenario** — now uses the average CAGR of the three demonstrably high-growth TADs (Atlantic Station, Beltline, Eastside) rather than the 75th percentile.
- [x] **Add growth rate explainers** — in-UI description of what CAGR is, how each method is computed, and what the options represent. Could live as a small accordion or tooltip beneath the sidebar growth buttons.
- [ ] **Dual-phase growth rate model** — projection scenario where each TAD uses its historical CAGR while open, then switches to citywide average after closure. Requires updating `build_projections()` and adding a new sidebar option.
- [x] **Eastside TAD PILOT exception — per-TAD APS participation % slider** — implemented. Hardcoded per scenario (Eastside = 100% under Current Plan and Original NRI, 0% under Updated NRI since Updated NRI dropped the Beltline and Perry Bolton TADs but also removed the Eastside PILOT arrangement). Custom mode exposes per-TAD participation sliders. `compute_diverted()` accepts a `pilot_df` argument; projection chart renders lighter pre-closure line segments for PILOT-paying TADs.

### "Why TAD Closures Matter" Panel

- [x] Vision cards: Universal Pre-K (\$78.2M), Free MARTA (\$27.7M), Teacher salaries to \$100K (\$34.6M) — all with methodology accordions and sources
- [x] Challenge cards: Healthcare costs (+\$127M/+78%), Property tax revenue at risk (\~3% cap / SB 33), Declining enrollment (2,398 students by 2030)
- [x] Two-section layout: "What Becomes Possible" (teal) and "What's Already at Risk" (amber)
- [x] **"At Risk" card flip / more info** — descriptions are currently long. Explore a card-flip or "more info" expand so each card shows 1–2 sentences by default with full detail on interaction.
- [ ] **Accordion padding** — the methodology accordions (Pre-K, MARTA, Teacher Salary) have too much vertical padding; tighten to reduce whitespace.

### Charts — Total Revenue Diverted

- [x] Tab renamed to "Total Revenue Diverted from Schools"
- [x] Y-axis drops decimal (\$2B not \$2.0B)
- [x] On-curve labels enlarged
- [x] Chart fills container width
- [x] **2055 vertical line** — add a dotted vertical line at 2055 (amber/orange) with label "Extended TADs close / (NRI Proposals, 2055)" marking where the Mayor's proposals finally close all TADs.
- [ ] **Tab header styling** — current teal color on active tab feels off; revisit to match overall color scheme.

### Charts — Projected Annual APS Revenue

- [x] **Tab title** — rename to "Projected Annual APS Revenue from Closed TADs" (add "Annual").
- [ ] **Chart polish** — stretch chart height/width, consider removing or simplifying the y-axis label.
- [ ] **Tab header styling** — same as above; revisit active tab color.

### Charts — Historic Property Values

- [x] **Remove "value" y-axis label** — redundant given context; clean up the chart margin.

### Explainer & Sources

- [x] **Growth rate explainer** — see Growth Assumption section above.
- [x] **Learn More / Relevant Links tab** — NRI Information, APS data, Georgia Education Finance, Geospatial data, and Research subsections with curated links. NRI Information includes Mayor's Updated NRI Legislation, Original NRI Legislation, Draft NRIC Final Report, and NRI Website.
- [ ] **Feedback section** — small section inviting users to send feedback (email link or simple form).

### QA & Verification *(before public launch)*

- [ ] **Dynamic text spot-check** — audit every reactive dollar figure and number that appears in subheaders, card labels, and inline text. Verify each one against a manual calculation using known inputs (e.g., set all sliders to Current Plan, select Historic TAD Growth, and hand-check the subheader gap figure, the annual diversion number, and the Beltline 2035 revenue example).
- [ ] **Chart data point verification** — hover over specific data points on each chart and cross-reference against the raw CSV and projection model. Priority: cumulative diversion endpoints at 2055, first-year revenue figures for each TAD after closure, historic property values for 2024.
- [x] **Vision card cost figures** — re-verify the three static cost estimates (Pre-K \$78.2M, MARTA \$27.7M, Teacher Salaries \$34.6M) once any enrollment, salary, or rate data is updated.
- [ ] **Scenario consistency** — confirm that preset buttons (Current Plan, Mayor's Original NRI, Mayor's Updated NRI) produce the same outputs across all three charts. The diversion chart uses fixed scenario dates; the projection chart uses sliders — verify they agree when sliders are set to a matching preset.
- [ ] **Edge cases** — test slider extremes (all TADs set to 2055, all set to 2025), custom growth rates at 0% and 15%, and the custom closure panel with mixed years. Confirm no chart errors or blank states appear unexpectedly.

### Phase 4 — Design Polish

- [x] Sidebar preset button active states (filled color when selected)
- [x] Growth rate buttons styled as stacked toggle group
- [x] Custom TAD closure and custom growth rate collapse panels
- [ ] **Overall visual design brainstorm** — review color scheme, fonts, background colors. Look at Atlanta-specific palette inspiration (not MARTA colors — too on the nose). Consider other interactive infographics/dashboards for reference.
- [ ] **Typography hierarchy** — ensure headers, callout numbers, and chart labels are visually distinct at a glance.
- [ ] **Responsive layout check** at 1280px.
- [ ] **Page-level TAD explainer** — brief intro text about what TADs are and why this app exists; currently a stub in the header.

### Phase 5 — Social Media / IG Slides *(narrative taking shape)*

**Planned slide series — working titles:**

1.  **"Extending the TADs Will Imperil APS"** — lead with the stakes. Cumulative diversion chart, big number, stark framing.

2.  **"Why APS Needs the TADs to Close On Time"** — structural pressures: benefits costs up 78%, property tax revenue now legally capped at inflation, declining enrollment forcing school closures. APS is already in a hole; TAD revenue is not optional.

3.  **"Closing the TADs Unlocks the Future for APS"** — the flip side. Revenue projections under the current plan, show the trajectory once TADs close.

4.  **"What APS Could Do with Revenue from Closed TADs"** — Universal Pre-K for 3 & 4-year-olds (APS Board goal), \$100K average teacher salaries, Free MARTA for every APS student. Peer cities already doing this: DC, SF, Seattle. This isn't a wish list — it's a plan.

5.  **"APS Is Already at Huge Financial Risk"** — benefits costs skyrocketing, property tax revenue constrained by SB 33, enrollment declining. The system is under pressure from every direction. TAD revenue arriving on schedule is the difference between managing it and a crisis.

6.  **"Investing in Schools, Teachers, and Students — or Private Developers?"** — the narrative reframe. Great schools draw families into the city. Properly paid teachers can live where they teach. Universal Pre-K for 3 & 4-year-olds sets kids up for academic success and saves parents thousands in childcare — freeing them to work and putting money back in their pockets. That *is* economic development. Stop pretending it isn't.

7.  **"Don't Fall for the Equity-Washing"** — the Mayor's office is using equity language to justify NRI extensions. Real inequities exist in Atlanta — no one disputes that. But the answer isn't taking from schools to fund developer projects. Go to the voters with a bond proposal. Pass a new tax. Get creative. Stop raiding the schools.

**Production checklist:** - \[ \] Finalize slide content and exact stats for each - \[ \] Design slide template: large stat or headline, minimal supporting text, consistent brand palette - \[ \] Decide on Atlanta-inspired color palette (not MARTA — too on the nose) - \[ \] Generate programmatically with `{ggplot2}` + `{camcorder}` where charts are involved; static slides in design tool or R - \[ \] Export at 1080×1080 (Instagram) and 1200×628 (Bluesky / Twitter card)

------------------------------------------------------------------------

### Phase 6 — Performance, Refactor & Documentation *(after feature completion)*

**Performance optimization:** - \[ \] **Profile slow reactives** — use `profvis::profvis({ shiny::runApp("posts/shiny-practice/tad_app") })` to identify which reactive expressions take longest. Prime suspects: shapefile rendering on the map (re-renders on every `selected_tad` change), custom growth rate projection (recomputes on every slider move). - \[ \] **Debounce custom sliders** — wrap high-frequency slider inputs (closure year and growth rate sliders) in `debounce()` so projections only recompute after the user stops dragging, not on every tick. Reduces server load significantly. - \[ \] **Cache shapefile transforms** — the map renders `tad_sf`, `roads_sf`, and `city_sf` into SVG on every click interaction. Pre-computing and storing the base map layers (city boundary + roads) as a static rendered element could reduce per-interaction render time. - \[x\] **Pre-compute expensive startup objects** — `precompute.R` (one-time local script) generates `roads_sf.rds`, `proj_list.rds`, and `diversion_list.rds` in a `precomputed/` directory inside the app folder. `data.R` loads these at startup via `readRDS()` with a `file.exists()` fallback that re-computes on the fly if the cache is missing. Run `source("posts/shiny-practice/tad_app/precompute.R")` from project root before each deploy. - \[ \] **shinyapps.io instance sizing** — free tier uses a small instance. If load times are slow under real traffic, evaluate whether upgrading to a larger instance (or Posit Connect) is worthwhile. - \[ \] **Debounce custom sliders** — wrap high-frequency slider inputs in `debounce()` so projections only recompute after the user stops dragging.

**Code refactor:** - \[ \] **Light code refactor** — consider splitting stable/dense logic out of `app.R` into sourced files (e.g., `R/data.R`, `R/projections.R`, `R/theme.R`). Current file is \~2,000+ lines; threshold for splitting is \~2,500–3,000 or when finding specific logic becomes friction. - \[ \] **Code explainer document** — written guide to how the app fits together: the reactive graph, the cross-filtering pattern, the projection model, the ggiraph interactivity approach, the Bootstrap JS patterns used for the sidebar controls. Audience: future-you or a collaborator picking this up cold. - \[ \] **Inline comment pass** — ensure all non-obvious decisions have explanatory comments, particularly the JS/Shiny input wiring, the `closure_years()` fallback logic, and the custom growth rate reactive.

------------------------------------------------------------------------

## Technical Stack

| Layer | Tool | Notes |
|------------------------|------------------------|------------------------|
| App framework | R / Shiny | Single-file `app.R` |
| UI layout | `{bslib}` | Bootstrap 5; `page_sidebar`, `navset_card_tab`, `layout_columns`, `card` |
| Charts | `{ggplot2}` + `{ggiraph}` | `ggiraph` adds click/hover via SVG-native interactive geoms |
| On-curve text labels | `{geomtextpath}` | `geom_textline()` for scenario labels on the diversion chart |
| Map | `{ggplot2}` + `geom_sf` + `{ggiraph}` | Programmatic basemap — no tile dependencies |
| Spatial data | `{sf}` | All shapefiles read with `st_read`, reprojected to WGS84 |
| Data wrangling | `{tidyverse}` |  |
| Custom theme | `theme_tad()` function | Defined once, applied to all charts |
| Static export / social slides | `{ggplot2}` + `{camcorder}` | Phase 5 |
| Hosting | shinyapps.io (free tier) | Deploy: `rsconnect::deployApp("posts/shiny-practice/tad_app")` |

------------------------------------------------------------------------

## Architecture Notes

**Cross-filtering:** A single `reactiveVal` called `selected_tad` stores the currently highlighted TAD (or `NULL` for all). Map clicks, chart line clicks, and a "× Show all" link all write to this value. All three charts read it and re-render automatically via Shiny's reactive graph. Blank-space clicks inside ggiraph containers are caught by a custom JavaScript listener that fires a `bg_click` Shiny input.

**Dynamic UI:** Per-TAD closure year sliders are built in `renderUI` (server-side). Preset buttons use `updateSliderInput` to batch-update all sliders. An `active_preset` reactiveVal tracks which preset (if any) is currently active; a custom JS message handler (`setActivePreset`) toggles Bootstrap button classes to show filled/outline state.

**Preset button reactivity before slider render:** The `closure_years` reactive reads slider inputs, but sliders don't exist in the DOM until the custom accordion is opened. To ensure preset buttons update charts immediately (without requiring the user to open the accordion), `closure_years` checks `active_preset()` and uses the corresponding `tad_meta` column as a fallback when `input[[slider_id]]` is `NULL`.

**Slider drift detection:** An `observe()` reads all slider values and compares them against the active preset's expected values. If any differ, `active_preset(NULL)` is set, which the JS handler uses to revert all buttons to outline state.

**Diversion chart vs. projection chart:** The projection chart responds to the sliders (custom closure dates). The diversion chart always shows the three named scenarios with their fixed closure dates from `tad_meta` — this keeps it as a clean comparison tool independent of slider state.

**Active TAD filtering:** The projection chart explicitly excludes Atlantic Station and Princeton Lake (confirmed closed) by name rather than relying on the `already_closed` flag, which had data-ambiguity issues with Stadium TAD.

------------------------------------------------------------------------

## Solved Technical Problems

### ggiraph SVG clipping (`clip = "off"` doesn't work)

**Problem:** ggiraph renders ggplots as SVG and clips everything to the panel bounds, regardless of `clip = "off"` in `coord_cartesian`. Annotations outside the panel (bracket labels, end-of-line text) were being silently dropped.

**Solution:** Extend `coord_cartesian(xlim)` to cover the annotation zone, then add an `annotate("rect", xmin = 2055, xmax = Inf, fill = "white")` layer to mask gridlines in the extended zone. This keeps annotations within the SVG bounds while hiding the visual clutter of extended gridlines.

### Dynamic subheader line breaks

**Problem:** Building subheader text with Shiny's `p()` + `strong()` + `tagList()` produced unwanted line breaks between elements. Shiny's HTML serializer adds newline characters between sibling tag nodes, which some browsers render as visible whitespace.

**Solution:** Build the entire subheader as a single flat HTML string using `sprintf()` + `HTML()`. Text formatting (`<strong>`, `<br>`) is embedded directly in the string. The only non-string element (the inline year picker) is also built as an HTML string via `sprintf`, so everything concatenates cleanly with no tag-node boundaries.

### Inline year picker

**Problem:** Wanted a year selector embedded inline within subheader text that looks like part of the sentence (not a form field). Several approaches failed: - **Shiny `selectInput`** — the `.form-group` wrapper and browser-native styling looked janky despite CSS overrides - **Bootstrap dropdown with `data-bs-toggle`** — Bootstrap 5 doesn't auto-initialize dropdowns on dynamically-injected content (added via `renderUI`), so the menu never opened - **Custom CSS/JS dropdown** — JS toggle fired but `renderUI` re-renders on every reactive change, destroying the `.open` class before the menu became visible

**Solution:** A native HTML `<select>` element built directly into the HTML string, with `onchange="Shiny.setInputValue('ref_year', parseInt(this.value), {priority:'event'})"`. Native `<select>` always works, opens reliably, and `onchange` fires before any Shiny re-render. CSS removes the border, adds a bottom underline, and matches the surrounding text weight/color. Two separate input IDs (`ref_year` for the projected revenue card, `ref_year_div` for the diversion card) prevent the two pickers from interfering.

### Scenario string matching with apostrophes

**Problem:** Filtering `diversion_data()` by scenario name using `==` failed silently when Claude's text editor introduced curly apostrophes (`'`) into string literals that the source data stored with straight apostrophes (`'`). The comparison returned zero rows, making all dynamic dollar values blank.

**Solution:** Use `grepl(..., fixed = TRUE)` for all scenario lookups (e.g., `grepl("Updated NRI", dd$scenario, fixed = TRUE)`). This is substring-based and encoding-agnostic, so it doesn't depend on exact apostrophe type.

### On-curve text labels (diversion chart)

**Problem:** The three cumulative scenario lines needed labels that felt part of the chart, not a separate legend or cluttered end-of-line callouts (which already showed cumulative totals).

**Solution:** `geomtextpath::geom_textline()`, which draws both the line and its label in a single layer, with the text rotated to follow the line's slope. Key parameters: - `offset = unit(5, "pt")` shifts labels above the line; negative offset for the bottom line ("Current Plan") places it below - `gap = FALSE` keeps the line continuous beneath the text (setting `gap = TRUE` cut the line, which looked broken) - `offset` is not a mappable aesthetic, so two separate `geom_textline` calls are needed — one for Current Plan (negative offset) and one for the NRI scenarios (positive offset), using `data = ~ filter(.x, ...)` lambda syntax - A `geom_line_interactive(alpha = 0)` layer sits on top to preserve ggiraph hover events on the line

### precompute.R — Positron working directory issues

**Problem:** Running `precompute.R` from Positron's console hit a chain of working directory failures. Positron monkey-patches `source()`, wrapping it as `"original_source"`, so `setwd()` calls inside a sourced script don't persist after the call returns. Several approaches failed: `setwd()` before `source()`, `setwd()` + `on.exit()`, and `source(..., chdir = TRUE)` (which sets wd to the script's directory `R/`, not the app root, so relative paths like `TAD Basics.csv` still didn't resolve).

**Solution:** `withr::with_dir(app_dir, source("R/data.R"))` — sets wd to the app root for the duration of the `source()` call, regardless of Positron's wrapping. The `app_dir` path is computed via `normalizePath(file.path(getwd(), "posts/shiny-practice/tad_app"))` rather than `rstudioapi::getSourceEditorContext()$path` (not available in Positron).

### precompute.R — chicken-and-egg RDS problem

**Problem:** After the working directory was fixed, `withr::with_dir()` successfully sourced `data.R` — but `data.R` now calls `readRDS("precomputed/proj_list.rds")` at the top level, which doesn't exist on the first run (the whole point of `precompute.R` is to create it). This caused a "cannot open compressed file" error, preventing `precompute.R` from ever generating the cache files.

**Solution:** Wrapped all three `readRDS()` calls in `data.R` with `file.exists()` fallbacks. If the `.rds` file exists, load it; otherwise compute from scratch. This makes `data.R` self-contained for both cold-start (no cache) and warm-start (cache present) scenarios. `precompute.R` then runs cleanly: sources `data.R` (which computes fresh), then saves the results to `precomputed/`.

### Projected revenue chart — empty state

**Problem:** Under Mayor's Original NRI, all TADs close after 2055, leaving `per_tad` empty. An empty ggplot with `scale_y_continuous(limits = c(0, NA))` errors because it can't determine an upper bound from no data.

**Solution:** Early-return guard: `if (nrow(per_tad) == 0)` returns a styled empty chart with a brief annotation ("No TADs close before 2055 under this scenario") and fixed y-axis limits, rather than allowing the render to error.

------------------------------------------------------------------------

## Projection Methodology (as implemented)

**CAGR approach:** For each TAD, compute a Compound Annual Growth Rate from the first to last observed assessed value. Project forward by applying that rate from the last known year to 2055.

Four selectable methods (radio button in scenario controls):

| Method | How it works | Status |
|------------------------|------------------------|------------------------|
| TAD 2007–2024 CAGR | Each TAD uses its own CAGR from 2007 to 2024 | ✅ Built |
| TAD Baseline–2024 CAGR | Each TAD uses its own CAGR from its creation year / baseline value to 2024 | ✅ Built |
| Citywide average | All TADs use the CAGR of the Atlanta column (total City property value from Fulton County tax digest) | ✅ Built |
| Optimistic | All TADs use the average CAGR of Atlantic Station, Beltline, and Eastside | ✅ Built |

**APS revenue formula:** `aps_annual_revenue = (projected_value - baseline) × APS_MILLAGE / 1000` where `APS_MILLAGE = 20.74` mills (assumed constant; update if rate changes).

**Diversion formula:** Same as APS revenue formula, but applied only to years when the TAD is *still open*.

------------------------------------------------------------------------

## Data Sources

| Data | Source | Status |
|------------------------|------------------------|------------------------|
| Historic TAD property values (2007–2024) | `TAD Basics.csv` — sourced from Invest Atlanta / Fulton County tax digest | ✅ In app |
| TAD baseline values | Same CSV | ✅ In app |
| TAD closure years — all three scenarios | Same CSV | ✅ In app |
| TAD boundary shapefiles | City of Atlanta Open Data | ✅ In app |
| City of Atlanta boundary | `Official_Atlanta_City_Limits_-_Open_Data.shp` | ✅ In app |
| Road centerlines (7 counties: Fulton, DeKalb, Cobb, Clayton, Henry, Douglas, Cherokee) | US Census TIGER/Line 2023 | ✅ In app (cached as `roads_sf.rds`) |
| APS millage rate | Assumed 20.74 mills | ⚠️ Needs verification |
| Eastside TAD PILOT payments | Invest Atlanta / APS | ⬜ Not yet incorporated |
| Unit costs for "What Could This Fund?" | Various — see outstanding items | ⚠️ Needs update |