# TAD Shiny App — Project Plan

*Last updated: June 2026*

## Overview

An interactive R/Shiny application visualizing Tax Allocation District (TAD) property values and their fiscal impact on Atlanta Public Schools (APS). The app connects five linked graphics enabling users to explore how TAD closure timelines affect school funding, and what that revenue could otherwise pay for.

**Audience:** Public advocacy and portfolio demonstration. Design and copy should be accessible to a general public audience, not just policy wonks.

---

## Background: How TAD Revenue Works

A TAD freezes the property tax base ("the base") at the moment it's created. All property tax revenue generated *above* that base — the **increment** — flows to Invest Atlanta (the City's development arm) for the life of the TAD, rather than to APS, the City, or Fulton County. Only when a TAD closes does the full increment return to the general tax digest, at which point APS receives its share (roughly half, with the remainder split between the City and County).

**Exception — Eastside TAD:** Although not yet closed, APS currently receives the full increment from the Eastside TAD via PILOT (Payment in Lieu of Taxes) payments. This is noted in the data but not yet fully reflected in the projection model.

This dynamic — and how long TADs stay open — is the central policy question the app explores.

---

## Current App Layout (as built)

Cards appear in this order, top to bottom:

### 1. Revenue Diverted from Schools & Kids / Projected APS Revenue — tabbed card ✅
A `navset_card_tab` card with two tabs:

**Tab A — Revenue Diverted from Schools & Kids**
Cumulative line chart showing the total APS property tax revenue redirected to Invest Atlanta over time, under each of three fixed scenarios: Current Plan, Mayor's Original NRI, Mayor's Updated NRI. Three colored lines. Does not respond to sliders — always shows the three named scenarios for direct comparison. Scenario names are labeled directly on the curves using `geomtextpath::geom_textline()` (no legend). End-of-line labels at 2055 show each scenario's total cumulative cost. Dynamic subheader shows the gap between Updated NRI and Current Plan, plus an annual diversion figure at a user-selectable year.

**Tab B — Projected APS Revenue Coming from Closed TADs**
Per-TAD line chart showing projected annual APS revenue starting from each TAD's closure year. Responds to sliders and projection method. Cross-filters with map and historic chart. Start-of-line labels for four individually named TADs (Beltline, Eastside, Westside, Perry Bolton); the four corridor TADs (Campbellton, Metropolitan, Stadium, Hollowell) are unlabeled (hover-only). Dynamic subheader shows a Beltline revenue example at a user-selectable year.

### 2. What Could This Fund? ✅ *(unit costs need updating — see outstanding items)*
Cards showing what the projected annual APS revenue from closed TADs (in 2035) could purchase: pre-K teachers, school lunches, electric buses, playgrounds, neighborhood schools saved. Responds to the scenario sliders.

### 3. Map + Historic Property Values (side by side) ✅ *(map still has items outstanding)*
- **Left:** TAD boundary map of Atlanta. Clicking a TAD cross-filters both charts. City of Atlanta boundary shown as mid-gray polygon. TAD polygons color-coded. Labels use `st_point_on_surface` with manual overrides for Beltline (NE arc) and Atlantic Station (nudged north). Major roads (interstates + primary) shown in white from TIGER/Line shapefiles.
- **Right:** Historic assessed property values by TAD, 2007–2024. Per-year tooltips via `geom_point_interactive`. Tooltips display in billions when value ≥ $1B, millions otherwise.

---

## What's Been Built / Checked Off

**Infrastructure**
- [x] Shiny app scaffolded with `{bslib}` layout
- [x] Real data loaded from `TAD Basics.csv` (historic property values 2007–2024, baselines, closure years for all three scenarios)
- [x] TAD boundary shapefiles sourced and integrated
- [x] City of Atlanta boundary shapefile sourced and integrated
- [x] TIGER/Line road shapefiles (Fulton + DeKalb) sourced and integrated
- [x] Consistent TAD color palette applied across all graphics
- [x] Custom `theme_tad()` ggplot2 theme function
- [x] `reactiveVal` cross-filtering wired across all charts and map
- [x] Blank-space click to deselect (JavaScript + Shiny input)
- [x] "× Show all" deselect link in map card header

**Scenario Controls**
- [x] Per-TAD closure year sliders with tick marks showing preset positions
- [x] Three preset buttons with active-state highlighting (filled when selected, outlined when not)
- [x] Preset buttons reactive even before the custom slider accordion is opened (fallback logic in `closure_years`)
- [x] Projection methodology radio buttons (4 options including TAD baseline CAGR)
- [x] Closure years for all three scenarios in `tad_meta`

**Charts**
- [x] Historic property value chart with smart B/M tooltip formatting
- [x] TAD map with city boundary, roads, colored polygons, and labels
- [x] Projected APS revenue chart — active TADs only, start-of-line labels for 4 named TADs, empty-state fallback when no TADs close before 2055
- [x] Revenue diverted chart — on-curve labels via `geomtextpath`, end-of-line cumulative totals, no legend
- [x] "What Could This Fund?" panel
- [x] Both revenue charts share a `navset_card_tab` card

**Dynamic Subheaders**
- [x] Diversion chart: growth assumption name, cumulative gap vs. current plan, annual diversion at user-selectable year with inline picker
- [x] Projected revenue chart: Beltline 2035 (or user-selected year) revenue example with inline picker, falls back gracefully when Beltline hasn't closed yet under the active scenario

---

## Outstanding Items

### Sliders
- [ ] **Tick marks on closure year sliders** — previous attempt used JavaScript to inject marks on the ion.rangeSlider track, but it wasn't working reliably. Either find a cleaner JS approach, use a custom slider widget, or document a simpler fallback (e.g., a small reference table showing the three years per TAD).

### Map
- [ ] **Additional county road shapefiles** — currently only Fulton + DeKalb. Roads from Cobb, Clayton, and Henry counties cut off at county lines within the visible map extent. Add those TIGER/Line files and re-filter to S1100 + S1200.
- [ ] **Further map polish** — label positions, polygon styling, and surrounding area context can still be refined.

### Growth Assumption & Projection Methodology

- [x] **Fix the citywide growth rate** — now uses the **Atlanta column** in `TAD Basics.csv` (total City of Atlanta property value from the Fulton County tax digest) rather than the TAD aggregate.

- [x] **Redefine the "Optimistic" scenario** — now uses the average CAGR of the three demonstrably high-growth TADs (Atlantic Station, Beltline, Eastside) rather than the 75th percentile.

- [ ] **Dual-phase growth rate model** — a new projection scenario where each TAD uses its historical CAGR while open (TAD investment driving faster growth), then switches to citywide average CAGR after closure (reflecting slower, stabilized growth once development incentives end). Requires updating `build_projections()` to accept per-TAD closure years and a post-closure rate override, then wiring up a new radio button option.

- [ ] **Add a growth rate table as an accordion** — an expandable section showing each TAD's projected growth rate under each scenario. Rows = projection methods, columns = TAD names. Serves as methodology explainer.

- [ ] **Add explainer text** describing the projection methodology: what CAGR is, how it's computed per TAD, and what the four options represent.

- [ ] **Eastside TAD PILOT exception** — should be noted in the UI and may affect revenue calculations; not yet fully handled.

### "What Could This Fund?" Panel
- [ ] **Fix reactivity — decouple from sliders.** The panel should always show what could be funded under the *current planned closure timeline*, not the slider state. Replace `closure_years()` dependency with the fixed `diversion_scenarios[["Current Plan"]]` closure dates.
- [ ] **Teacher cost** — update to reflect APS's stated goal of $100k average annual salary by 2030, plus benefits (typically 30–35%). Current placeholder is too low.
- [ ] **Add infrastructure line items** — e.g., sidewalks and bike lanes near schools.
- [ ] **Review all other items** — verify free school lunch cost against current USDA data, check electric bus cost against recent Atlanta/Georgia purchases, update playground cost range.
- [ ] **Add source citations** for each unit cost, ideally linkable.

### Explainer & Sources
- [ ] **In-card explainer functionality** — collapsible `bslib::accordion` panels with data source, methodology, and caveats for each chart.
- [ ] **Footnotes section** at page bottom with numbered references for all charts and data sources.

### Phase 4 — Design Polish *(partially complete)*
- [x] Sidebar preset button active states
- [ ] Typography hierarchy — ensure headers, labels, and callout numbers are visually distinct
- [ ] Responsive layout check at 1280px
- [ ] Tighten chart whitespace and legend placement
- [ ] Add brief page-level explainer text about what TADs are

### Phase 5 — Social Media Takeaways *(not yet started)*
- [ ] Identify 4–6 headline findings
- [ ] Design slide template: large stat, minimal text, consistent brand
- [ ] Generate programmatically with `{ggplot2}` + `{camcorder}`
- [ ] Export at 1080×1080 (Instagram) and 1200×628 (Bluesky / Twitter card)

---

## Technical Stack

| Layer | Tool | Notes |
|---|---|---|
| App framework | R / Shiny | Single-file `app.R` |
| UI layout | `{bslib}` | Bootstrap 5; `page_sidebar`, `navset_card_tab`, `layout_columns`, `card` |
| Charts | `{ggplot2}` + `{ggiraph}` | `ggiraph` adds click/hover via SVG-native interactive geoms |
| On-curve text labels | `{geomtextpath}` | `geom_textline()` for scenario labels on the diversion chart |
| Map | `{ggplot2}` + `geom_sf` + `{ggiraph}` | Programmatic basemap — no tile dependencies |
| Spatial data | `{sf}` | All shapefiles read with `st_read`, reprojected to WGS84 |
| Data wrangling | `{tidyverse}` | |
| Custom theme | `theme_tad()` function | Defined once, applied to all charts |
| Static export / social slides | `{ggplot2}` + `{camcorder}` | Phase 5 |
| Hosting | Posit Connect | |

---

## Architecture Notes

**Cross-filtering:** A single `reactiveVal` called `selected_tad` stores the currently highlighted TAD (or `NULL` for all). Map clicks, chart line clicks, and a "× Show all" link all write to this value. All three charts read it and re-render automatically via Shiny's reactive graph. Blank-space clicks inside ggiraph containers are caught by a custom JavaScript listener that fires a `bg_click` Shiny input.

**Dynamic UI:** Per-TAD closure year sliders are built in `renderUI` (server-side). Preset buttons use `updateSliderInput` to batch-update all sliders. An `active_preset` reactiveVal tracks which preset (if any) is currently active; a custom JS message handler (`setActivePreset`) toggles Bootstrap button classes to show filled/outline state.

**Preset button reactivity before slider render:** The `closure_years` reactive reads slider inputs, but sliders don't exist in the DOM until the custom accordion is opened. To ensure preset buttons update charts immediately (without requiring the user to open the accordion), `closure_years` checks `active_preset()` and uses the corresponding `tad_meta` column as a fallback when `input[[slider_id]]` is `NULL`.

**Slider drift detection:** An `observe()` reads all slider values and compares them against the active preset's expected values. If any differ, `active_preset(NULL)` is set, which the JS handler uses to revert all buttons to outline state.

**Diversion chart vs. projection chart:** The projection chart responds to the sliders (custom closure dates). The diversion chart always shows the three named scenarios with their fixed closure dates from `tad_meta` — this keeps it as a clean comparison tool independent of slider state.

**Active TAD filtering:** The projection chart explicitly excludes Atlantic Station and Princeton Lake (confirmed closed) by name rather than relying on the `already_closed` flag, which had data-ambiguity issues with Stadium TAD.

---

## Solved Technical Problems

### ggiraph SVG clipping (`clip = "off"` doesn't work)

**Problem:** ggiraph renders ggplots as SVG and clips everything to the panel bounds, regardless of `clip = "off"` in `coord_cartesian`. Annotations outside the panel (bracket labels, end-of-line text) were being silently dropped.

**Solution:** Extend `coord_cartesian(xlim)` to cover the annotation zone, then add an `annotate("rect", xmin = 2055, xmax = Inf, fill = "white")` layer to mask gridlines in the extended zone. This keeps annotations within the SVG bounds while hiding the visual clutter of extended gridlines.

### Dynamic subheader line breaks

**Problem:** Building subheader text with Shiny's `p()` + `strong()` + `tagList()` produced unwanted line breaks between elements. Shiny's HTML serializer adds newline characters between sibling tag nodes, which some browsers render as visible whitespace.

**Solution:** Build the entire subheader as a single flat HTML string using `sprintf()` + `HTML()`. Text formatting (`<strong>`, `<br>`) is embedded directly in the string. The only non-string element (the inline year picker) is also built as an HTML string via `sprintf`, so everything concatenates cleanly with no tag-node boundaries.

### Inline year picker

**Problem:** Wanted a year selector embedded inline within subheader text that looks like part of the sentence (not a form field). Several approaches failed:
- **Shiny `selectInput`** — the `.form-group` wrapper and browser-native styling looked janky despite CSS overrides
- **Bootstrap dropdown with `data-bs-toggle`** — Bootstrap 5 doesn't auto-initialize dropdowns on dynamically-injected content (added via `renderUI`), so the menu never opened
- **Custom CSS/JS dropdown** — JS toggle fired but `renderUI` re-renders on every reactive change, destroying the `.open` class before the menu became visible

**Solution:** A native HTML `<select>` element built directly into the HTML string, with `onchange="Shiny.setInputValue('ref_year', parseInt(this.value), {priority:'event'})"`. Native `<select>` always works, opens reliably, and `onchange` fires before any Shiny re-render. CSS removes the border, adds a bottom underline, and matches the surrounding text weight/color. Two separate input IDs (`ref_year` for the projected revenue card, `ref_year_div` for the diversion card) prevent the two pickers from interfering.

### Scenario string matching with apostrophes

**Problem:** Filtering `diversion_data()` by scenario name using `==` failed silently when Claude's text editor introduced curly apostrophes (`'`) into string literals that the source data stored with straight apostrophes (`'`). The comparison returned zero rows, making all dynamic dollar values blank.

**Solution:** Use `grepl(..., fixed = TRUE)` for all scenario lookups (e.g., `grepl("Updated NRI", dd$scenario, fixed = TRUE)`). This is substring-based and encoding-agnostic, so it doesn't depend on exact apostrophe type.

### On-curve text labels (diversion chart)

**Problem:** The three cumulative scenario lines needed labels that felt part of the chart, not a separate legend or cluttered end-of-line callouts (which already showed cumulative totals).

**Solution:** `geomtextpath::geom_textline()`, which draws both the line and its label in a single layer, with the text rotated to follow the line's slope. Key parameters:
- `offset = unit(5, "pt")` shifts labels above the line; negative offset for the bottom line ("Current Plan") places it below
- `gap = FALSE` keeps the line continuous beneath the text (setting `gap = TRUE` cut the line, which looked broken)
- `offset` is not a mappable aesthetic, so two separate `geom_textline` calls are needed — one for Current Plan (negative offset) and one for the NRI scenarios (positive offset), using `data = ~ filter(.x, ...)` lambda syntax
- A `geom_line_interactive(alpha = 0)` layer sits on top to preserve ggiraph hover events on the line

### Projected revenue chart — empty state

**Problem:** Under Mayor's Original NRI, all TADs close after 2055, leaving `per_tad` empty. An empty ggplot with `scale_y_continuous(limits = c(0, NA))` errors because it can't determine an upper bound from no data.

**Solution:** Early-return guard: `if (nrow(per_tad) == 0)` returns a styled empty chart with a brief annotation ("No TADs close before 2055 under this scenario") and fixed y-axis limits, rather than allowing the render to error.

---

## Projection Methodology (as implemented)

**CAGR approach:** For each TAD, compute a Compound Annual Growth Rate from the first to last observed assessed value. Project forward by applying that rate from the last known year to 2055.

Four selectable methods (radio button in scenario controls):

| Method | How it works | Status |
|---|---|---|
| TAD 2007–2024 CAGR | Each TAD uses its own CAGR from 2007 to 2024 | ✅ Built |
| TAD Baseline–2024 CAGR | Each TAD uses its own CAGR from its creation year / baseline value to 2024 | ✅ Built |
| Citywide average | All TADs use the CAGR of the Atlanta column (total City property value from Fulton County tax digest) | ✅ Built |
| Optimistic | All TADs use the average CAGR of Atlantic Station, Beltline, and Eastside | ✅ Built |

**APS revenue formula:** `aps_annual_revenue = (projected_value - baseline) × APS_MILLAGE / 1000`
where `APS_MILLAGE = 20.74` mills (assumed constant; update if rate changes).

**Diversion formula:** Same as APS revenue formula, but applied only to years when the TAD is *still open*.

---

## Data Sources

| Data | Source | Status |
|---|---|---|
| Historic TAD property values (2007–2024) | `TAD Basics.csv` — sourced from Invest Atlanta / Fulton County tax digest | ✅ In app |
| TAD baseline values | Same CSV | ✅ In app |
| TAD closure years — all three scenarios | Same CSV | ✅ In app |
| TAD boundary shapefiles | City of Atlanta Open Data | ✅ In app |
| City of Atlanta boundary | `Official_Atlanta_City_Limits_-_Open_Data.shp` | ✅ In app |
| Road centerlines (Fulton + DeKalb) | US Census TIGER/Line 2023 | ✅ In app |
| Road centerlines (Cobb, Clayton, Henry) | US Census TIGER/Line 2023 | ⬜ Not yet added |
| APS millage rate | Assumed 20.74 mills | ⚠️ Needs verification |
| Eastside TAD PILOT payments | Invest Atlanta / APS | ⬜ Not yet incorporated |
| Unit costs for "What Could This Fund?" | Various — see outstanding items | ⚠️ Needs update |
