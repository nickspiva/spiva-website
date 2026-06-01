# TAD Shiny App — Project Plan

*Last updated: May 2026*

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

### 1. Revenue Diverted from Schools & Kids to Invest Atlanta ✅
Cumulative line chart showing the total APS property tax revenue redirected to Invest Atlanta over time, under each of three fixed scenarios: Current Plan, Mayor's Original NRI, Mayor's Updated NRI. Three colored lines; the gap between them shows the fiscal impact of the Mayor's proposals. Does not respond to sliders — always shows the three named scenarios for direct comparison.

### 2. What Could This Fund? ✅ *(unit costs need updating — see outstanding items)*
Cards showing what the projected annual APS revenue from closed TADs (in 2035) could purchase: pre-K teachers, school lunches, electric buses, playgrounds, neighborhood schools saved. Responds to the scenario sliders.

### 3. Scenario Controls ✅
Preset buttons (Current Plan / Mayor's Original NRI / Mayor's Updated NRI) that set all TAD closure year sliders at once. Per-TAD sliders for custom simulation. Growth assumption radio buttons (TAD-specific CAGR / Citywide average / Optimistic). Slider tick marks showing preset reference points are not yet working — see outstanding items.

### 4. Map + Historic Property Values (side by side) ✅ *(map still has items outstanding)*
- **Left:** TAD boundary map of Atlanta. Clicking a TAD cross-filters both charts on the right and below. City of Atlanta boundary shown as mid-gray polygon. TAD polygons color-coded. Labels use `st_point_on_surface` with manual overrides for Beltline (NE arc) and Atlantic Station (nudged north). Major roads (interstates + primary) shown in white from TIGER/Line shapefiles (Fulton + DeKalb counties only currently).
- **Right:** Historic assessed property values by TAD, 2007–2024. Per-year tooltips via `geom_point_interactive`.

### 5. Projected Annual APS Revenue from Closed TADs ✅
Per-TAD line chart showing projected annual APS revenue starting from each TAD's closure year. Responds to sliders and projection method. Cross-filters with map and historic chart.

---

## What's Been Built / Checked Off

**Infrastructure**
- [x] Shiny app scaffolded with `{bslib}` layout
- [x] Real data loaded from `TAD Basics.csv` (historic property values 2007–2024, baselines, closure years for all three scenarios)
- [x] TAD boundary shapefiles sourced and integrated (`Tax_Allocation_District.shp`)
- [x] City of Atlanta boundary shapefile sourced and integrated
- [x] TIGER/Line road shapefiles (Fulton + DeKalb) sourced and integrated
- [x] Consistent TAD color palette applied across all graphics
- [x] Custom `theme_tad()` ggplot2 theme function
- [x] `reactiveVal` cross-filtering wired across all charts and map
- [x] Blank-space click to deselect (JavaScript + Shiny input)
- [x] "× Show all" deselect link in map card header

**Charts**
- [x] Historic property value chart (Graphic 4 in order, Graphic 1 conceptually)
- [x] TAD map with city boundary, roads, colored polygons, and labels
- [x] Projected APS revenue chart with per-TAD lines, closure year annotations, cross-filtering
- [x] Revenue diverted chart (cumulative, three fixed scenario lines)
- [x] "What Could This Fund?" panel

**Scenario controls**
- [x] Per-TAD closure year sliders
- [x] Three preset buttons wired to `tad_meta` columns
- [x] Projection methodology toggle (TAD-specific, citywide, optimistic)
- [x] Closure years for all three scenarios in `tad_meta`

---

## Outstanding Items

### Sliders
- [ ] **Tick marks on closure year sliders** — previous attempt used JavaScript to inject marks on the ion.rangeSlider track, but it wasn't working reliably. Either find a cleaner JS approach, use a custom slider widget, or document a simpler fallback (e.g., a small reference table showing the three years per TAD).

### Map
- [ ] **Additional county road shapefiles** — currently only Fulton + DeKalb. Roads from Cobb, Clayton, and Henry counties cut off at county lines within the visible map extent. Add those TIGER/Line files and re-filter to S1100 + S1200.
- [ ] **Further map polish** — label positions, polygon styling, and surrounding area context can still be refined.

### Growth Assumption & Projection Methodology

- [ ] **Fix the citywide growth rate** — currently computed as the CAGR of the *sum of all TAD values* over time, which is TAD-aggregate growth, not city-wide growth. Should instead be computed from the **Atlanta column** in `TAD Basics.csv`, which is the total City of Atlanta property value from the Fulton County tax digest. This is the more meaningful citywide baseline.

- [ ] **Add a new growth rate option: "TAD Baseline-to-2024 CAGR"** — a per-TAD CAGR computed from each TAD's *baseline value* (set at the time the TAD was created) to its 2024 assessed value, using the `Year Created` column as the starting year. This differs from the existing "TAD 2007–2024 CAGR" in that it captures the full growth history since each TAD's inception — for older TADs like Westside (created 1998), this means going back to 1998 rather than 2007, giving a longer and arguably more representative track record. Implementation note: for TADs created before 2007 (when the CSV data starts), the baseline value from `tad_meta` serves as the starting point since we don't have annual data that far back.

- [ ] **Redefine the "Optimistic" scenario** — currently uses the 75th percentile of individual TAD CAGRs, which is not a reliable upper bound and may be lower than some individual rates. Replace with a rate derived from the high-growth TADs only: compute the average (or median) CAGR across Atlantic Station, Beltline, and Eastside — the three demonstrably high-growth TADs — and apply that rate uniformly to all TADs. This gives a grounded "what if all TADs grew like the successful ones?" scenario rather than an arbitrary percentile.

- [ ] **Add a growth rate table as an accordion** — an expandable section showing each TAD's projected growth rate under each scenario. Suggested layout: rows = projection methods, columns = TAD names. Projection method rows:
  - TAD 2007–2024 CAGR (individual, existing)
  - TAD Baseline–2024 CAGR (individual, new)
  - Citywide average (same value for all TADs, fixed)
  - Optimistic — high-growth TAD average (same value for all TADs, fixed)

  This table also serves as the methodology explainer for the projection charts. Implement as a `bslib::accordion_panel` within the scenario controls card or as a standalone card below it.

- [ ] **Add explainer text** in the app describing the projection methodology: what CAGR is, how it's computed per TAD, and what the four options represent.
- [ ] **QA projections** — compare against any published APS revenue forecasts if available.
- [ ] **Eastside TAD PILOT exception** — should be noted in the UI and may affect revenue calculations; not yet fully handled.

### "What Could This Fund?" Panel
- [ ] **Fix reactivity — decouple from sliders.** The panel should always show what could be funded under the *current planned closure timeline*, not the slider state. It's meant to answer "what do we get if we just stay the course?" as a fixed reference, not a simulation tool. Practically: replace the `closure_years()` reactive dependency with the fixed `diversion_scenarios[["Current Plan"]]` closure dates. Note: the panel is also not currently reacting to sliders in practice (likely a slider initialization timing issue), so this fix aligns the behavior with the intended design.
- [ ] **Teacher cost** — update to reflect APS's stated goal of $100k average annual salary by 2030, *plus* benefits (typically 30–35% on top). Current placeholder of $85k is too low.
- [ ] **Add infrastructure line items** — e.g., sidewalks and bike lanes near schools (cost per mile or per school catchment area). Locate relevant Atlanta/APS cost estimates.
- [ ] **Review all other items** — verify free school lunch cost against current USDA reimbursement data, check electric bus cost against recent Atlanta/Georgia purchases, update playground cost range.
- [ ] **Add source citations** for each unit cost, ideally linkable.

### Explainer & Sources
- [ ] **In-card explainer functionality** — users (especially public audience) need context for each chart. Two options to evaluate:
  - **Flip card / info button:** clicking an ⓘ or "About this chart" button on each card reveals the data source, methodology, and key caveats — either as a modal, a card flip, or a collapsible section.
  - **Footnotes section:** a single section at the bottom of the page with numbered references for all charts and data sources.
  - Recommendation: start with collapsible `bslib::accordion` panels at the bottom (simpler), then evaluate whether flip cards add enough UX value to be worth the complexity.

### Phase 4 — Design Polish *(not yet started)*
- [ ] Typography hierarchy — ensure headers, labels, and callout numbers are visually distinct
- [ ] Responsive layout check at 1280px
- [ ] Tighten chart whitespace and legend placement
- [ ] Add brief page-level explainer text about what TADs are (currently a stub in the header)

### Phase 5 — Social Media Takeaways *(not yet started)*
- [ ] Identify 4–6 headline findings (e.g., "Under the Mayor's NRI proposal, APS foregoes $X — enough to fund Y pre-K teachers for a year")
- [ ] Design slide template: large stat, minimal text, consistent brand
- [ ] Generate programmatically with `{ggplot2}` + `{camcorder}`
- [ ] Export at 1080×1080 (Instagram) and 1200×628 (Bluesky / Twitter card)

---

## Technical Stack

| Layer | Tool | Notes |
|---|---|---|
| App framework | R / Shiny | Single-file `app.R` |
| UI layout | `{bslib}` | Bootstrap 5; `page_fluid`, `layout_columns`, `card` |
| Charts | `{ggplot2}` + `{ggiraph}` | `ggiraph` adds click/hover via SVG-native interactive geoms |
| Map | `{ggplot2}` + `geom_sf` + `{ggiraph}` | Programmatic basemap — no tile dependencies |
| Spatial data | `{sf}` | All shapefiles read with `st_read`, reprojected to WGS84 |
| Data wrangling | `{tidyverse}` | |
| Custom theme | `theme_tad()` function | Defined once, applied to all charts |
| Static export / social slides | `{ggplot2}` + `{camcorder}` | Phase 5 |
| Hosting | Posit Connect | |

### Map Approach: Programmatic Basemap (no tiles)

**What was tried first:** `{ggspatial}` + `annotation_map_tile(type = "cartolight")` — this downloads CartoDB Light raster tiles and renders them behind the `geom_sf` layers. It required the `raster` and `prettymapr` packages, caused slowness on each render (tiles re-fetched), and created aspect ratio conflicts with `coord_sf`.

**What we use now:** A fully programmatic basemap using shapefiles only:
- Panel background set to `#e4e4e4` (light gray = "surrounding area")
- City of Atlanta boundary rendered as `geom_sf` with `fill = "#d0d0d0"` (medium gray)
- Road centerlines rendered as `geom_sf(color = "white")` from TIGER/Line shapefiles filtered to S1100 + S1200

Advantages: no package dependencies beyond `{sf}`, instant renders, full color/style control, no aspect ratio fights.

---

## Projection Methodology (as implemented)

**CAGR approach:** For each TAD, compute a Compound Annual Growth Rate from the first to last observed assessed value. Project forward by applying that rate from the last known year to 2055.

Three selectable methods (radio button in scenario controls):

| Method | How it works |
|---|---|
| TAD 2007–2024 CAGR | Each TAD uses its own CAGR from 2007 (first data year) to 2024 |
| TAD Baseline–2024 CAGR | Each TAD uses its own CAGR from its creation year / baseline value to 2024 — reaches further back for older TADs (e.g. Westside from 1998) | ⬜ Not yet built |
| Citywide average | All TADs use the CAGR of the **Atlanta column** (total City of Atlanta property value from Fulton County tax digest) | ⚠️ Currently uses TAD aggregate — needs fix |
| Optimistic — high-growth TADs | All TADs use the average CAGR of the three high-growth TADs (Atlantic Station, Beltline, Eastside) | ⚠️ Currently uses 75th pctile — needs fix |

**APS revenue formula:** `aps_annual_revenue = (projected_value - baseline) × APS_MILLAGE / 1000`
where `APS_MILLAGE = 20.74` mills (assumed constant; update if rate changes).

**Diversion formula (for the cumulative diversion chart):** Same as APS revenue formula, but applied only to years when the TAD is *still open* (diverting revenue away from APS rather than returning it).

---

## Data Sources

| Data | Source | Status |
|---|---|---|
| Historic TAD property values (2007–2024) | `TAD Basics.csv` — sourced from Invest Atlanta / Fulton County tax digest | ✅ In app |
| TAD baseline values | Same CSV | ✅ In app |
| TAD closure years — current plan | Same CSV | ✅ In app |
| TAD closure years — Mayor's Original NRI | Same CSV | ✅ In app |
| TAD closure years — Mayor's Updated NRI | Same CSV | ✅ In app |
| TAD boundary shapefiles | City of Atlanta Open Data | ✅ In app |
| City of Atlanta boundary | `Official_Atlanta_City_Limits_-_Open_Data.shp` | ✅ In app |
| Road centerlines (Fulton + DeKalb) | US Census TIGER/Line 2023 | ✅ In app |
| Road centerlines (Cobb, Clayton, Henry) | US Census TIGER/Line 2023 | ⬜ Not yet added |
| APS millage rate | Assumed 20.74 mills | ⚠️ Needs verification |
| Eastside TAD PILOT payments | Invest Atlanta / APS | ⬜ Not yet incorporated |
| Unit costs for "What Could This Fund?" | Various — see outstanding items | ⚠️ Needs update |

---

## Architecture Notes

**Cross-filtering:** A single `reactiveVal` called `selected_tad` stores the currently highlighted TAD (or `NULL` for all). Map clicks, chart line clicks, and a "× Show all" link all write to this value. All three charts read it and re-render automatically via Shiny's reactive graph. Blank-space clicks inside ggiraph containers are caught by a custom JavaScript listener that fires a `bg_click` Shiny input.

**Dynamic UI:** Per-TAD closure year sliders are built in `renderUI` (server-side) rather than declared in `ui` directly, because the number of active TADs could change. Preset buttons use `updateSliderInput` to batch-update all sliders.

**Diversion chart vs. projection chart:** The projection chart responds to the sliders (custom closure dates). The diversion chart always shows the three named scenarios with their fixed closure dates from `tad_meta` — this keeps it as a clean comparison tool independent of slider state.
