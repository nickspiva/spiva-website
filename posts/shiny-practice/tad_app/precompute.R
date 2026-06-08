# ============================================================
# precompute.R — run this once locally before deploying
# ============================================================
# Generates cached RDS files for the three most expensive
# startup operations in data.R:
#
#   roads_sf       — 7 county road shapefiles merged & transformed
#   proj_list      — property value projections (4 growth scenarios)
#   diversion_list — cumulative diversion data  (4 growth scenarios)
#
# HOW TO RUN:
#   source("posts/shiny-practice/tad_app/precompute.R")
#   from your project root in Positron/RStudio.
#
# Re-run whenever you update the source shapefiles or the
# projection / diversion model in data.R.
# ============================================================

message("── Precompute: starting ─────────────────────────────")

app_dir <- normalizePath(file.path(getwd(), "posts/shiny-practice/tad_app"))
out_dir  <- file.path(app_dir, "precomputed")
message("App dir: ", app_dir)

# withr::with_dir() sets the working directory to app_dir for the duration
# of the source() call, so all relative paths in data.R resolve correctly.
message("Sourcing data.R …")
withr::with_dir(app_dir, source("R/data.R"))

# Create output directory and save the three expensive objects
dir.create(out_dir, showWarnings = FALSE)

message("Saving roads_sf …")
saveRDS(roads_sf,       file.path(out_dir, "roads_sf.rds"))

message("Saving proj_list …")
saveRDS(proj_list,      file.path(out_dir, "proj_list.rds"))

message("Saving diversion_list …")
saveRDS(diversion_list, file.path(out_dir, "diversion_list.rds"))

message("── Precompute: done ─────────────────────────────────")
message("Files written to: ", out_dir)
message("Redeploy with: rsconnect::deployApp('posts/shiny-practice/tad_app')")
