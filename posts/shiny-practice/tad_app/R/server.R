# ── Server function ──────────────────────────────────────────────────────────
# Sourced by app.R; depends on globals defined in R/data.R
library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggiraph)
library(scales)
library(geomtextpath)

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

  # ── 7b-ii. Custom growth rate sliders ────────────────────
  # One slider per active TAD; defaults to each TAD's historical CAGR.
  # Only used when proj_method == "custom".
  output$growth_sliders <- renderUI({
    sliders <- map(active_tads$tad_id, \(tid) {
      default_rate <- round(
        growth_rates$cagr[growth_rates$tad_id == tid] * 100,
        1
      )
      div(
        tags$p(strong(tid), class = "mb-0 small"),
        sliderInput(
          inputId = paste0("gr_", make.names(tid)),
          label = NULL,
          min = 0,
          max = 15,
          value = default_rate,
          step = 0.1,
          post = "%",
          sep = "",
          ticks = FALSE
        )
      )
    })
    div(class = "px-1 pb-1", tagList(sliders))
  })

  # ── 7b-iii. PILOT participation sliders ─────────────────────
  # One slider per active TAD: 0–100% of the open-year increment returned to APS.
  # Defaults to 0% (no participation). Does NOT affect the diversion chart,
  # which uses hardcoded scenario-specific PILOT assumptions.
  output$pilot_sliders <- renderUI({
    # isolate() so re-clicking presets doesn't re-render and reset custom values;
    # the initial render picks up whichever preset is active at open time.
    preset <- isolate(active_preset()) %||% "current"
    sliders <- map(active_tads$tad_id, \(tid) {
      default_pct <- if (tid == "Eastside" && preset %in% c("current", "mayor1")) 100 else 0
      div(
        tags$p(strong(tid), class = "mb-0 small"),
        sliderInput(
          inputId = paste0("pilot_", make.names(tid)),
          label = NULL,
          min = 0,
          max = 100,
          value = default_pct,
          step = 5,
          post = "%",
          sep = "",
          ticks = FALSE
        )
      )
    })
    div(class = "px-1 pb-1", tagList(sliders))
  })

  # Reads pilot slider inputs. When a slider hasn't been rendered yet (panel
  # still collapsed), falls back to the active preset's assumed value rather
  # than 0 — so Eastside starts at 100% under Current Plan on first load.
  # Debounced so dragging doesn't trigger proj_chart re-renders on every pixel.
  pilot_rates_raw <- reactive({
    preset <- active_preset() %||% "current"
    map_dfr(active_tads$tad_id, \(tid) {
      val <- input[[paste0("pilot_", make.names(tid))]]
      pct <- if (!is.null(val)) {
        val / 100
      } else {
        # Pre-render fallback: match the hardcoded scenario PILOT assumptions
        if (tid == "Eastside" && preset %in% c("current", "mayor1")) 1.0 else 0
      }
      tibble(tad_id = tid, pilot_pct = pct)
    })
  })
  pilot_rates <- pilot_rates_raw |> debounce(350)

  # When the custom growth panel opens, seed sliders with rates from the
  # previously selected method — mirrors how closure sliders inherit preset dates.
  observeEvent(input$custom_growth_opened, {
    prev <- input$custom_growth_opened

    rates <- switch(
      prev,
      "tad" = setNames(round(growth_rates$cagr * 100, 1), growth_rates$tad_id),
      "city" = setNames(
        rep(round(citywide_cagr * 100, 1), nrow(growth_rates)),
        growth_rates$tad_id
      ),
      "optimistic" = setNames(
        rep(round(optimistic_cagr * 100, 1), nrow(growth_rates)),
        growth_rates$tad_id
      ),
      setNames(round(growth_rates$cagr * 100, 1), growth_rates$tad_id) # fallback to tad
    )

    walk(active_tads$tad_id, \(tid) {
      val <- rates[[tid]]
      if (!is.null(val) && !is.na(val)) {
        updateSliderInput(session, paste0("gr_", make.names(tid)), value = val)
      }
    })
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

  # Sets PILOT sliders to match a named scenario. pilot_overrides is a named
  # list of tad_id → pct (0–100); all other active TADs are reset to 0.
  apply_pilot_preset <- function(pilot_overrides = list()) {
    walk(active_tads$tad_id, \(tid) {
      val <- if (!is.null(pilot_overrides[[tid]])) pilot_overrides[[tid]] else 0
      updateSliderInput(session, paste0("pilot_", make.names(tid)), value = val)
    })
  }

  observeEvent(input$btn_current, {
    apply_preset("year_end_current")
    apply_pilot_preset(list("Eastside" = 100)) # Eastside → 100%, rest → 0%
    active_preset("current")
  })
  observeEvent(input$btn_mayor1, {
    apply_preset("year_end_mayor1")
    apply_pilot_preset(list("Eastside" = 100)) # Eastside → 100%, rest → 0%
    active_preset("mayor1")
  })
  observeEvent(input$btn_mayor2, {
    apply_preset("year_end_mayor2")
    apply_pilot_preset() # all PILOTs → 0%
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

  # Which projection dataset to use.
  # Growth-rate-only projections — no PILOT. Used by diversion_data so that
  # moving PILOT sliders doesn't trigger the diversion chart to recompute.
  proj_data_base <- reactive({
    method <- input$proj_method %||% "tad"
    if (method == "custom") {
      map_dfr(seq_len(nrow(growth_rates)), \(i) {
        g <- growth_rates[i, ]
        rate_pct <- input[[paste0("gr_", make.names(g$tad_id))]]
        r <- if (!is.null(rate_pct)) rate_pct / 100 else g$cagr
        tibble(
          year = (g$last_year + 1):PROJ_END,
          tad_id = g$tad_id,
          value = g$last_val * (1 + r)^seq_len(PROJ_END - g$last_year)
        )
      }) |>
        left_join(tad_meta |> select(tad_id, baseline), by = "tad_id") |>
        mutate(
          increment = pmax(value - baseline, 0),
          aps_annual_revenue = increment * APS_MILLAGE / 1000
        )
    } else {
      proj_list[[method]]
    }
  })

  # Full projections: base + PILOT rates. Used by proj_chart and proj_subheader.
  # PILOT slider changes only invalidate this, not proj_data_base or diversion_data.
  proj_data <- reactive({
    proj_data_base() |>
      left_join(pilot_rates(), by = "tad_id") |>
      mutate(
        pilot_pct = replace_na(pilot_pct, 0),
        aps_revenue_open = increment * APS_MILLAGE / 1000 * pilot_pct
      )
  })

  # Closure year for each TAD under the current slider/preset state.
  # _raw fires immediately on every slider tick; the debounced version waits
  # until the user stops dragging so the chart only re-renders once per gesture.
  closure_years_raw <- reactive({
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
  closure_years <- closure_years_raw |> debounce(400)

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

  # Cumulative diversion data. For the four static growth methods the result is
  # pre-computed in diversion_list (data.R) and looked up instantly.
  # Only custom growth rates require on-the-fly computation — and even then,
  # diversion_data depends on proj_data_base() not proj_data(), so PILOT slider
  # changes never trigger a diversion recompute.
  diversion_data <- reactive({
    method <- input$proj_method %||% "tad"
    if (method == "custom") {
      make_diversion_data(proj_data_base())
    } else {
      diversion_list[[method]]
    }
  })

  # ── 7e-b. Diversion chart subheader (reactive) ───────────────────────────
  # Rebuilds when the growth-rate assumption changes so the gap dollar amount
  # and assumption name always match what's shown in the chart.
  output$diversion_subheader <- renderUI({
    proj_labels <- c(
      "tad"       = "Historic TAD Growth",
      "city"      = "Citywide Average Growth",
      "optimistic" = "Optimistic Growth",
      "custom"    = "Custom Growth Rate"
    )
    growth_name <- proj_labels[[input$proj_method %||% "tad"]]
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
        2030:(PROJ_END - 1),
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
      '<p class="text-muted small px-3 pt-1 mt-1 subheader-text">%s %s</p>',
      sprintf(
        "This chart projects the <strong>cumulative APS property tax revenue redirected to Invest Atlanta</strong> from 2025 onward. Under the current growth assumption, based on <span class='dyn-val'>%s</span>, the Mayor's Updated NRI proposal would divert an additional <span class='dyn-val'>%s</span> more than the current plan over the next 30 years.",
        growth_name,
        gap_fmt
      ),
      sprintf(
        "On just an annual basis, the Mayor's Updated NRI proposal would divert approximately <span class='dyn-val'>%s</span> from APS in %s, ballooning to <span class='dyn-val'>%s</span> per year by 2055.",
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
      geom_vline(
        xintercept = 2055,
        color = "#da9124",
        linetype = "dotted",
        linewidth = 0.6,
        alpha = 0.8
      ) +
      annotate(
        "text",
        x = 2055 - 0.4,
        y = Inf,
        label = paste0(
          "All TADs closed\n(NRI Plans, ",
          2055,
          ")"
        ),
        hjust = 1,
        vjust = 1.3,
        size = 2.6,
        color = "#da9124"
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
        size = 3.5,
        fontface = "bold",
        hjust = 0.72,
        gap = FALSE,
        text_smoothing = 20,
        offset = unit(-14, "pt")
      ) +
      # NRI scenario labels sit above their lines
      geom_textline(
        data = ~ filter(.x, !grepl("Current Plan", scenario)),
        aes(label = scenario),
        linewidth = 1.3,
        size = 3.5,
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
        size = 3.2,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_size_identity() +
      scale_color_manual(values = SCENARIO_COLORS, name = NULL) +
      scale_y_continuous(
        labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 1),
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
        opts_sizing(rescale = TRUE, width = 1),
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
      labs(y = "") +
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
      "tad"        = "Historic TAD Growth",
      "city"       = "Citywide Average Growth",
      "optimistic" = "Optimistic Growth",
      "custom"     = "Custom Growth Rate"
    )[[input$proj_method %||% "tad"]]

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
        " For example, in %s, tax on property in the former Beltline TAD area will generate <span class='dyn-val'>%s</span> of annual revenue for schools, using the <span class='dyn-val'>%s</span> growth assumption.",
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
      '<p class="text-muted small px-3 pt-1 mt-1 subheader-text">%s%s</p>',
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

    # Pre-closure PILOT segments: shown only for TADs where pilot_pct > 0.
    # Uses aps_revenue_open (increment × millage × pilot_pct) as the y value.
    # Rendered as lighter dashed lines to distinguish from post-closure revenue.
    pilot_tad <- pd |>
      left_join(cy, by = "tad_id") |>
      filter(tad_id %in% active_tad_ids, year < closure_year, pilot_pct > 0) |>
      mutate(
        aps_annual_revenue = aps_revenue_open,
        line_a = if (is.null(sel)) 0.4 else if_else(tad_id == sel, 0.55, 0.06),
        line_w = if (is.null(sel)) 0.55 else if_else(tad_id == sel, 1.0, 0.3)
      )

    # ── Empty-state guard ─────────────────────────────────────────────────
    # Under scenarios where all TADs close after 2055, per_tad has no rows.
    # Return a blank chart with proper axes rather than letting ggplot error.
    if (nrow(per_tad) == 0 && nrow(pilot_tad) == 0) {
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
        lbl_vjust = 1.6,
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
      # Pre-closure PILOT lines & points — lighter, dashed; only drawn when pilot_pct > 0
      {
        if (nrow(pilot_tad) > 0) {
          list(
            geom_line_interactive(
              data = pilot_tad,
              aes(
                alpha = line_a,
                linewidth = line_w,
                data_id = tad_id,
                tooltip = tad_id
              ),
              linetype = "dashed"
            ),
            geom_point_interactive(
              data = pilot_tad,
              aes(
                alpha = line_a,
                size = line_w,
                data_id = tad_id,
                tooltip = paste0(
                  "<b>",
                  tad_id,
                  "</b> (PILOT, TAD still open)<br>",
                  year,
                  "<br>",
                  "PILOT revenue to APS: ",
                  dollar(
                    aps_annual_revenue,
                    scale = 1e-6,
                    suffix = "M",
                    accuracy = 0.1
                  )
                )
              )
            )
          )
        }
      } +
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
        size = 3.2,
        fontface = "bold",
        show.legend = FALSE
      ) +
      scale_color_manual(values = TAD_PALETTE, na.value = "grey70") +
      scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
      scale_size_identity() +
      scale_alpha_identity() +
      scale_linewidth_identity() +
      labs(y = "") +
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
            "APS has committed to several goals by 2030 as part of its Back to Basics strategic plan. TADs closing on schedule will generate an estimated <strong>%s</strong> in annual revenue for schools by %d, putting these aspirations within reach.",
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
        short_desc = "Healthcare and pension costs have grown dramatically and are largely outside APS control. The State Health Benefit Plan employer rate doubled between FY2021 and FY2026, and teacher pension contributions have risen from 14% to 22% over the past decade.",
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
        short_desc = "Georgia's legislature capped annual property assessment increases at ~3% this spring, constraining APS's primary revenue source to grow near inflation while underlying costs consistently outpace it.",
        desc = paste0(
          "Georgia's legislature passed Senate Bill 33 this spring capping annual property assessment increases ",
          "at the rate of inflation (currently around 3%). ",
          "Property taxes are APS's primary revenue source, and legally capping revenue growth near inflation ",
          "while underlying costs consistently grow faster creates a structural deficit that compounds over time. ",
          "APS may have limited flexibility through millage rate increases, given that its rate has historically ",
          "exceeded the state's 20-mill cap, but this legislation is part of a broader pattern of constraints ",
          "on local districts' ability to fund public education."
        )
      ),
      list(
        title = "Declining Enrollment",
        stat = "-2,398",
        stat_lbl = "projected student decline by 2030",
        title = "Declining Enrollment",
        stat = "-2,398",
        stat_lbl = "projected student decline by 2030",
        short_desc = "APS enrollment is projected to fall by 2,398 students by 2030. Last December, the Board cited declining enrollment in its vote to close 16 schools. Fewer students mean less revenue, and potentially more closures.",
        desc = paste0(
          "APS enrollment is projected to fall from 49,944 students in 2024–25 to 47,546 in 2029–30. ",
          "The district cited this trend in its decision last fall to close 16 neighborhood schools,",
          " including high-academic-growth schools like Dunbar. ",
          "TAD revenue could change the calculus on which schools can stay open and fully resourced moving forward."
        )
      )
    )

    challenge_cards <- map(challenge_items, \(item) {
      div(
        class = "risk-flip-card",
        div(
          class = "risk-flip-inner",
          # Front face — stat + title + short summary
          div(
            class = "risk-flip-front text-center",
            style = "background-color:#fff8e1;",
            div(
              style = "font-size:2rem; font-weight:700; color:#e8a020; line-height:1;",
              item$stat
            ),
            tags$p(item$stat_lbl, class = "small text-muted mb-1"),
            tags$hr(class = "mx-4 my-1 w-100"),
            tags$p(item$title, class = "small fw-semibold mb-1"),
            tags$p(
              item$short_desc,
              style = "font-size:0.75rem; color:#555; line-height:1.4; margin-bottom:0;"
            ),
            div(class = "risk-flip-hint", "learn more ↻")
          ),
          # Back face — full description
          div(
            class = "risk-flip-back",
            style = "background-color:#fff8e1;",
            tags$p(
              item$title,
              class = "small fw-semibold mb-2 text-center w-100"
            ),
            tags$p(
              item$desc,
              style = "font-size:0.72rem; color:#555; line-height:1.45; margin-bottom:0;"
            ),
            div(class = "risk-flip-hint", "↺ flip back")
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
            "APS faces structural financial pressures threatening its system in the years to come. TAD revenue is crucial to navigating these challenges."
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

  # ── 7j. Methodology reference tables ─────────────────────────────────────
  output$hist_wide_table <- renderTable(
    {
      hist_data |>
        select(year, tad_id, value) |>
        bind_rows(atl_series |> mutate(tad_id = "Atlanta")) |>
        mutate(
          value_fmt = if_else(
            value >= 1e9,
            dollar(value, scale = 1e-9, suffix = "B", accuracy = 0.01),
            dollar(value, scale = 1e-6, suffix = "M", accuracy = 1)
          )
        ) |>
        select(-value) |>
        pivot_wider(names_from = tad_id, values_from = value_fmt) |>
        arrange(year) |>
        rename(Year = year)
    },
    striped = TRUE,
    width = "100%",
    align = "r",
    na = "—"
  )

  output$growth_rate_table <- renderTable(
    {
      growth_rates |>
        mutate(
          period = paste0(first_year, "–", last_year),
          cagr_pct = paste0(round(cagr * 100, 1), "%")
        ) |>
        select(tad_id, period, cagr_pct) |>
        bind_rows(tibble(
          tad_id = "Atlanta (citywide)",
          period = paste0(min(atl_series$year), "–", max(atl_series$year)),
          cagr_pct = paste0(round(citywide_cagr * 100, 1), "%")
        )) |>
        rename(
          "TAD" = tad_id,
          "Period" = period,
          "CAGR" = cagr_pct
        )
    },
    striped = TRUE,
    width = "100%"
  )
}
