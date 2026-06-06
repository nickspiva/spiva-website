# ── UI definition ────────────────────────────────────────────────────────────
# Sourced by app.R; depends on globals defined in R/data.R
library(shiny)
library(bslib)
library(ggiraph)

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

  # ── Google Fonts ─────────────────────────────────────────
  tags$head(tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Barlow:ital,wght@0,400;0,500;0,600;0,700;1,400&family=Fira+Sans+Condensed:ital,wght@0,400;0,600;0,700;1,400&display=swap"
  )),

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

  # ── JS: "What's at Risk" card flip ───────────────────────────────────────
  tags$script(HTML(
    "
    $(document).on('click', '.risk-flip-card', function() {
      $(this).toggleClass('flipped');
    });
  "
  )),

  # ── CSS: inline year select ───────────────────────────────────────────────
  tags$style(HTML(
    "
    body, p, li, td, th, label, .btn, .small, .text-muted,
    .shiny-input-container, input, select, textarea {
      font-family: 'Barlow', sans-serif !important;
      font-size: 15.5px;
    }
    h1, h2, h3, h4, h5, h6,
    .h1, .h2, .h3, .h4, .h5, .h6,
    .card-header, .navbar-brand, .sidebar-title,
    .nav-link, .accordion-button,
    [style*='font-weight:700'], [style*='font-weight: 700'] {
      font-family: 'Fira Sans Condensed', sans-serif !important;
    }
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
    .nav-tabs .nav-link { font-weight: 600; font-size: 0.95rem; }
    .nav-tabs .nav-link.active { font-weight: 700; }
    .collapse-caret { display: inline-block; transition: transform 0.2s ease; font-size: 0.65rem; }
    [aria-expanded='true'] .collapse-caret { transform: rotate(180deg); }
    .sidebar .btn-outline-secondary { color: #495057; }
    .sidebar .btn-outline-secondary:hover { color: #fff; background-color: #6c757d; border-color: #6c757d; }
    #tad_map, #tad_map .girafe_container_std, #tad_map svg { width: 100% !important; display: block; }
    .risk-flip-card { perspective: 1200px; cursor: pointer; height: 250px; }
    .risk-flip-inner { position: relative; width: 100%; height: 100%; transform-style: preserve-3d; transition: transform 0.5s ease; }
    .risk-flip-card.flipped .risk-flip-inner { transform: rotateY(180deg); }
    .risk-flip-front, .risk-flip-back { position: absolute; inset: 0; backface-visibility: hidden; -webkit-backface-visibility: hidden; border-radius: 0.375rem; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 1rem; overflow: hidden; }
    .risk-flip-back { transform: rotateY(180deg); overflow-y: auto; justify-content: flex-start; align-items: flex-start; }
    .risk-flip-hint { font-size: 0.65rem; color: #adb5bd; margin-top: auto; padding-top: 6px; user-select: none; width: 100%; text-align: center; flex-shrink: 0; }
    @keyframes subheader-fade { from { opacity: 0.45; } to { opacity: 1; } }
    .subheader-text { animation: subheader-fade 0.5s ease-out; }
    .dyn-val { background: rgba(74,144,217,0.12); border-radius: 3px; padding: 0 3px; font-weight: 600; }
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

    div(
      class = "btn-group-vertical w-100",
      actionButton(
        "btn_current",
        "Current Plan in Place",
        class = "btn btn-outline-primary btn-sm text-start"
      ),
      actionButton(
        "btn_mayor1",
        "Mayor's Original NRI",
        class = "btn btn-outline-warning btn-sm text-start"
      ),
      actionButton(
        "btn_mayor2",
        "Mayor's Updated NRI",
        class = "btn btn-outline-danger btn-sm text-start"
      ),
      tags$button(
        type = "button",
        class = "btn btn-outline-secondary btn-sm text-start d-flex justify-content-between align-items-center",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#tad-sliders-collapse",
        `aria-expanded` = "false",
        `aria-controls` = "tad-sliders-collapse",
        span("Custom Selection"),
        tags$span("▼", class = "collapse-caret")
      )
    ),

    div(
      id = "tad-sliders-collapse",
      class = "collapse mt-2",
      uiOutput("tad_sliders")
    ),

    hr(class = "my-2"),

    h6("How fast will property values grow?", class = "fw-bold mb-1"),
    # Growth rate toggle buttons — Bootstrap btn-check pattern.
    # "TAD growth since inception" (tad_baseline) is intentionally omitted
    # from the UI (projections look unrealistic) but the data remains in
    # proj_list for future use.
    tags$div(
      class = "btn-group-vertical w-100 mb-1",
      role = "group",
      tags$input(
        type = "radio",
        class = "btn-check",
        name = "proj_method_grp",
        id = "proj_tad",
        value = "tad",
        autocomplete = "off",
        checked = "checked"
      ),
      tags$label(
        class = "btn btn-outline-secondary btn-sm text-start",
        `for` = "proj_tad",
        "Historic TAD Growth"
      ),
      tags$input(
        type = "radio",
        class = "btn-check",
        name = "proj_method_grp",
        id = "proj_city",
        value = "city",
        autocomplete = "off"
      ),
      tags$label(
        class = "btn btn-outline-secondary btn-sm text-start",
        `for` = "proj_city",
        "Citywide Average Growth"
      ),
      tags$input(
        type = "radio",
        class = "btn-check",
        name = "proj_method_grp",
        id = "proj_optimistic",
        value = "optimistic",
        autocomplete = "off"
      ),
      tags$label(
        class = "btn btn-outline-secondary btn-sm text-start",
        `for` = "proj_optimistic",
        "Optimistic Growth"
      ),
      tags$button(
        type = "button",
        id = "btn_custom_growth",
        class = "btn btn-outline-secondary btn-sm text-start d-flex justify-content-between align-items-center",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#growth-sliders-collapse",
        `aria-expanded` = "false",
        span("Custom Growth Rate"),
        tags$span("▼", class = "collapse-caret")
      )
    ),
    div(
      id = "growth-sliders-collapse",
      class = "collapse mt-1",
      uiOutput("growth_sliders")
    ),
    tags$script(HTML(
      "
      // Preset radios: update Shiny + collapse custom panel if open
      $(document).on('change', 'input[name=\"proj_method_grp\"]', function() {
        Shiny.setInputValue('proj_method', this.value, {priority: 'event'});
        var el = document.getElementById('growth-sliders-collapse');
        if (el && el.classList.contains('show')) {
          bootstrap.Collapse.getInstance(el).hide();
        }
      });
      // Custom panel opens: capture prev method, activate custom, deselect radios
      $(document).on('show.bs.collapse', '#growth-sliders-collapse', function() {
        var prevMethod = $('input[name=\"proj_method_grp\"]:checked').val() || 'tad';
        Shiny.setInputValue('proj_method', 'custom', {priority: 'event'});
        Shiny.setInputValue('custom_growth_opened', prevMethod, {priority: 'event'});
        $('input[name=\"proj_method_grp\"]').prop('checked', false);
      });
      // Custom panel closes: restore whichever radio is checked (or default tad)
      $(document).on('hide.bs.collapse', '#growth-sliders-collapse', function() {
        var checked = $('input[name=\"proj_method_grp\"]:checked').val() || 'tad';
        Shiny.setInputValue('proj_method', checked, {priority: 'event'});
      });
    "
    )),

    hr(class = "my-2"),

    h6(
      "Will APS partially participate in any TADs?",
      class = "fw-bold mb-1 mt-1"
    ),
    p(
      "By default, APS receives no increment while TADs are open. ",
      "Use these sliders to model what-if PILOT scenarios.",
      class = "small text-muted mb-2"
    ),
    tags$button(
      type = "button",
      id = "btn_custom_pilot",
      class = "btn btn-outline-secondary btn-sm text-start w-100 d-flex justify-content-between align-items-center",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = "#pilot-sliders-collapse",
      `aria-expanded` = "false",
      span("Custom PILOT Rates"),
      tags$span("▼", class = "collapse-caret")
    ),
    div(
      id = "pilot-sliders-collapse",
      class = "collapse mt-1",
      uiOutput("pilot_sliders")
    ),

    hr(class = "my-2"),

    tags$button(
      type = "button",
      class = "btn btn-link btn-sm text-start w-100 d-flex justify-content-between align-items-center px-0 fw-bold",
      style = "color: inherit; text-decoration: none;",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = "#iga-collapse",
      `aria-expanded` = "false",
      span("Intergovernmental Agreements", class = "h6 mb-0 fw-bold"),
      tags$span("▼", class = "collapse-caret")
    ),
    div(
      id = "iga-collapse",
      class = "collapse mt-1",
      p(
        "While this app gives a picture of the revenue implications of extended TADs for APS, ",
        "the reality of what is being considered often gets even wonkier! ",
        "APS has previously, and may again, negotiate with the City and ",
        "Invest Atlanta to partially participate in individual TADs.",
        class = "small text-muted mb-2"
      ),
      p(
        HTML(
          "Currently, APS receives the full increment for the
          Eastside TAD via PILOT payments, even though it remains open.
          APS contributions for the Corridor TADs (Campbellton, Hollowell,
          Metropolitan Pkwy, &amp; Stadium) are capped at $6.5M from 2029&ndash;2050. Please note these caps are not reflected in the charts currently."
        ),
        class = "small text-muted mb-2"
      ),
      p(
        HTML(
          "Under the Mayor's updated NRI, the Eastside PILOT would end and
          the full increment would flow to Invest Atlanta through 2055. Under the
          Mayor's original NRI, existing agreements (PILOTs &amp; caps)
          would have continued."
        ),
        class = "small text-muted mb-0"
      )
    ),
  ),

  # ════════════════════════════════════════════════════════
  # MAIN CONTENT — charts scroll past the sticky sidebar
  # ════════════════════════════════════════════════════════

  p(
    "Atlanta's Tax Allocation Districts (TADs) redirect property tax growth from ",
    "schools to fund development. While a TAD is open, all tax revenue on property value ",
    "above the original baseline goes to ",
    strong("Invest Atlanta"),
    " - not Atlanta Public Schools, the City, or County. ",
    "Use the controls on the left to explore different closure scenarios.",
    class = "text-muted mb-3"
  ),

  # ── Revenue impact — stacked cards ───────────────────────
  card(
    card_header("Total Revenue Diverted from Schools"),
    uiOutput("diversion_subheader"),
    girafeOutput("diversion_chart", height = "380px")
  ),

  br(),

  card(
    card_header("Projected APS Revenue from Closed TADs"),
    uiOutput("proj_subheader"),
    girafeOutput("proj_chart", height = "380px")
  ),

  br(),

  # ── What could this fund? ────────────────────────────────
  card(
    card_header("Why TAD Closures Matter for APS Schools"),
    uiOutput("buy_panel"),
    uiOutput("challenge_panel")
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
          span("TAD Boundaries"),
          actionLink(
            "clear_sel",
            "× Show all",
            class = "small text-muted text-decoration-none"
          )
        )
      ),
      card_body(
        class = "p-0",
        girafeOutput("tad_map", height = "460px")
      )
    ),
    card(
      card_header("Historic Property Values  ·  2007–2024"),
      girafeOutput("historic_chart", height = "380px")
    )
  ),

  br(),

  # ── How is all this being calculated? ─────────────────────────
  navset_card_tab(
    title = "How is all this being calculated?",

    nav_panel(
      "Estimates",
      div(
        class = "p-3",
        p(
          HTML(
            "These cost figures are approximations. Napkin math grounded in publicly available data, but <strong>not official APS projections</strong>. Each gives a rough sense of what TAD revenue could support."
          ),
          class = "text-muted small mb-3"
        ),
        accordion(
          open = FALSE,
          accordion_panel(
            "TAD Increment Projections",
            p(
              "Each TAD's projected APS revenue is calculated by compounding property values
              forward from a 2024 baseline, then applying the APS millage rate to the
              increment above the original baseline.",
              class = "small text-muted mb-3"
            ),
            tags$ol(
              class = "small text-muted ps-3",
              tags$li(
                class = "mb-2",
                strong("Baseline increment:"),
                " The 2024 assessed value minus each TAD's original baseline value gives
                the current taxable increment — the portion of property value that would
                return to APS if the TAD closed today."
              ),
              tags$li(
                class = "mb-2",
                strong("Annual projection:"),
                " Each subsequent year's assessed value is estimated by multiplying the
                prior year's value by (1 + growth rate). The growth rate depends on the
                scenario selected in the sidebar: Historic TAD Growth, Citywide Average,
                Optimistic, or a custom rate."
              ),
              tags$li(
                class = "mb-2",
                strong("APS revenue:"),
                HTML(
                  " The projected increment is multiplied by APS's current millage rate
                (<strong>20.5 mills</strong>) to estimate what APS would collect if the
                TAD were closed and the increment returned to the tax digest."
                )
              ),
              tags$li(
                class = "mb-2",
                strong("Repeat through closure year:"),
                " Steps 2 and 3 repeat annually from 2025 through the year each TAD closes
                under the selected scenario. Revenue in years before closure is $0; that
                increment still flows to Invest Atlanta."
              )
            ),
            p(
              "These are projections, not forecasts. They assume constant growth and
              millage rates over the projection period.",
              class = "small text-muted fst-italic mb-0"
            )
          ),
          accordion_panel(
            "Universal Pre-K Staffing  ·  $78.2M / year",
            p(
              strong("Scope:"),
              " Annual staffing costs only: salaries plus employer benefits. Excludes capital costs, curriculum, and materials."
            ),
            p(strong("Seat gap:")),
            tags$ul(
              tags$li(
                "APS kindergarten enrollment (2025–26): 3,620. This is the proxy for each age cohort"
              ),
              tags$li(
                "Total seats needed for universal 3K + 4K: 7,240 (3,620 × 2 cohorts)"
              ),
              tags$li(
                "Existing APS pre-K seats: 1,234 (GADOE via APS Insights; may not fully reflect Head Start seats for 3-year-olds)"
              ),
              tags$li(strong("Gap: ~6,006 additional seats"))
            ),
            p(strong("Staffing:")),
            tags$ul(
              tags$li(
                "Class size: 18 (state cap is 20; 18 for inclusion classrooms)"
              ),
              tags$li("Classrooms needed: ceiling(6,006 ÷ 18) = 334"),
              tags$li(
                "334 lead teachers + 334 assistant teachers = 668 total new staff"
              )
            ),
            p(strong("Annual employer cost per employee:")),
            tags$ul(
              tags$li(
                "Lead teacher: $100k salary + $22,620 health + $21,910 pension (21.91% TRS) = $144,530"
              ),
              tags$li(
                "Assistant: $55k salary + $22,620 health + $12,050 pension = $89,670"
              ),
              tags$li(
                "Health insurance: $1,885/month × 12 (employer share, individual plan)"
              )
            ),
            p(strong("Total: 334 × $144,530 + 334 × $89,670 ≈ $78.2M / year")),
            p(
              "Pension rises to 22.32% in 2028 (TRS of Georgia). Capital costs for 334 new classrooms not included.",
              class = "text-muted small mt-2 mb-0"
            )
          ),
          accordion_panel(
            "Free MARTA for K–12 Students  ·  $27.7M / year",
            p(
              strong("Scope:"),
              " Year-round unlimited MARTA access for all APS K–12 students (younger kids already ride free). Modeled on DC's Kids Ride Free program (ages 5–21, all Metro/bus service)."
            ),
            p(strong("Calculation:")),
            tags$ul(
              tags$li("44,876 APS K–12 students (October 2024 enrollment)"),
              tags$li("× $68.50/month (MARTA UPass unlimited rate)"),
              tags$li("× 12 months"),
              tags$li(
                "× 0.75 (25% bulk discount assumed for a district-wide contract)"
              ),
              tags$li(strong("= 44,876 × $68.50 × 12 × 0.75 ≈ $27.7M / year"))
            ),
            p(strong("Limitations:")),
            tags$ul(
              tags$li(
                "The 25% bulk discount is an assumption, the actual negotiated rate may differ"
              ),
              tags$li(
                "UPass is a university product; APS would need a comparable institutional agreement"
              ),
              tags$li(
                "Not all APS students live near high-frequency MARTA service"
              )
            )
          ),
          accordion_panel(
            "$100K Average Teacher Salary  ·  $34.6M / year",
            p(
              strong("Scope:"),
              " Additional annual employer cost of raising the average APS teacher salary from $90,470 to $100,000, including the corresponding pension increase."
            ),
            p(strong("Inputs:")),
            tags$ul(
              tags$li(
                "Current average APS teacher salary: $90,470 (APS Back to Basics 2030 KPI Dashboard)"
              ),
              tags$li("Total APS classroom teachers: 2,976"),
              tags$li("Salary gap: $100,000 − $90,470 = $9,530 per teacher"),
              tags$li("TRS of Georgia employer pension rate: 21.91%")
            ),
            p(strong("Calculation:")),
            tags$ul(
              tags$li("Additional salary: 2,976 × $9,530 = $28,361,280"),
              tags$li("Additional pension: $28,361,280 × 21.91% = $6,213,896"),
              tags$li(strong("Total: ~$34.6M / year"))
            ),
            p(
              "Pension rises to 22.32% in 2028.",
              class = "text-muted small mt-2 mb-0"
            )
          )
        )
      )
    ),

    nav_panel(
      "TAD Property Values & Growth Rates",
      div(
        class = "p-3",
        h6(
          "Assessed Property Values by TAD, 2007–2024",
          class = "fw-bold mb-1"
        ),
        p(
          "Total assessed value within each TAD boundary, in millions of dollars. Only the increment above each TAD's original baseline flows to Invest Atlanta.",
          class = "text-muted small mb-2"
        ),
        div(style = "overflow-x: auto;", tableOutput("hist_wide_table")),
        p(
          HTML(
            "<em>Note: Atlanta property values reflect the Fulton County portion of the City of Atlanta.</em>"
          ),
          class = "small text-muted mt-1 mb-0"
        ),
        tags$hr(class = "my-4"),
        h6("Compound Annual Growth Rates (CAGR)", class = "fw-bold mb-1"),
        p(
          HTML(
            "CAGR = (ending value ÷ starting value)<sup>1 ÷ years</sup> − 1, measuring average annual growth from 2007 to 2024."
          ),
          class = "text-muted small mb-2"
        ),
        div(style = "overflow-x: auto;", tableOutput("growth_rate_table"))
      )
    ),

    nav_panel(
      "Relevant Links",
      div(
        class = "p-3",
        layout_columns(
          col_widths = c(6, 6),
          div(
            p(strong("APS data"), class = "mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "APS Insights — Enrollment 1994–2024",
                href = "https://apsinsights.org/2025/03/13/aps-enrollment-data-1994-2024/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "APS Insights — Pre-K enrollment",
                href = "https://apsinsights.org/2026/02/23/aps-enrollment-1994-2026/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "APS Back to Basics 2030 — KPI Dashboard",
                href = "https://www.atlantapublicschools.us/about/strategic-plan/key-performance-indicators",
                target = "_blank"
              )),
              tags$li(tags$a(
                "ABOE TV — Budget Commission Meeting, Sep 30 2025",
                href = "https://www.youtube.com/watch?v=LvB1r0vA_qU",
                target = "_blank"
              )),
              tags$li(tags$a(
                "APS Board Meeting Attachment (simbli)",
                href = "https://simbli.eboardsolutions.com/Meetings/Attachment.aspx?S=36031014&AID=1824821&MID=123422",
                target = "_blank"
              ))
            ),
            p(strong("Transit comparisons"), class = "mt-3 mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "MARTA University Pass Program",
                href = "https://itsmarta.com/university-program.aspx",
                target = "_blank"
              )),
              tags$li(tags$a(
                "DC Kids Ride Free Program (DDOT)",
                href = "https://ddot.dc.gov/page/kids-ride-free-program",
                target = "_blank"
              ))
            ),
            p(strong("APS budget & policy"), class = "mt-3 mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "Hatcher — Restoring State Funding for State-Mandated Costs",
                href = "https://www.kerryhatcher.com/restoring-state-funding-for-state-mandated-costs/",
                target = "_blank"
              ))
            )
          ),
          div(
            p(strong("Georgia education finance"), class = "mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "TRS of Georgia — Employer Contribution Rates",
                href = "https://www.trsga.com/employer/contribution-rates/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — FY2027 K-12 Budget Overview",
                href = "https://gbpi.org/overview-2027-fiscal-year-budget-for-k-12-education/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — Retirement in Georgia's Public Schools",
                href = "https://gbpi.org/retirement-in-georgias-public-schools/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia SB 33 — annual property assessment cap",
                href = "https://legiscan.com/GA/bill/SB33/2025",
                target = "_blank"
              )),
              tags$li(tags$a(
                "WABE — Schools Could Lose Most If Property Tax Legislation Becomes Law",
                href = "https://www.wabe.org/schools-could-lose-most-if-property-tax-legislation-becomes-law/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia Recorder — Kemp Approves Property Tax Relief",
                href = "https://georgiarecorder.com/2026/05/11/kemp-approves-property-tax-relief-for-georgia-homeowners-amid-concerns-over-local-revenues-process/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "SaportaReport — Setting Teachers up for Success",
                href = "https://saportareport.com/setting-teachers-up-for-success/columnists/guestcolumn/derek/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Hatcher — Restoring State Funding for State-Mandated Costs",
                href = "https://www.kerryhatcher.com/restoring-state-funding-for-state-mandated-costs/",
                target = "_blank"
              ))
            )
          ),
          div(
            p(strong("Georgia education finance"), class = "mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "TRS of Georgia — Employer Contribution Rates",
                href = "https://www.trsga.com/employer/contribution-rates/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — FY2027 K-12 Budget Overview",
                href = "https://gbpi.org/overview-2027-fiscal-year-budget-for-k-12-education/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — Retirement in Georgia's Public Schools",
                href = "https://gbpi.org/retirement-in-georgias-public-schools/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia SB 33 — annual property assessment cap",
                href = "https://legiscan.com/GA/bill/SB33/2025",
                target = "_blank"
              )),
              tags$li(tags$a(
                "WABE — Schools Could Lose Most If Property Tax Legislation Becomes Law",
                href = "https://www.wabe.org/schools-could-lose-most-if-property-tax-legislation-becomes-law/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia Recorder — Kemp Approves Property Tax Relief",
                href = "https://georgiarecorder.com/2026/05/11/kemp-approves-property-tax-relief-for-georgia-homeowners-amid-concerns-over-local-revenues-process/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "SaportaReport — Setting Teachers up for Success",
                href = "https://saportareport.com/setting-teachers-up-for-success/columnists/guestcolumn/derek/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Hatcher — Restoring State Funding for State-Mandated Costs",
                href = "https://www.kerryhatcher.com/restoring-state-funding-for-state-mandated-costs/",
                target = "_blank"
              ))
            )
          ),
          div(
            p(strong("Georgia education finance"), class = "mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "TRS of Georgia — Employer Contribution Rates",
                href = "https://www.trsga.com/employer/contribution-rates/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — FY2027 K-12 Budget Overview",
                href = "https://gbpi.org/overview-2027-fiscal-year-budget-for-k-12-education/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — Retirement in Georgia's Public Schools",
                href = "https://gbpi.org/retirement-in-georgias-public-schools/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia SB 33 — annual property assessment cap",
                href = "https://legiscan.com/GA/bill/SB33/2025",
                target = "_blank"
              )),
              tags$li(tags$a(
                "WABE — Schools Could Lose Most If Property Tax Legislation Becomes Law",
                href = "https://www.wabe.org/schools-could-lose-most-if-property-tax-legislation-becomes-law/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia Recorder — Kemp Approves Property Tax Relief",
                href = "https://georgiarecorder.com/2026/05/11/kemp-approves-property-tax-relief-for-georgia-homeowners-amid-concerns-over-local-revenues-process/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "SaportaReport — Setting Teachers up for Success",
                href = "https://saportareport.com/setting-teachers-up-for-success/columnists/guestcolumn/derek/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Hatcher — Restoring State Funding for State-Mandated Costs",
                href = "https://www.kerryhatcher.com/restoring-state-funding-for-state-mandated-costs/",
                target = "_blank"
              ))
            )
          ),
          div(
            p(strong("Georgia education finance"), class = "mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "TRS of Georgia — Employer Contribution Rates",
                href = "https://www.trsga.com/employer/contribution-rates/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — FY2027 K-12 Budget Overview",
                href = "https://gbpi.org/overview-2027-fiscal-year-budget-for-k-12-education/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "GBPI — Retirement in Georgia's Public Schools",
                href = "https://gbpi.org/retirement-in-georgias-public-schools/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia SB 33 — annual property assessment cap",
                href = "https://legiscan.com/GA/bill/SB33/2025",
                target = "_blank"
              )),
              tags$li(tags$a(
                "WABE — Schools Could Lose Most If Property Tax Legislation Becomes Law",
                href = "https://www.wabe.org/schools-could-lose-most-if-property-tax-legislation-becomes-law/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Georgia Recorder — Kemp Approves Property Tax Relief",
                href = "https://georgiarecorder.com/2026/05/11/kemp-approves-property-tax-relief-for-georgia-homeowners-amid-concerns-over-local-revenues-process/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "SaportaReport — Setting Teachers up for Success",
                href = "https://saportareport.com/setting-teachers-up-for-success/columnists/guestcolumn/derek/",
                target = "_blank"
              )),
              tags$li(tags$a(
                "Hatcher — Restoring State Funding for State-Mandated Costs",
                href = "https://www.kerryhatcher.com/restoring-state-funding-for-state-mandated-costs/",
                target = "_blank"
              ))
            ),
            p(strong("Geospatial data"), class = "mt-3 mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "U.S. Census TIGER/Line Road Shapefiles",
                href = "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html",
                target = "_blank"
              )),
              tags$li(
                "Atlanta TAD boundaries — City of Atlanta Open Data Portal"
              ),
              tags$li("Atlanta city limits — City of Atlanta Open Data Portal")
            ),
            p(strong("Research"), class = "mt-3 mb-1"),
            tags$ul(
              class = "small",
              tags$li(tags$a(
                "Wen et al. — Students Lose Out as Cities Give Billions in Property Tax Breaks to Businesses (NEPC, 2024)",
                href = "https://nepc.colorado.edu/publication/tax-abatement",
                target = "_blank"
              ))
            )
          )
        )
      )
    )
  ),

  br()
)
