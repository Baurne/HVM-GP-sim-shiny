# app.R
library(shiny)
library(ggplot2)
library(ggridges)
library(purrr)
library(DT)
library(dplyr)
library(latex2exp)

# load your simulation results
results  <- readRDS("data/results.rds")
long_res <- readRDS("data/long_res.rds")

ui <- fluidPage(
  titlePanel("Parameter Recovery Explorer"),
  shiny::p(
    "Welcome to the interactive exploration of our Monte Carlo ",
    "parameter‐recovery study.  Select a parameter below, choose ",
    "quantile bounds to zoom the plot, and view summary statistics ",
    "for any subset of the design."
  ),
  shiny::hr(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("param", "Parameter of interest:",
                  choices = sort(unique(long_res$param))),
      sliderInput("zoom_q", "Zoom quantile bounds:",
                  min = 0, max = 1, value = c(0.01, 0.99), step = 0.01),
      sliderInput("table_quantiles", "Table quantiles (lower & upper):",
                  min = 0, max = 1, value = c(0.25, 0.75), step = 0.01),
      
      # dynamic filters
      selectInput("filter_J", "Groups (J):",
                  choices  = sort(unique(long_res$J)),
                  selected = unique(long_res$J),
                  multiple = TRUE),
      selectInput("filter_mj", "Persons per group (mj):",
                  choices  = sort(unique(long_res$mj)),
                  selected = unique(long_res$mj),
                  multiple = TRUE),
      selectInput("filter_Ti", "Time points (Ti):",
                  choices  = sort(unique(long_res$Ti)),
                  selected = unique(long_res$Ti),
                  multiple = TRUE),
      
      # decimals control
      numericInput("decimals", "Table: decimal places",
                   value = 3, min = 0, max = 10, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Density Plots",
                 h4("Full Distribution"),
                 shiny::p(HTML(
                   "Each ridge corresponds to the <em>distribution</em> of your chosen parameter across all<br/>",
                   "simulations for a fixed design (J, mj, Ti). The red line shows the true parameter value."
                 )),
                 # note about exclusions (per-cell)
                 uiOutput("exclusion_note_plots"),
                 uiOutput("nonneg_note_full"),
                 plotOutput("ridge_full", height = "300px"),
                 
                 h4("Zoomed–In View"),
                 uiOutput("zoom_text"),
                 uiOutput("nonneg_note_zoom"),
                 plotOutput("ridge_zoom", height = "300px")
        ),
        tabPanel("Summary Table",
                 tags$br(),
                 shiny::p("Below is the lower quantile, median, and upper quantile of the",
                          "estimate distribution for your selections."),
                 # note about exclusions (per-cell)
                 uiOutput("exclusion_note_table"),
                 DT::DTOutput("summary_table")
        ),
        
        tabPanel("Example Interpretation",
                 tags$br(),
                 p("Below is a worked example for the parameter ", strong("beta1"),", where we’ll show you how the density plots and tables can be interpreted."),
                 
                 h4("Density Plots"),
                 p("The density plots show the sampling distributions of the parameter over 100 simulations, for each combination of group size, number of persons per group, and number of time points (J, mj, Ti). The red line marks the true parameter value."),
                 fluidRow(
                   column(12,
                          tags$img(src = "beta1_full_distribution.png", width = "100%"),
                          tags$figcaption(
                            em("Figure 1.A"),
                            " Full‐distribution ridge plot. The ridges are centered around the true parameter value (red lines), which means that the estimates have low bias. The estimates does not systematically over- or underestimate the true parameter value.",
                            "The precision of the estimates is evaluated based on how narrow or wide the distribution is. The greatest precision is found for 40 groups and 10 time points, which is expected as this gives the largest sample sizes.",
                            "For small sample sizes, e.g. 4 groups and 4 time points, it is more difficult to find the true parameter value. This can be seen for group sizes 3 and 5, where the densities have more than one peak (multimodal) and are asymmetric."
                          )
                   )
                 ),
                 tags$br(),
                 fluidRow(
                   column(12,
                          tags$img(src = "beta1_zoomed_distribution.png", width = "100%"),
                          tags$figcaption(
                            em("Figure 1.B"),
                            " Zoomed 1–99% quantiles. In some cases, there are a few outliers amongst the parameter estimates. To be able to study how the majority of the estimates behaves, we zoom in on the plot. Here you can clearly see the bulk of the estimates and assess the distribution without extreme outliers.",
                            " Note that the scale of the x-axis changes compared to the full distribution plot."
                          )
                   )
                 ),
                 tags$br(),
                 h4("Summary Table"),
                 p("The summary table shows the chosen percentiles of the the sampling distributions of the parameter over 100 simulations, for each combination of group size, number of persons per group, and number of time points (J, mj, Ti). The default values are the 25th percentile for the lower, and the 75th percentile for the upper value. The table also shows the median and the true value of the parameter."),
                 fluidRow(
                   column(12,
                          tags$img(src = "beta1_table.png", width = "80%"),
                          tags$figcaption(
                            em("Figure 2"),
                            " Summary table. Here we can see the exact values of the percentiles and the median, and compare with the true parameter value. Narrow interquartile span indicates high precision."
                          )
                   )
                 ),
                 tags$br(),
                 h4("Failure rates"),
                 p("This table shows how many of the 100 repetitions of each simulation setting that failed. Failure can be due to ..."),
                 fluidRow(
                   column(12,
                          tags$img(src = "failure_rates_table.png", width = "80%"),
                          tags$figcaption(
                            em("Figure 3"),
                            " Failure rates. 2 % of the repetitions in the simulation setting with 4 groups, 3 persons per group, and 4 time points failed."
                          )
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Helper: detect a replication ID column (rep, iter, etc.) ---
  find_rep_col <- function(x) {
    candidates <- c("rep", "repetition", "replicate", "rep_id",
                    "iter", "iteration", "sim", "draw", "seed")
    hits <- intersect(candidates, names(x))
    if (length(hits)) hits[1] else NA_character_
  }
  rep_col_name <- find_rep_col(long_res)
  
  # Reactive filtered data (by chosen param and design filters),
  # collapsed to ONE ROW PER REPLICATION if the rep id is available.
  df_param <- reactive({
    df <- long_res %>%
      filter(param == input$param,
             J   %in% input$filter_J,
             mj  %in% input$filter_mj,
             Ti  %in% input$filter_Ti)
    
    if (!is.na(rep_col_name)) {
      df <- df %>%
        group_by(J, mj, Ti, param, .data[[rep_col_name]]) %>%
        summarise(
          estimate = dplyr::first(estimate[!is.na(estimate)]),
          .groups = "drop"
        )
    } else {
      # Optional: warn once if we can't collapse to rep-level
      showNotification("No replication ID column found; counts reflect rows, not replications.", type = "warning", duration = 5)
    }
    df
  })
  
  # === Apply parameter-specific exclusion rules and build per-cell counts ===
  # - For delta_tau and delta_v: exclude estimate < -5 OR > 5
  # - For kappa_tau and kappa_v: exclude estimate > 50
  # - For sigma2_tau1 and sigma2_v1: exclude estimate > 5
  df_param_filtered <- reactive({
    df_raw <- df_param()
    
    if (nrow(df_raw) == 0) {
      return(list(df = df_raw, per_cell = NULL, cmp_text = NULL))
    }
    
    # Decide rule based on the single selected parameter
    p <- unique(df_raw$param); p <- if (length(p) == 1) p else input$param
    
    # Default: keep everything (no exclusions)
    rule <- function(x) rep(FALSE, length(x))
    cmp_text <- NULL
    
    if (p %in% c("delta_tau", "delta_v")) {
      rule     <- function(x) (x < -5) | (x > 5)
      cmp_text <- "< -5 or > 5"
    } else if (p %in% c("kappa_tau", "kappa_v")) {
      rule     <- function(x) x > 50
      cmp_text <- "> 50"
    } else if (p %in% c("sigma2_tau1", "sigma2_v1")) {
      rule     <- function(x) x > 5
      cmp_text <- "> 5"
    }
    
    # Per-cell, per-replication exclusion counts (after collapsing to 1 row/rep)
    per_cell <- df_raw %>%
      dplyr::group_by(J, mj, Ti) %>%
      dplyr::summarise(
        n_total    = dplyr::n(),
        n_excluded = sum(rule(estimate), na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      dplyr::filter(n_excluded > 0) %>%
      dplyr::arrange(dplyr::desc(n_excluded))
    
    # Filtered data used by plots/tables
    df_kept <- df_raw %>% dplyr::filter(!rule(estimate))
    
    list(df = df_kept, per_cell = per_cell, cmp_text = cmp_text)
  })
  
  # Helper that returns the note (or NULL)
  nonnegative_params <- c(
    "kappa_tau", "kappa_v",
    "sigma2_e", "sigma2_v0", "sigma2_v1",
    "sigma2_tau0", "sigma2_tau1"
  )
  
  make_nonneg_note <- reactive({
    param <- input$param
    
    if (param %in% nonnegative_params) {
      notes <- list(
        tags$p(
          style = "color:#555; font-style:italic; margin-top:0.5em;",
          "Note: This parameter cannot take negative values. ",
          "Due to the smoothing in the density plots, it may look as if there are some negative values."
        )
      )
      
      # Add special extra note for sigma2_e
      if (param == "sigma2_e") {
        notes <- append(notes, list(
          tags$p(
            style = "color:#555; font-style:italic; margin-top:0.5em;",
            "Note 2: The residual variance is very small, and corresponds to only 0.9% of the total variability. ",
            "Thus, one should not be worried of it seeming biased in these plots."
          )
        ))
      }
      
      # Add special extra note for kappa_tau
      if (param == "kappa_tau") {
        notes <- append(notes, list(
          tags$p(
            style = "color:#555; font-style:italic; margin-top:0.5em;",
            "Note 2: The parameter is slightly biased, and gives more conservative estimates than the true value. ",
            "For the largest number of groups, it starts moving towards the true value."
          )
        ))
      }
      
      # Add special extra note for kappa_v
      if (param == "kappa_v") {
        notes <- append(notes, list(
          tags$p(
            style = "color:#555; font-style:italic; margin-top:0.5em;",
            "Note 2: The parameter is slightly biased for small sample sizes, ",
            "and gives more conservative estimates than the true value."
          )
        ))
      }
      
      do.call(tagList, notes)
    } else {
      NULL
    }
  })
  
  output$nonneg_note_full <- renderUI({ make_nonneg_note() })
  output$nonneg_note_zoom <- renderUI({ make_nonneg_note() })
  
  
  # parameters from Cohesion application
  true_params <- list(
    mu0         = 1.001582e+00,
    mu1         = -2.191354e-02,
    delta_group = 8.124248e-02,
    sgp_group   = 1.694252e-02,
    #  corr_group  = 3.963931e-01,
    k_group     = 3.244458e+00,
    delta_indiv = 1.699087e-01,
    sgp_indiv   = 6.894795e-03,
    #  corr_indiv  = 5.077924e-01,
    k_indiv     = 4.446048e+00,
    sigma2_e    = 2.179537e-04,
    sigma2_v0   = 1.290276e-07,
    sigma2_tau0 = 9.128659e-25
  )
  
  get_true <- function(param, Ti) {
    maxDist <- Ti - 1  # time grid 0:(Ti-1)
    switch(param,
           beta0       = true_params$mu0,
           beta1       = true_params$mu1,
           sigma2_v0   = true_params$sigma2_v0,
           sigma2_v1   = true_params$sgp_indiv,
           delta_v     = true_params$delta_indiv,
           kappa_v     = true_params$k_indiv,
           #kappa_v     = maxDist / (-log(true_params$corr_indiv)),
           #corr_v      = exp(-maxDist/true_params$k_indiv),
           sigma2_tau0 = true_params$sigma2_tau0,
           sigma2_tau1 = true_params$sgp_group,
           delta_tau   = true_params$delta_group,
           kappa_tau   = true_params$k_group,
           # kappa_tau   = maxDist / (-log(true_params$corr_group)),
           # corr_tau    = exp(-maxDist/true_params$k_group),
           sigma2_e    = true_params$sigma2_e,
           NA_real_)
  }
  
  # === Notes shown above plots and table (ONLY per-cell counts) ===
  output$exclusion_note_plots <- renderUI({
    info <- df_param_filtered()
    pc   <- info$per_cell
    if (is.null(pc) || nrow(pc) == 0) return(NULL)
    
    expl_text <- if (input$param %in% c("delta_tau", "delta_v")) {
      "In some cases, the model does not find any temporally changing effect at this level. This makes the delta parameter unidentifiable, and these repetitions are excluded from the results shown here."
    } else {
      "In some cases, the model does not find any temporally changing effect at this level. This makes the parameter unidentifiable, and these repetitions are excluded from the results shown here."
    }
    
    show_rows <- head(pc, 10)
    items <- apply(show_rows, 1, function(r) {
      pct <- 100 * as.numeric(r[["n_excluded"]]) / as.numeric(r[["n_total"]])
      tags$li(sprintf("J=%s, mj=%s, Ti=%s: %s/%s excluded (%.1f%%)",
                      r[["J"]], r[["mj"]], r[["Ti"]],
                      r[["n_excluded"]], r[["n_total"]], pct))
    })
    more <- if (nrow(pc) > 10) tags$li(sprintf("…and %d more cells", nrow(pc) - 10)) else NULL
    
    title <- if (!is.null(info$cmp_text)) {
      sprintf("Excluded estimates per design cell (criterion: %s)", info$cmp_text)
    } else {
      "Excluded estimates per design cell"
    }
    
    tagList(
      tags$p(style = "color:#555; font-style:italic; margin-top:0.5em;", expl_text),
      tags$details(open = "open",
                   tags$summary(title),
                   tags$ul(items, more)
      )
    )
  })
  
  
  output$exclusion_note_table <- renderUI({
    info <- df_param_filtered()
    pc   <- info$per_cell
    if (is.null(pc) || nrow(pc) == 0) return(NULL)
    
    expl_text <- if (input$param %in% c("delta_tau", "delta_v")) {
      "In some cases, the model does not find any temporally changing effect at this level. This makes the delta parameter unidentifiable, and these repetitions are excluded from the results shown here."
    } else {
      "In some cases, the model does not find any temporally changing effect at this level. This makes the parameter unidentifiable, and these repetitions are excluded from the results shown here."
    }
    
    show_rows <- head(pc, 10)
    items <- apply(show_rows, 1, function(r) {
      pct <- 100 * as.numeric(r[["n_excluded"]]) / as.numeric(r[["n_total"]])
      tags$li(sprintf("J=%s, mj=%s, Ti=%s: %s/%s excluded (%.1f%%)",
                      r[["J"]], r[["mj"]], r[["Ti"]],
                      r[["n_excluded"]], r[["n_total"]], pct))
    })
    more <- if (nrow(pc) > 10) tags$li(sprintf("…and %d more cells", nrow(pc) - 10)) else NULL
    
    title <- if (!is.null(info$cmp_text)) {
      sprintf("Excluded estimates per design cell (criterion: %s)", info$cmp_text)
    } else {
      "Excluded estimates per design cell"
    }
    
    tagList(
      tags$p(style = "color:#555; font-style:italic; margin-top:0.5em;", expl_text),
      tags$details(open = "open",
                   tags$summary(title),
                   tags$ul(items, more)
      )
    )
  })
  
  
  # Full ridge (uses filtered, rep-collapsed data)
  output$ridge_full <- renderPlot({
    param <- input$param
    df    <- df_param_filtered()$df
    
    validate(need(nrow(df) > 0, "No estimates to display (all were excluded by the threshold and/or filters)."))
    
    true_df <- df %>%
      dplyr::distinct(Ti) %>%
      dplyr::mutate(true = purrr::map_dbl(Ti, ~ get_true(param, .x)))
    
    ggplot(df, aes(x = estimate, y = factor(Ti), fill = factor(mj))) +
      geom_density_ridges(alpha = 0.6, scale = 1) +
      facet_wrap(~ J, labeller = labeller(J = function(j) paste("Groups =", j))) +
      geom_vline(data = true_df,
                 aes(xintercept = true),
                 linetype    = "solid",
                 color       = "red") +
      labs(
        x =  NULL,
        y = "Time Points (Ti)",
        fill = "Persons per Group\n(mj)"
      ) +
      theme_minimal()
  })
  
  # Zoomed ridge (uses filtered, rep-collapsed data)
  output$ridge_zoom <- renderPlot({
    param <- input$param
    df    <- df_param_filtered()$df
    
    validate(need(nrow(df) > 0, "No estimates to display (all were excluded by the threshold and/or filters)."))
    
    lims  <- quantile(df$estimate, probs = input$zoom_q, na.rm = TRUE)
    
    true_df <- df %>%
      dplyr::distinct(Ti) %>%
      dplyr::mutate(true = purrr::map_dbl(Ti, ~ get_true(param, .x)))
    
    ggplot(df, aes(x = estimate, y = factor(Ti), fill = factor(mj))) +
      geom_density_ridges(alpha = 0.6, scale = 1) +
      facet_wrap(~ J, labeller = labeller(J = function(j) paste("Groups =", j))) +
      coord_cartesian(xlim = lims) +
      geom_vline(data = true_df,
                 aes(xintercept = true),
                 linetype     = "solid",
                 color        = "red") +
      labs(
        x    =  NULL,
        y    = "Time Points (Ti)",
        fill = "Persons per Group\n(mj)"
      ) +
      theme_minimal()
  })
  
  # Zoom text
  output$zoom_text <- renderUI({
    z <- input$zoom_q
    HTML(paste0(
      "<p style='margin-bottom:4px;'>",
      "The x-axis is limited to the central <strong>",
      sprintf("%d%%–%d%%", z[1]*100, z[2]*100),
      "</strong> quantiles so you can better see where most estimates lie,",
      " without extreme outliers stretching the scale.",
      "</p>",
      "<p style='margin-bottom:4px;'>",
      "You can adjust the slider <strong>Zoom quantile bounds</strong> to change these bounds.",
      "</p>",
      "<p style='margin-bottom:4px;'>",
      "Moving the lower bound up excludes more of the left tail; moving the upper bound down excludes more of the right tail.",
      " The red line marks the true parameter value.",
      "</ul>"
    ))
  })
  
  # Summary table (uses filtered, rep-collapsed data)
  output$summary_table <- DT::renderDT({
    param <- input$param
    qs    <- sort(input$table_quantiles)
    d     <- input$decimals
    df    <- df_param_filtered()$df
    
    validate(need(nrow(df) > 0, "No rows to summarize (all were excluded by the threshold and/or filters)."))
    
    tbl <- df %>%
      group_by(J, mj, Ti) %>%
      summarise(
        lower  = unname(quantile(estimate, qs[1], na.rm = TRUE)),
        median = unname(quantile(estimate, 0.5,    na.rm = TRUE)),
        upper  = unname(quantile(estimate, qs[2], na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      # add the true value column
      mutate(true = map_dbl(Ti, ~ get_true(param, .x))) %>%
      arrange(J, mj, Ti) %>%
      as.data.frame()
    
    DT::datatable(tbl, rownames = FALSE,
                  options = list(paging = FALSE, dom = 't')) %>%
      formatRound(columns = c("true","lower","median","upper"), digits = d)
  })
  
 }

shinyApp(ui, server)

