# app.R
library(shiny)
library(ggplot2)
library(ggridges)
library(purrr)
library(DT)
library(dplyr)
library(latex2exp)




# load your simulation results
results <- readRDS("data/results.rds")
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
      
      # NEW: dynamic filters
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
      # NEW: decimals control
      numericInput("decimals", "Table: decimal places", 
                   value = 3, min = 0, max = 10, step = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Density Plots",
                 # explanatory text for full plot
                 h4("Full Distribution"),
                 shiny::p(HTML(
                   "Each ridge corresponds to the <em>distribution</em> of your chosen parameter across all<br/>",
                   "simulations for a fixed design (J, mj, Ti). The red line shows the true parameter value."
                 )),
                 plotOutput("ridge_full", height = "300px"),
                 # explanatory text for zoomed plot
                 h4("Zoomed–In View"),
                 # placeholder for dynamic text:
                 uiOutput("zoom_text"),
                 plotOutput("ridge_zoom", height = "300px")
        ),
        tabPanel("Summary Table",
                 # Some vertical space
                 tags$br(),
                 shiny::p("Below is the lower quantile, median, and upper quantile of the",
                   "estimate distribution for your selections."),
                 DT::DTOutput("summary_table")
        ),
        tabPanel("Failure Rates",
                 # Some vertical space
                 tags$br(),
                 p("Percentage of simulation replicates that failed for each design cell."),
                 DT::DTOutput("fail_table")
        ),
        tabPanel("Example Interpretation",
                 # Some vertical space
                 tags$br(),
                 # A short introduction
                 p("Below is a worked example for the parameter ", strong("beta1"),", where we’ll show you how the density plots and tables can be interpreted."),
                 
                 h4("Density Plots"),
                 p("The density plots show the sampling distributions of the parameter over 100 simulations, for each combination of group size, number of persons per group, and number of time points (J, mj, Ti). The red line marks the true parameter value."),
                 # First screenshot + caption
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
                 
                 # Some vertical space
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
                 
                 # Some vertical space
                 tags$br(),
                 
                h4("Summary Table"),
                p("The summary table shows the chosen percentiles of the the sampling distributions of the parameter over 100 simulations, for each combination of group size, number of persons per group, and number of time points (J, mj, Ti). The default values are the 25th percentile for the lower, and the 75th percentile for the upper value. The table also shows the median and the true value of the parameter."),
                 # Third row: the table screenshot + explanatory bullets
                 fluidRow(
                   column(12,
                          tags$img(src = "beta1_table.png", width = "80%"),
                          tags$figcaption(
                            em("Figure 2"), 
                            " Summary table. Here we can see the exact values of the percentiles and the median, and compare with the true parameter value. Narrow interquartile span indicates high precision."
                          )
                   )
                 ),
                
                # Some vertical space
                tags$br(),
                 
                h4("Failure rates"),
                p("This table shows how many of the 100 repetitions of each simulation setting that failed. Failure can be due to ..."),
                
                # Fourth row: the failure table screenshot
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
  # Reactive filtered data
  df_param <- reactive({
    long_res %>%
      filter(param == input$param,
             J   %in% input$filter_J,
             mj  %in% input$filter_mj,
             Ti  %in% input$filter_Ti)
  })
  
  # parameters from Cohesion application
  true_params <- list(
    mu0         = 1.001573e+00,
    mu1         = -2.190427e-02,
    delta_group = 8.047156e-02,
    sgp_group   = 1.700019e-02,
    #  corr_group  = 3.963931e-01,
    k_group     = 3.242020e+00,
    delta_indiv = 1.698632e-01,
    sgp_indiv   = 6.914247e-03,
    #  corr_indiv  = 5.077924e-01,
    k_indiv     = 4.426851e+00,
    sigma2_e    = 2.173637e-04,
    sigma2_v0   = 3.907732e-07,
    sigma2_tau0 = 2.123306e-06
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
       #    corr_v      = exp(-maxDist/true_params$k_indiv),
           sigma2_tau0 = true_params$sigma2_tau0,
           sigma2_tau1 = true_params$sgp_group,
           delta_tau   = true_params$delta_group,
           kappa_tau   = true_params$k_group,
           # kappa_tau   = maxDist / (-log(true_params$corr_group)),
        #   corr_tau    = exp(-maxDist/true_params$k_group),
           sigma2_e    = true_params$sigma2_e,
           NA_real_)
  }
  
  
  # Full ridge
  output$ridge_full <- renderPlot({
    param <- input$param
    df    <- df_param()   # <-- call the reactive!!
    
    # now build a tiny data.frame of true values, one per Ti
    true_df <- df %>%
      dplyr::distinct(Ti) %>%
      dplyr::mutate(true = purrr::map_dbl(Ti, ~ get_true(param, .x)))
    
    # lookup the “pretty” label
    #xlab_latex <- param_latex_map[[ input$param ]]
    #xlab_latex <- param_latex_map[[param]]
 #   xlab_code <- param_parse_map[[param]]
    
    ggplot(df, aes(x = estimate, y = factor(Ti), fill = factor(mj))) +
      geom_density_ridges(alpha = 0.6, scale = 1) +
      facet_wrap(~ J,labeller = labeller(J = function(j) paste("Groups =", j)))+
      geom_vline(data = true_df,
                 aes(xintercept = true),
                 linetype    = "solid",
                 color       = "red") +
      labs(
        x =  NULL,    # <-- R expression here
        y = "Time Points (Ti)",
        fill = "Persons per Group\n(mj)"
      ) +
   #   xlab(parse(text = xlab_code)) +
      theme_minimal()
  })
  
  ## zoomed ridge
  output$ridge_zoom <- renderPlot({
    param <- input$param
    df    <- df_param()   # <-- call the reactive!!
    lims  <- quantile(df$estimate, probs = input$zoom_q, na.rm=T)
    
    true_df <- df %>%
      dplyr::distinct(Ti) %>%
      dplyr::mutate(true = purrr::map_dbl(Ti, ~ get_true(param, .x)))
    
    # lookup the “pretty” label
  #  xlab_latex <- param_latex_map[[ input$param ]]
    
    ggplot(df, aes(x = estimate, y = factor(Ti), fill = factor(mj))) +
      geom_density_ridges(alpha = 0.6, scale = 1) +
      facet_wrap(~ J,labeller = labeller(J = function(j) paste("Groups =", j))) +
      coord_cartesian(xlim = lims) +
      geom_vline(data = true_df,
                 aes(xintercept = true),
                 linetype     = "solid",
                 color        = "red") +
      labs(
        x    =  NULL,#latex2exp::TeX(xlab_latex),
        y    = "Time Points (Ti)",
        fill = "Persons per Group\n(mj)"
      ) +
      theme_minimal()
  })
  
  # for nice info text
  output$zoom_text <- renderUI({
    # now you can safely use input$zoom_q
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
  
  # Summary table
  output$summary_table <- DT::renderDT({
    param <- input$param
    qs    <- sort(input$table_quantiles)
    d     <- input$decimals
    
    tbl <- df_param() %>%
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
                  options = list(paging = FALSE,   # turn off pagination
                                 dom    = 't' )) %>%
      formatRound(columns = c("true","lower","median","upper"),
                  digits  = d)
  })
  
  # failure rates table
  # Pre‐compute (or reactive‐compute) the failure rates
  failure_rates <- reactive({
    results %>%
      group_by(J, mj, Ti) %>%
      summarise(
        n_failed   = sum(failed, na.rm = TRUE),
        n_total    = n(),
        pct_failed = 100 * n_failed / n_total,
        .groups    = "drop"
      ) %>%
      mutate(
        total_obs = J * mj * Ti
      ) %>%
      # move total_obs to just after Ti
      relocate(total_obs, .after = Ti) %>%
      arrange(J, mj, Ti)
  })
  
  output$fail_table <- DT::renderDT({
    DT::datatable(
      failure_rates(),
      rownames = FALSE,
      options = list(
        paging = FALSE,
        dom    = 't'
      )
    ) %>%
      formatRound("pct_failed", 1)   # show pct_failed to 1 decimal
  })
  
  
}

shinyApp(ui, server)
