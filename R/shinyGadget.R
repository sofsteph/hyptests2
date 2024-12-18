library(shiny)
library(miniUI)

# Define the Shiny gadget
hypGadget <- function() {
  ui <- miniPage(
    gadgetTitleBar("hyptests2: Statistical Analysis and Visualization"),
    miniContentPanel(
      tabsetPanel(
        id = "test_type",
        tabPanel("T-Test",
                 fileInput("t_test_data", "Upload Dataset (CSV):"),
                 uiOutput("t_test_column_selector"),  # Dynamically created column selectors
                 selectInput("t_test_alternative", "Alternative Hypothesis:",
                             c("two.sided", "less", "greater")),
                 numericInput("t_test_mu", "Mean Difference (mu):", value = 0),
                 checkboxInput("t_test_paired", "Paired Samples?", FALSE),
                 checkboxInput("t_test_var_equal", "Equal Variances?", FALSE),
                 actionButton("run_t_test", "Run T-Test")
        ),
        tabPanel("Chi-Square Test",
                 fileInput("chi_square_data", "Upload Contingency Table CSV:"),
                 checkboxInput("chi_square_simulate", "Simulate p-value?", FALSE),
                 checkboxInput("chi_square_rescale", "Rescale p-values?", FALSE),
                 actionButton("run_chi_square", "Run Chi-Square Test")
        ),
        tabPanel("ANOVA",
                 fileInput("anova_data", "Upload Data CSV:"),
                 textInput("anova_formula", "Formula (e.g., `value ~ group`):"),
                 actionButton("run_anova", "Run ANOVA")
        )
      ),
      hr(),
      textOutput("results"),
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )

  server <- function(input, output, session) {
    #  hold results
    results <- reactiveVal()
    plot1 <- reactiveVal()
    plot2 <- reactiveVal()

    # select data columns for t-test
    observe({
      req(input$t_test_data)
      dataset <- read.csv(input$t_test_data$datapath)

      # Dynamically generate selectors for choosing columns
      output$t_test_column_selector <- renderUI({
        column_names <- names(dataset)
        fluidRow(
          column(6, selectInput("t_test_x_col", "Select Column for X:", choices = column_names)),
          column(6, selectInput("t_test_y_col", "Select Column for Y (optional):",
                                choices = c("None", column_names), selected = "None"))
        )
      })
    })

    # T-Test
    observeEvent(input$run_t_test, {
      req(input$t_test_data, input$t_test_x_col)
      dataset <- read.csv(input$t_test_data$datapath)
      x <- dataset[[input$t_test_x_col]]
      y <- if (input$t_test_y_col != "None") dataset[[input$t_test_y_col]] else NULL

      res <- t_test_2.0(
        x, y, alternative = input$t_test_alternative,
        mu = input$t_test_mu, paired = input$t_test_paired, var.equal = input$t_test_var_equal
      )
      results(res)
    })

    # Chi-Square Test
    observeEvent(input$run_chi_square, {
      req(input$chi_square_data)
      data <- as.matrix(read.csv(input$chi_square_data$datapath, row.names = 1))
      res <- chi_square.2.0(data, simulate.p.value = input$chi_square_simulate,
                            rescale.p = input$chi_square_rescale)
      results(res)
    })

    # ANOVA
    observeEvent(input$run_anova, {
      req(input$anova_data, input$anova_formula)
      data <- read.csv(input$anova_data$datapath)
      formula <- as.formula(input$anova_formula)
      res <- anova.2.0(data, formula)
      results(res)
    })

    # Display results
    output$results <- renderText({
      req(results())
      capture.output(results())
    })

    # make plots
    output$plot1 <- renderPlot({
      req(plot1())
      print(plot1())
    })
    output$plot2 <- renderPlot({
      req(plot2())
      print(plot2())
    })

    # Close gadget
    observeEvent(input$done, {
      stopApp(results())
    })
  }

  runGadget(ui, server, viewer = dialogViewer("hyptests2"))
}
