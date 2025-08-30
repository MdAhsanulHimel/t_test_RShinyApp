

# Load packages -----------------------------------------------------------

library(shiny)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Source function
source("t_test_flex.R", local = TRUE)

# ---------- Helpers ----------
parse_vector <- function(txt) {
  if (is.null(txt) || length(txt) == 0) txt <- ""
  if (!is.character(txt)) txt <- as.character(txt)
  txt <- trimws(txt)
  if (!nzchar(txt)) return(list(vec = numeric(0), bad = character(0)))
  tokens <- unlist(strsplit(txt, "[,\\s]+", perl = TRUE))
  tokens <- tokens[nzchar(tokens)]
  nums <- suppressWarnings(as.numeric(tokens))
  bad  <- tokens[is.na(nums)]
  vec  <- nums[!is.na(nums) & is.finite(nums)]
  list(vec = vec, bad = bad)
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("t-test Shiny App"),
  sidebarLayout(
    sidebarPanel(
      tags$p("Paste numbers either comma-separated or one per line. Decimal point is '.'"),
      textAreaInput("x", "Vector X", rows = 3,
                    placeholder = "116, 118, 117\nor\n116\n118\n117"),
      textAreaInput("y", "Vector Y", rows = 3,
                    placeholder = "10, 11, 12\nor\n10\n11\n12"),
      
      radioButtons("test_type", "Kind of test:",
                   choices = c("One-sample t test" = "one",
                               "Two-sample t test" = "two",
                               "Paired sample t test" = "paired"),
                   selected = "two"
      ),
      
      conditionalPanel(
        condition = "input.test_type == 'one'",
        numericInput("mu0", HTML("Null mean (μ<sub>0</sub>)"), value = 0, step = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.test_type == 'two'",
        radioButtons("var_equal", "Variance assumption:",
                     choices = c("Equal (Pooled t)" = "equal",
                                 "Unequal (Welch t)" = "unequal"),
                     selected = "unequal")
      ),
      
      radioButtons("side", "Alternative (tail):",
                   choices = c("Left ( < )" = "left",
                               "Right ( > )" = "right",
                               "Both ( ≠ )" = "both"),
                   selected = "both", inline = TRUE),
      
      radioButtons("alpha", HTML("Significance level (α):"),
                   choices = c("0.01", "0.05", "0.10"),
                   selected = "0.05", inline = TRUE),
      
      actionButton("run", "Run test", class = "btn btn-primary")
    ),
    mainPanel(
      verbatimTextOutput("report", placeholder = TRUE),
      uiOutput("dl_report_ui"),
      plotOutput("plot", height = "420px"),
      uiOutput("dl_plot_ui")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  raw <- eventReactive(input$run, {
    list(
      x_txt = input$x,
      y_txt = input$y,
      test_type = input$test_type,
      side = input$side,
      alpha = as.numeric(input$alpha),
      var_equal_choice = input$var_equal %||% "unequal",
      mu0 = input$mu0 %||% 0
    )
  })
  
  get_params <- reactive({
    r <- raw(); req(r)
    
    px <- parse_vector(r$x_txt)
    py <- parse_vector(r$y_txt)
    
    # Validate X always
    if (length(px$bad)) {
      shiny::validate(shiny::need(FALSE,
                                  paste0("Vector X contains non-numeric values: ", paste(px$bad, collapse = ", "))))
    }
    
    if (r$test_type == "one") {
      # Do NOT validate Y for one-sample (it's optional)
      shiny::validate(shiny::need(length(px$vec) >= 2,
                                  "For one-sample t test, provide at least 2 numbers in Vector X."))
      y_vec <- NULL
      paired <- FALSE
      var_equal <- FALSE
      mu0 <- r$mu0
      
    } else if (r$test_type == "two") {
      # Validate Y only here
      if (length(py$bad)) {
        shiny::validate(shiny::need(FALSE,
                                    paste0("Vector Y contains non-numeric values: ", paste(py$bad, collapse = ", "))))
      }
      shiny::validate(
        shiny::need(length(px$vec) >= 2, "For two-sample t test, provide at least 2 numbers in Vector X."),
        shiny::need(length(py$vec) >= 2, "For two-sample t test, provide at least 2 numbers in Vector Y.")
      )
      y_vec <- py$vec
      paired <- FALSE
      var_equal <- (r$var_equal_choice == "equal")
      mu0 <- 0
      
    } else { # paired
      # Validate Y only here
      if (length(py$bad)) {
        shiny::validate(shiny::need(FALSE,
                                    paste0("Vector Y contains non-numeric values: ", paste(py$bad, collapse = ", "))))
      }
      shiny::validate(
        shiny::need(length(px$vec) >= 2, "For paired t test, Vector X needs at least 2 numbers."),
        shiny::need(length(py$vec) >= 2, "For paired t test, Vector Y needs at least 2 numbers."),
        shiny::need(length(px$vec) == length(py$vec),
                    sprintf("For paired t test, X and Y must have equal length. Currently: n1 = %d, n2 = %d.",
                            length(px$vec), length(py$vec)))
      )
      y_vec <- py$vec
      paired <- TRUE
      var_equal <- FALSE
      mu0 <- 0
    }
    
    list(
      x = px$vec,
      y = y_vec,
      side = r$side,
      alpha = r$alpha,
      paired = paired,
      var_equal = var_equal,
      mu0 = mu0
    )
  })
  
  output$report <- renderText({
    withProgress(message = "Running t-test...", value = 0, {
      p <- get_params(); incProgress(0.7)
      res <- t_test_flex(
        x = p$x, y = p$y,
        side = p$side, sig = p$alpha,
        plot = FALSE, paired = p$paired,
        var.equal = p$var_equal, mu0 = p$mu0
      )
      incProgress(0.95)
      res$report
    })
  })
  
  output$dl_report <- downloadHandler(
    filename = function() sprintf("t_test_report_%s.txt", Sys.Date()),
    content = function(file) {
      p <- get_params()
      res <- t_test_flex(
        x = p$x, y = p$y, side = p$side, sig = p$alpha,
        plot = FALSE, paired = p$paired, var.equal = p$var_equal, mu0 = p$mu0
      )
      writeLines(res$report, con = file)
    }
  )
  
  output$dl_report_ui <- renderUI({
    req(raw())  # available only after the Run button is clicked
    downloadButton("dl_report", "Download report")
  })
  
  
  
  output$plot <- renderPlot({
    p <- get_params()
    t_test_flex(
      x = p$x, y = p$y,
      side = p$side, sig = p$alpha,
      plot = TRUE, paired = p$paired,
      var.equal = p$var_equal, mu0 = p$mu0
    )
  })
  
  output$dl_plot <- downloadHandler(
    filename = function() sprintf("t_test_plot_%s.png", Sys.Date()),
    content = function(file) {
      p <- get_params()
      png(file, width = 1100, height = 800, res = 150)
      t_test_flex(
        x = p$x, y = p$y, side = p$side, sig = p$alpha,
        plot = TRUE, paired = p$paired, var.equal = p$var_equal, mu0 = p$mu0
      )
      dev.off()
    }
  )
  
  output$dl_plot_ui <- renderUI({
    req(raw())  # available only after the Run button is clicked
    downloadButton("dl_plot", "Download plot")
  })
  
}

shinyApp(ui, server)
