library(shiny);library(shinythemes);library(ggplot2)

##########################################################

ui = fluidPage(
  titlePanel("Fit Linear Regression Model by F Tabeei (E-Mail: fereshteh.tabeei@mail.um.ac.ir) and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  theme = shinytheme("superhero"),
  tags$style(HTML("
       #calculate_btn {
         background-color: gray;
         color: white;
         border: none;
         padding: 10px 30px;
         text-align: center;
         font-size: 16px;
         cursor: pointer;
         border-radius: 10px; 
       } 
     ")),
  sidebarLayout(
    sidebarPanel(
      textInput("x", "Enter X values (comma-separated):", value = "21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2"),
      textInput("y", "Enter Y values (comma-separated):", value = "2.62, 2.875, 2.32, 3.215, 3.44, 3.46, 3.57, 3.19, 3.15, 3.44"),
      numericInput("alpha", "Confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001),
      checkboxInput("calc_ci", "Calculate Confidence Interval", FALSE),
      
      conditionalPanel(
        condition = "input.calc_ci",
        selectInput("CItype", "Select confidence interval type:",
                    choices = list("Confidence interval for mean response" = "mean",
                                   "Prediction interval for y0" = "prediction",
                                   "Confidence interval for y0 bar with level (1-alpha)" = "Mprediction"),
                    selected = "mean")
      ),
      
      # Panel only for input fields based on CI type
      conditionalPanel(
        condition = "input.CItype == 'mean' && input.calc_ci",
        h3("Mean Response"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      
      conditionalPanel(
        condition = "input.CItype == 'prediction' && input.calc_ci",
        h3("Prediction Interval (y0hat)"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      
      conditionalPanel(
        condition = "input.CItype == 'Mprediction' && input.calc_ci",
        h3("Mprediction Interval (y0bar)"),
        numericInput("x0", "Enter x0 value for prediction:", value = 14.3),
        numericInput("m", "Enter m:", value = 5, min = 0, step = 1),
        numericInput("alpha", "Enter confidence level (alpha):", value = 0.05, min = 0.001, max = 1, step = 0.001)
      ),
      checkboxInput("remove_intercept", "If intercept is non-significant, fit model without intercept?", value = TRUE),
      actionButton("calculate_btn", "Calculate")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.calculate_btn > 0",
        h4("Scatter Plot"),
        plotOutput("scatterPlot"),
        h4("Regression Summary"),
        verbatimTextOutput("modelSummary"),
        verbatimTextOutput("regressionTests"),
        h4("MSE "),
        verbatimTextOutput("mse"),
        
        conditionalPanel(
          condition = "input.calc_ci",
          h4("Confidence Interval"),
          verbatimTextOutput("predictions"),
          verbatimTextOutput("confidenceIntervalOutput")
        )
      )
    )
  )
)

server.reg = function(input, output, session) {
  
  observeEvent(input$calculate_btn, {
    x = as.numeric(unlist(strsplit(input$x, ",")))
    y = as.numeric(unlist(strsplit(input$y, ",")))
    x0 = input$x0
    reg_model = lm(y ~ x)
    model_summary = summary(reg_model)
    alpha = input$alpha
    p_value_intercept = model_summary$coefficients[1, 4]
    slope_test = coef(summary(reg_model))[2, 4]
    intercept_test = coef(summary(reg_model))[1, 4]
    model_significance = anova(reg_model)$"Pr(>F)"[1]
    check_bool <- 0
    if (input$remove_intercept && p_value_intercept > alpha) {
      reg_model <- lm(y ~ 0 + x)
      b0 <- 0
      b1 <- coef(reg_model)[1]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 1)
      sxx <- sum(x^2)
      yhat = b0 + b1 * x
      yhat0 = b0 + b1 * x0
      check_bool <- 1
    } 
    else {
      b0 <- coef(reg_model)[1]
      b1 <- coef(reg_model)[2]
      MSE <- sum(residuals(reg_model)^2) / (length(x) - 2)
      sxx <- sum((x - mean(x))^2)
      yhat = b0 + b1 * x
      yhat0 = b0 + b1 * x0
    }
    output$modelSummary <- renderPrint({
      cat("Regression Model Summary:\n")
      print(summary(reg_model))
    })
    
    output$regressionTests <- renderPrint({
      cat("Test Results:\n")
      cat("Slope test p-value:", slope_test, "\n")
      cat("Intercept test p-value:", intercept_test, "\n")
      cat("Overall model significance test p-value:", model_significance, "\n")
    })
    
    calc_ci <- function(z) {
      df <- length(x) - ifelse(input$remove_intercept, 1, 2)
      t_value <- qt(1 - alpha / 2, df = df)
      len <- ifelse(input$remove_intercept, 0, 1/length(x))
      se <- sqrt(MSE * (if (input$CItype == "mean") {
        len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      } else if (input$CItype == "prediction") {
        1 + len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      } else if (input$CItype == "Mprediction") {
        1 / input$m + len + (if (input$remove_intercept) z^2 else (z - mean(x))^2) / sxx
      }))
      
      lower <- (b0 + b1 * z) - t_value * se
      upper <- (b0 + b1 * z) + t_value * se
      
      list(lower = lower, upper = upper)
    }
    output$predictions = renderPrint({
      cat("Predicted value for x0 = ", x0, ": yhat0 = ", yhat0, "\n")
    })
    output$mse = renderPrint({
      cat("MSE = ", MSE, "\n")
    })
    
    if (input$calc_ci) {
      ci <- calc_ci(x0)
      output$confidenceIntervalOutput <- renderPrint({
        cat("Confidence Interval:\n")
        cat("Lower bound =", ci$lower, "\n")
        cat("Upper bound =", ci$upper, "\n")
      })
    } 
    bounds <- data.frame(x = x, y = y)
    bounds$lower <- sapply(bounds$x, function(z) calc_ci(z)$lower)
    bounds$upper <- sapply(bounds$x, function(z) calc_ci(z)$upper)
    if(check_bool){
      output$scatterPlot = renderPlot({
        ggplot(bounds, aes(x = x, y = y)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y~0+x, se = FALSE, color = "dark red") +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#FF8000", alpha = 0.15) +
          ggtitle("Linear Regression With No_Intercept Ray+ Confidence Intervals") +
          xlab("X") +
          ylab("Y")
      })
    }
    else{
      output$scatterPlot = renderPlot({
        ggplot(bounds, aes(x = x, y = y)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "dark red") +
          geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#CC00CC", alpha = 0.15) +
          ggtitle("Linear Regression + Confidence Intervals") +
          xlab("X") +
          ylab("Y")
      })
    }
  })
}

shinyApp(ui,server.reg)