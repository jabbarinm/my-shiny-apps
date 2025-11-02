library(shiny)
library(shinythemes)

##########################################################

generate_homogeneous_poisson_process <- function(lambda) {
  u <- runif(1)
  t <- exp(-lambda)
  i <- 1
  while (u >= t) {
    u <- u * runif(1)
    i <- i + 1
  }
  return(i - 1)
}

ui.poisson1 <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Simulate from Poisson Distribution by F. Tabeei (E-Mail: fereshteh.tabeei@mail.um.ac.ir) and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
    tags$style(HTML("
       #generate {
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
      numericInput("lambda", "Lambda (Rate):", value = 3, min = 0.1, step = 0.1),
      numericInput("sampleSize", "Sample Size:", value = 1000, min = 10, step = 10),
      checkboxInput("showPoisson", "Compare with Poisson Distribution", value = FALSE),
      actionButton("generate", "Simulate")
    ),
    
    mainPanel(
      h4(textOutput("titleText")),
      verbatimTextOutput("generatedNumbers"),
      plotOutput("histogramPlot")
    )
  )
)

server.poisson1 <- function(input, output) {
  observeEvent(input$generate, {
    sampleSize <- input$sampleSize
    lambda <- input$lambda
    
    if (lambda <= 0 || sampleSize <= 0) {
      showNotification("Lambda and Sample Size must be positive.", type = "error")
      return()
    }
    
    homopoisson <- replicate(sampleSize, generate_homogeneous_poisson_process(lambda))
    
    output$titleText <- renderText({
      " Generated Random Numbers"
    })
    
    output$generatedNumbers <- renderPrint({
      homopoisson
    })
    
    output$histogramPlot <- renderPlot({
      hist_data <- hist(homopoisson, plot = FALSE)
      ylim_max <- ifelse(max(hist_data$counts) > 0, max(hist_data$counts) * 1.2, 1)
      
      barplot(
        hist_data$counts,
        names.arg = hist_data$breaks[-length(hist_data$breaks)],
        col = "darkcyan",
        border = "white",
        main = paste("Histogram of Poisson Process Events (λ =", lambda, ")"),
        xlab = "Number of Events",
        ylab = "Frequency",
        ylim = c(0, ylim_max)
      )
      
      if (input$showPoisson) {
        x_vals <- 0:max(homopoisson)
        pois_probs <- dpois(x_vals, lambda) * sampleSize
        
        if (length(x_vals) == length(pois_probs)) {
          barplot(
            pois_probs,
            names.arg = x_vals,
            col = rgb(1, 0, 0, 0.5),
            border = "white",
            add = TRUE
          )
        }
      }
    })
  })
}

shinyApp(ui = ui.poisson1, server = server.poisson1)
