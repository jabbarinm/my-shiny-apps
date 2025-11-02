####################################################
library(shiny)
library(shinythemes)
###################################################
generate_poisson <- function(lambda) {
   l <- exp(-lambda)
   p <- 1
   k <- 0
 while (TRUE) {
    u <- runif(1, min = 0, max = 1)
    p <- p * u
    k <- k + 1
 if (p <= l) {
  break
  }
 }
 return(k - 1)
}

ui.poisson <- fluidPage(
theme = shinytheme("cerulean"),
titlePanel("Simualtion from Poisson Distribution (Poisson Process) by N. Jahani and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
sidebarLayout(sidebarPanel(
  h3("Settings"),
  br(),
  numericInput("lambda", " λ:", value = 5, min = 0.1, step = 0.1),
  numericInput("num_samples"," n:", value = 10, min = 1),
  actionButton("generate", "Random number generation", class = "btn-primary"),
  br(),
  br(),
  p("Random numbers are generated from the Poisson distribution")),

mainPanel(
  h4("results"),
  textOutput("poisson_result"),
  br(),
  plotOutput("poisson_histogram")
  )
 )
)
server.poisson <- function(input, output) {
 observeEvent(input$generate, {
 poisson_numbers <- replicate(input$num_samples, generate_poisson(lambda = input$lambda))
 output$poisson_result <- renderText({
 paste("Generated random numbers: ", paste(poisson_numbers, collapse = ", "))
})
    
output$poisson_histogram <- renderPlot({
  hist(poisson_numbers, breaks = max(poisson_numbers) - min(poisson_numbers), 
  main = paste("poisson_histogram (λ =", input$lambda, ")"),
  col = "lightblue", border = "black")
  })
 })
}

shinyApp(ui = ui.poisson, server = server.poisson)