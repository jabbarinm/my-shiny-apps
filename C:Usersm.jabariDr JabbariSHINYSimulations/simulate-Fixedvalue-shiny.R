library(shiny)
library(shinythemes)
###########################################################
ui.fixed <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("تولید اعداد تصادفی به روش مضرب ثابت توسط محمدرضاشجاعيان فر و دكتر مهدي جباري نوقابي (E-mail: jabbarinm@um.ac.ir)"),
  sidebarLayout(
   sidebarPanel(
   numericInput("value", "مقدار اولیه (value):", value = 7, min = 0),
   numericInput("a", "ضریب ضربی (a):", value = 5, min = 1),
   numericInput("c", "ثابت افزایشی (c):", value = 3, min = 0),
   numericInput("m", "مدول (m):", value = 16, min = 1),
   numericInput("repeats", "تعداد تکرار:", value = 10, min = 1),
   actionButton("generate", "تولید اعداد تصادفی", class = "btn-primary")),
   mainPanel(
      tableOutput("random_numbers"),
      plotOutput("random_plot"))
  )
 )

server.fixed <- function(input, output) {
 random_data <- eventReactive(input$generate, {
   value <- input$value
   a <- input$a
   c <- input$c
   m <- input$m
   repeats <- input$repeats

   constant <- function(value, a, c, m, repeats) {
     random_numbers <- numeric(repeats)
     X <- value
       for (i in 1:repeats) {
       X <- (a * X + c) %% m
       random_numbers[i] <- X / m  
  }
return(random_numbers)

}

 constant(value, a, c, m, repeats)
})

 output$random_numbers <- renderTable({
   data.frame("Index" = 1:input$repeats, "Random Number" = random_data())
 })

 output$random_plot <- renderPlot({
  plot(random_data(), type = "o", col = "blue", 
      xlab = "Index", ylab = "Random Number",
      main = "اعداد تصادفی تولیدشده بازه صفر تا يک ")
 })
}

shinyApp(ui = ui.fixed, server = server.fixed)
