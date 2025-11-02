
library(shiny)
library(shinythemes)
######################################################
generate_sequence <- function(x0, k, n) {
 x0_str <- sprintf(paste0("%0", k, "s"), x0)
 x0_str <- gsub(" ", "0", x0_str)
 sequence <- character(n)
  for (i in 1:n) {
   x0_num <- as.numeric(x0_str)
   x_squared <- x0_num^2
 
 x_squared_str <- format(x_squared, scientific = FALSE)
 x_squared_str <- gsub(" ", "0", sprintf(paste0("%0", 2 * k, "s"), x_squared_str))
  if (k %% 2 == 0) {
    start <- k / 2 + 1
    end <- start + k - 1}
  else {
    start <- (k + 1) / 2
    end <- start + k - 1}
 start <- floor(start)
 end <- floor(end)
 x1_str <- substr(x_squared_str, start, end)
 x1_str <- sprintf(paste0("%0", k, "s"), x1_str)
    x1_str <- gsub(" ", "0", x1_str)
 sequence[i] <- x1_str
x0_str <- x1_str}
  
 return(sequence)
}

ui.middlesquare <- fluidPage(
 theme = shinytheme("cerulean"),
 titlePanel("Generating Random Numbers (Uniform Distribution) by Using the Middle-Square Method by N. Jahani, and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
 sidebarLayout(
 sidebarPanel(
 numericInput("k", "Number of Digits (k):", value = 4, min = 1, max = 10, step = 1),
 textInput("x0", "Initial Number (x0):", value = "1234", placeholder = "Enter a number with k digits"),
 numericInput("n", "Number of Random Numbers to Generate (n):", value = 10, min = 1, step = 1),
 actionButton("generate", "Generate Random Numbers")),
 mainPanel(
    h3("Sequence of Generated Random Numbers:"),
   tableOutput("sequenceTable"))))

server.middlesquare <- function(input, output, session) {
 observe({
  k <- input$k
  updateTextInput(session, "x0",
               placeholder = paste0("Enter a ", k, "-digit number"))})
  
 observeEvent(input$generate, {
  x0 <- input$x0
  k <- input$k
  n <- input$n

 if (!grepl(paste0("^\\d{", k, "}$"), x0)) {
    showModal(modalDialog(
     title = "Input Error",
     paste("Please enter an initial number (x0) with exactly", k, "digits."),
     easyClose = TRUE,
      footer = NULL))

 return()
}

print(paste("Generating sequence with x0 =", x0, ", k =", k, ", n =", n))
  sequence <- tryCatch({
  generate_sequence(x0, k, n)},
  error = function(e) {
  showModal(modalDialog(
      title = "Error",
      paste("An error occurred during sequence generation:", e$message),
      easyClose = TRUE,
      footer = NULL))

 return(NULL)
})
    
 if (is.null(sequence)) return() 
    print(paste("Generated sequence:", paste(sequence, collapse = ", ")))
    output$sequenceTable <- renderTable({
    data.frame(Index = 1:n, Numbers = sequence, stringsAsFactors = FALSE)
  })
 })
}

shinyApp(ui = ui.middlesquare, server = server.middlesquare)