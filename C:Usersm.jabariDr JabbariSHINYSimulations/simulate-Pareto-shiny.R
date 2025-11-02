library(shiny)
library(actuar)

##################################################
ui.simpareto=fluidPage(
  
  titlePanel("Inverse Transform Method(X~pareto) by F Tabeei (E-Mail: fereshteh.tabeei@mail.um.ac.ir) and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("sample_size","Sample Size:",value=100,min=1),
      
      #min is 0.01 because both Parameter should be bigger than zero
      numericInput("shape","Shape Parameter :",value=5,min=0.01),
      numericInput("scale","Scale Parameter :",value = 6,min=0.01),
      actionButton("generate","Generate Random Numbers")
    ),
    
    mainPanel(
      
      verbatimTextOutput("summary"),
      plotOutput("histPlot"),
      plotOutput("boxPlot"),
      plotOutput("qqPlot")
    )
  )
)


server.simpareto=function(input, output) {
  
  #function for generating random numbers which u~U(0,1)
  numbers=eventReactive(input$generate,
  {
    u=runif(input$sample_size)
    x=input$shape/((1-u)^(1/input$scale))
    return(x)
  })

  output$summary=renderPrint({
    x=numbers()
    cat("Generated random numbers with inverse transform method (Pareto):\n\n")
    print(x)
    
    #Comparison of means
    m=mean(x)
    ParetoMean=input$scale*input$shape/(input$scale-1)
    cat("\n\nComparison of means :\nWe can tell that the mean of generated random numbers which is : ",m," is almost equal to the real mean which is : ",ParetoMean,"\n\n")
    
    #Explanation for plots
    cat("Expelenation for plots: \nboth histogeam and boxplot of generated random numbers are positive skew(the tail is on the rigth)\n")
    cat("QQplot is a straight line so generated random numbers are likely to be following a Pareto distributhion with shape parameter= ",input$shape," and scale parameter =",input$scale)
  
  })
  
  #Histogram of generated random numbers
  output$histPlot=renderPlot({
    
    x=numbers()
    hist(x,col =7)
  })
  
  #Box plot of generated random numbers
  output$boxPlot=renderPlot({
    
    x=numbers()
    boxplot(x, col = 5)
  })
  
  #QQplot of generated random numbers
  output$qqPlot=renderPlot({
    
    x=numbers()
    plot(qpareto(ppoints(input$sample_size),input$shape,input$scale),sort(x))
 
  })
}

shinyApp(ui=ui.simpareto,server=server.simpareto)
