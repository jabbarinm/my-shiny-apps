library(shiny)
library(shinydashboard)

##########################################
ui.signtest <- dashboardPage(
  skin = "purple",
  title = "آزمون علامت",
  dashboardHeader(
    title = h3("آزمون علامت ارائه شده توسط يلدا غفاريان و دکتر مهدي جباري نوقابي، E-mail: jabbarinm@um.ac.ir",
               style = "font-family:B Titr; color:white; text-align: right;"),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("افزایشی یا کاهشی", tabName = "tab1", icon = icon("chart-line")),
      menuItem("بزرگ تر یا کوچکتر", tabName = "tab2", icon = icon("balance-scale"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* RTL direction for the whole app */
        * {
          font-family: 'B Titr', Tahoma, sans-serif;
        }
        
        body {
          direction: rtl;
          text-align: right;
        }
        
        # /* Main header adjustments */
        # .main-header .logo {
        #   text-align: right;
        #   direction: rtl;
        #   float: right;
        # }
        
        # .main-header .navbar {
        #   margin-right: 300px;
        #   margin-left: 0;
        # }
        
        /* Sidebar adjustments */
        .main-sidebar {
          right: 0;
          left: auto;
        }
        
        # .sidebar {
        #   direction: rtl;
        #   text-align: right;
        # }
        
        # .sidebar-menu>li>a {
        #   padding: 12px 15px 12px 5px;
        # }
        
        # .sidebar-menu li>a>.fa, 
        # .sidebar-menu li>a>.glyphicon, 
        # .sidebar-menu li>a>.ion {
        #   margin-left: 10px;
        #   margin-right: auto;
        # }
        
        # .treeview-menu {
        #   padding-right: 40px;
        #   padding-left: 0;
        # }
        
        # /* Box adjustments */
        # .box {
        #   direction: rtl;
        #   text-align: right;
        # }
        
        # .box-header .box-title {
        #   float: right;
        # }
        # 
        # .box-header>.box-tools {
        #   left: 10px;
        #   right: auto;
        #   float: left;
        # }
         
        # /* Input adjustments */
        # .form-control, .shiny-input-container {
        #   direction: rtl;
        #   text-align: right;
        # }
        
        # /* Navbar adjustments */
        # .navbar-nav {
        #   float: right;
        # }
        
        # .navbar-custom-menu .navbar-nav>li {
        #   float: right;
        # }
        
        # /* Tab adjustments */
        # .nav-tabs>li {
        #   float: right;
        # }
        
        /* Content adjustments */
        .content-wrapper {
          margin-right: 300px;
          margin-left: 0;
        }
        
        #' @media (max-width: 767px) {
        #'   .content-wrapper {
        #'     margin-right: 0;
        #'   }
        #' }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
          box(
            title = "جواب آزمون",
            width = 6,
            status = "primary",
            verbatimTextOutput("result1")
          ),
          box(
            title = "ورودی ها",
            width = 6,
            background = "navy",
            textInput("num1", "داده های مورد نظر را وارد کنید (با کاما یا فاصله):"),
            numericInput("alfa1", "الفای دلخواه خود را وارد کنید:", value = 0.05),
            actionButton("calculate1", "محاسبه", icon = icon("calculator"))
          ),
        )
      ),
      tabItem(
        tabName = "tab2",
        fluidRow(
          box(
            title = "جواب آزمون",
            width = 6,
            status = "info",
            verbatimTextOutput("result2")
          ),
          box(
            title = "ورودی ها",
            width = 6,
            background = "light-blue",
            textInput("num2", "داده های مورد نظر را وارد کنید (با کاما یا فاصله):"),
            numericInput("alfa2", "الفای دلخواه خود را وارد کنید:", value = 0.05),
            actionButton("calculate2", "محاسبه", icon = icon("calculator"))
          ),
        )
      )
    )
  )
)

server.signtest = function(input, output) {
  observeEvent(input$calculate1, {
    data = as.numeric(unlist(strsplit(input$num1, "[, ]+")))
    alfa = input$alfa1
    
    f = function(x){
      gereh = sum(x[-length(x)] == x[-1])
      m = length(x) - gereh - 1
      x1 = sum(x >= x[-1])
      zh0 = (2 * x1 - m) / sqrt(m)
      z = qnorm(1 - (alfa/2))
      
      if(abs(zh0) >= z){
        result = "فرضیه اولیه رد می‌شود، یعنی روند وجود دارد!"
      } else {
        result = "فرضیه اولیه رد نمی‌شود، یعنی روند وجود ندارد!"
      }
      return(list(result = result, zh0 = zh0, z = z))
    }
    result1 = f(data)
    
    output$result1 = renderPrint({
      cat("آماره آزمون =", result1$zh0, "\n")
      cat("*******************************************\n")
      cat("مرز ناحیه بحرانی =", result1$z, "\n")
      cat("*******************************************\n")
      cat("#################    نتیجه   ##############\n")
      cat("نتیجه =", result1$result, "\n")
    })
  })
  
  observeEvent(input$calculate2, {
    data = as.numeric(unlist(strsplit(input$num2, "[, ]+")))
    alfa = input$alfa2
    
    f = function(x){
      x2 = sum(x >= median(x))
      m = length(x)
      zh0 = ((2 * x2) - m) / sqrt(m)
      z = qnorm(1 - (alfa/2))
      
      if(abs(zh0) >= z){
        result = "فرضیه اولیه رد می‌شود، یعنی روند وجود دارد!"
      } else {
        result = "فرضیه اولیه رد نمی‌شود، یعنی روند وجود ندارد!"
      }
      return(list(result = result, zh0 = zh0, z = z))
    }
    result2 = f(data)
    
    output$result2 = renderPrint({
      cat("آماره آزمون =", result2$zh0, "\n")
      cat("*******************************************\n")
      cat("مرز ناحیه بحرانی =", result2$z, "\n")
      cat("*******************************************\n")
      cat("#################    نتیجه   ##############\n")
      cat("نتیجه =", result2$result, "\n")
    })
  })
}

shinyApp(ui.signtest, server.signtest)