library(reticulate)


source_python('C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first.py')
source_python('C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first1.py')

py_run_file("C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first.py")

py_run_file("C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first1.py")
names(py)
py$get_initial_input=265
py$initial_number=568
py$user_input2
py$iterations=50
py$n=12
py$square_method_random(125)

py_run_string("
def square_method_random1(n, iterations):
    k = len(str(n))  # Number of digits in the input number
    
    current_number = n
    
    for _ in range(iterations):
        # Calculate the square of the current number
        squared = str(current_number ** 2).zfill(2 * k)  # Padding with zeros
        
        # Remove digits based on the parity of k
        if k % 2 == 0:
            mid = k // 2
            new_number = squared[mid:-mid]
        else:
            mid_left = (k - 1) // 2
            mid_right = (k + 1) // 2
            new_number = squared[mid_left:-mid_right]
        
        # Print the new number
        print(int(new_number))
        print(int(new_number)/10**k)
        current_number = int(new_number)  # Update current number
")
py_run_string("square_method_random1(123,15)")


##################################################
py_install("shiny")
py_run_string("import shiny")
py_module_available("shiny")

py_install("pandas")
py_run_string("import pandas")
py_module_available("pandas")

py_run_file("C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\application.py")
py$app
source_python('C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\application.py')


py_run_string("kk=2",convert=TRUE)
py$kk


np <- import("numpy", convert = FALSE)
# do some array manipulations with NumPy
a <- np$array(c(1:4))
sum <- a$cumsum()

# convert to R explicitly at the end
py_to_r(sum)



library(shiny);

##################################################

ui.simmian<-navbarPage("Simulate Sample from Uniform(0,1) by E. SharifBagheri and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)",
               tabPanel("Simulation",
                        sidebarLayout(sidebarPanel(
#                          checkboxInput(inputId='header', label = 'CI for Proportion', value = FALSE),
#                          checkboxInput(inputId='header', label = 'CI for Mean', value = FALSE),
                          uiOutput("method_select"),
                          numericInput("samplesize","The Sample Size=",10),
                          numericInput("core","The Core=",567)
                       ),
			mainPanel( helpText("Your Selected variables"),
                            verbatimTextOutput("results"))))
)

server.simmian<-function(input,output) { 
output$method_select<-renderUI({
  selectInput("methodselect","Select the Method of Simulation",choices = c("Mid-square"="mid","Product"="product","Congruence"="cong"))
})

output$results<-renderPrint({

print("#####################################################################################")
print("###################  The Parameters for Confidence Interval  ########################")
print("#####################################################################################")
if (input$methodselect=="mid"){
print("#####################            The Sample Size          ###########################")
print(c("The Sample Size=",input$samplesize))
print("#####################         The Entrance Core          ###########################")
print(c("The Core=",input$core))
print("#####################################################################################")
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("#####################     Simulation Based on Mid-Square    #########################")

py$core=input$core
py$iterations=input$samplesize
py$x=NA
py_run_string("
k = len(str(core))  # Number of digits in the input number
current_number = core
for _ in range(int(iterations)):
    # Calculate the square of the current number
    squared = str(current_number ** 2).zfill(2 * k)  # Padding with zeros
    # Remove digits based on the parity of k
    if k % 2 == 0:
        mid = k // 2
        new_number = squared[mid:-mid]
    else:
        mid_left = (k - 1) // 2
        mid_right = (k + 1) // 2
        new_number = squared[mid_left:-mid_right]
        # Print the new number
    print(int(new_number))
    print(int(new_number)/10**k)
	current_number = int(new_number)  # Update current number
	x=[x,current_number]
              ")

#print(py$new_number)
#print(py$current_number)
print(py$x)
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("#####################################################################################")
					}

print("#####################################################################################")
print("#####################################################################################")
})

}
shinyApp(ui.simmian,server.simmian)


###########################################

ui.simmian1<-navbarPage("Simulate Sample from Uniform(0,1) by E. SharifBagheri and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)",
               tabPanel("Simulation",
                        sidebarLayout(sidebarPanel(
#                          checkboxInput(inputId='header', label = 'CI for Proportion', value = FALSE),
#                          checkboxInput(inputId='header', label = 'CI for Mean', value = FALSE),
                          uiOutput("method_select"),
                          numericInput("core","The Core=",567)
                       ),
			mainPanel( helpText("Your Selected variables"),
                            verbatimTextOutput("results"))))
)

server.simmian1<-function(input,output) { 
output$method_select<-renderUI({
  selectInput("methodselect","Select the Method of Simulation",choices = c("Mid-square"="mid","Product"="product","Congruence"="cong"))
})

output$results<-renderPrint({

print("#####################################################################################")
print("###################  The Parameters for Confidence Interval  ########################")
print("#####################################################################################")
if (input$methodselect=="mid"){
print("#####################         The Entrance Core          ###########################")
print(c("The Core=",input$core))
print("#####################################################################################")
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("#####################     Simulation Based on Mid-Square    #########################")
(source_python('C:\\Users\\m.jabari\\Documents\\Cloud\\m.jabari\\BI\\PythonExamples\\first1.py'))

print(py$square_method_random(input$core))

print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("#####################################################################################")
					}

print("#####################################################################################")
print("#####################################################################################")
})

}
shinyApp(ui.simmian1,server.simmian1)

