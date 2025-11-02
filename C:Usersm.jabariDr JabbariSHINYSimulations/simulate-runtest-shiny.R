library(reticulate)

rm(list = ls())
############################################
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

py_run_string("
from shiny import App, ui, render, reactive
import pandas as pd")
ui.runstest<-py_run_string("
app_ui = ui.page_fluid(
    ui.h1('Trend number test'),
    ui.input_file('file','Enter your CSV file.',accept=['.csv']),
    ui.input_select('alpha', 'Specify the alpha value',choices=[0.001, 0.01, 0.05], selected=0.05),  
    ui.output_text_verbatim('runtest')
						)
            ")
server.runstest<-function(input,output,session){
py$input=input;py$output=output;py$session=session
py_run_string("
def server(input,output,session):
#    @output
#    @render.text
    def runtest():
        file_info=input.file()
        alpha =float(input.alpha())
        if not file_info:
            return 'Please upload a CSV file to continue.'
        dfile = pd.read_csv(file_info[0]['datapath'])
        count=0
        for i in range(1,len(dfile)-1):
            if dfile.iloc[i+1,0] >= dfile.iloc[i,0]:
                count+=1
        m=len(dfile)
        mu= (2*m-1) / 3
        sigma=(16*m-13)/90
        zi=(count-mu)/sigma**.5 
        critical_values = {
        0.05: 1.96,
        0.01: 2.58,
        0.001: 3.30}
        if alpha in critical_values:
            critical_values=critical_values.get(alpha)
        if abs(zi) >= critical_values:
            return f'H0 is rejected because: {abs(zi):.2f} >= {critical_values}.'
        else:
            return f'H0 is accepted because: {abs(zi):.2f} < {critical_values}.'
     
				")
print(py$zi)			
								}

names(py)
py$App
shinyApp(py$app_ui,py$server)

shinyApp(ui.runstest,server.runstest)

##################################################

library(shiny);

##################################################

ui.runstest<-navbarPage("Runs test by S. Gholinia and Dr. M. Jabbari Nooghabi (E-Mail: jabbarinm@um.ac.ir)",
                    tabPanel("Data Import",
                             sidebarLayout(sidebarPanel( fileInput("file2","Upload your CSV",multiple=FALSE),
                                                         tags$hr(),
                                                         h5(helpText("Select the read.table parameters below")),
                                                         checkboxInput(inputId='header2',label='Header',value=FALSE),
                                                         checkboxInput(inputId="stringAsFactors2","stringAsFactors",FALSE),
                                                         radioButtons(inputId='sep2',label='Separator', 
                                                                      choices = c(Comma=',',Semicolon=';',Tab='\t',Space=''), selected=',')
                             ),
                             mainPanel(uiOutput("tb2"))
                             )),
                    tabPanel("Test",
                             sidebarLayout(sidebarPanel(
                               numericInput("significantlevel","The Significant Level of the Test:",0.05),
                               uiOutput("var_select"),
                             ),
                             mainPanel( helpText("Test"),
                                        verbatimTextOutput("testoutput"),
#                                        plotOutput("plot1"),
                                       ))
                    )
)

server.runstest<-function(input,output) { 
  data.test<-reactive({
    file2 <- input$file2
    if(is.null(file2)){return()} 
    read.csv(file=file2$datapath,sep=input$sep2,header=input$header2,stringsAsFactors=input$stringAsFactors2)
  })  
  output$table2 <- renderTable({
    if(is.null(data.test())){return ()}
    data.test()
  })
  output$tb2 <- renderUI({
    tableOutput("table2")
  })
  output$var_select<-renderUI({
    selectInput("varselect","Select the Variabale",choices=as.list(names(data.test())),multiple=F)
  })

output$testoutput<-renderPrint({
data1=data.test()
print("#####################################################################################")
print("############################            Head of DataFrame         ###################")
print(head(data1))
print("#####################################################################################")
print("#####################    The Significant Level (alpha)    ###########################")
print(c("Significant Level=",input$significantlevel))
print("#####################################################################################")
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
py_run_string("
from shiny import App, ui, render, reactive
import pandas as pd")
#print(data1[,input$varselect])
py$data1=data1[,input$varselect]
py$alpha=input$significantlevel
print("##################      Data of the Selected Varibale         #######################")
print(py$data1)
print("#####################################################################################")
print("###########################     Runs Tests    #######################################")
py_run_string("
dfile = data1
count=0
for i in range(1,len(dfile)-1):
    if dfile[i+1] >= dfile[i]:
        count+=1

m=len(dfile)
mu= (2*m-1) / 3
sigma=(16*m-13)/90
zi=(count-mu)/sigma**.5 
critical_values = {
0.05: 1.96,
0.01: 2.58,
0.001: 3.30}
if alpha in critical_values:
    critical_values=critical_values.get(alpha)
if abs(zi) >= critical_values:
    print('H0 is rejected because: {abs(zi):.2f} >= {critical_values}.')
else:
    print('H0 is NOT rejected because: {abs(zi):.2f} < {critical_values}.')
    				")
print("#####################################################################################")
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("###########################      Test Statistic         #############################")
print(py$zi)
print("#####################################################################################")
print("###########################      Critical Values        #############################")
print(py$critical_values)
print("#####################################################################################")
print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
print("#####################################################################################")
print("#####################################################################################")
})

}
shinyApp(ui.runstest,server.runstest)



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

