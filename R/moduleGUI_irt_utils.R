library(shiny)
library(shinydashboard)

shinyApp(ui=dashboardPage(
  dashboardHeader(
    title = "IRT"  
  ),
  
  dashboardSidebar(
    selectInput("people_method", "People Parameters", list("upload", "generate", "enter")),
    conditionalPanel("input.people_method == 'upload'", 
                     fileInput("people_file", "Upload People Parameters", accept=c("text/csv", "text/plain"))),
    conditionalPanel("input.people_method == 'generate'", 
                     numericInput("people_num", "Number of People", 10, min=1),
                     fluidRow(
                       column(6, numericInput("people_mean", "Mean", 0)),
                       column(6, numericInput("people_sd", "SD", 1))
                     )),
    
    selectInput("item_method", "Item Parameters", list("upload", "generate", "enter"))
    
  ),
  
  dashboardBody(
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  )
), server=function(input, output, session){
  
})