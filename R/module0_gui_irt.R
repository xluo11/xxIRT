#' Graphical User Interface for IRT Utility Functions
#' @description \code{gui.irt} is a shiny app for IRT common and utility functions
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
gui.irt <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),
    
    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/res/img/logo.png", height=50, width=50),
      span("Item Response Theory", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    
    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width=4,
                # model and parameters
                wellPanel(
                  selectInput("model", "IRT Model", list("3PL")),
                  hr(),
                  selectInput("people", "People Parameters", list("Import", "Generate")),
                  conditionalPanel("input.people == 'Import'", fileInput("peoplefile", "Upload People Parameters", accept="text/plain")),
                  conditionalPanel("input.people == 'Generate'", 
                                   numericInput("npeople", "Number of People", 10, min=1),
                                   fluidRow(column(6, numericInput("people.mu", "Mean", 0)),
                                            column(6, numericInput("people.sig", "SD", 1)))),
                  hr(),
                  selectInput("items", "Item Parameters", list("Import", "Generate")),
                  conditionalPanel("input.items == 'Import'", fileInput("itemsfile", "Upload Item Parameters", accept="text/plain")),
                  conditionalPanel("input.items == 'Generate'", 
                                   numericInput("nitems", "Number of Items", 5, min=1),
                                   h6("a-parameters (log-normal)"),
                                   fluidRow(column(6, numericInput("a.mu", "Log Mean", 0)),
                                            column(6, numericInput("a.sig", "Log SD", .2))),
                                   h6("b-parameters (normal)"),
                                   fluidRow(column(6, numericInput("b.mu", "Mean", 0)),
                                            column(6, numericInput("b.sig", "SD", 1))),
                                   h6("c-parameters (beta)"),
                                   fluidRow(column(6, numericInput("c.alpha", "Alpha", 5)),
                                            column(6, numericInput("c.beta", "Beta", 42)))),
                  hr(),
                  selectInput("responses", "Responses", list("Import", "Generate")),
                  conditionalPanel("input.responses == 'Import'", fileInput("responsefile", "Upload Responses", accept="text/plain"))
                ),
                
                # compute and plot
                wellPanel(
                  selectInput("stats", "Statistics", list("Probability"="probability", "Information"="information")),
                  selectInput("summary", "Summarization", list("None"="None", "People"="people", "Items"="items")),
                  radioButtons("plot", "Plot", list("Total"=TRUE, "Items"=FALSE), inline=TRUE)
                ),
                
                # download
                wellPanel(
                  selectInput("downloadtype", "Download Data", list("People", "Items", "Responses")),
                  downloadButton("download", "Download")
                )
      ), # end of sidebar panel
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   verbatimTextOutput("console")),
          tabPanel("Statistics", 
                   dataTableOutput("stats")), 
          tabPanel("Plots",
                   plotOutput("plots"))
        ) # end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    get.people <- reactive({
      if(input$people == 'Import'){
        validate(need(input$peoplefile, "Please upload people parameters"))
        people <- read.csv(input$peoplefile$datapath, header=TRUE, as.is=TRUE)
      } else {
        people <- irt.model.3pl()$gen.data(input$npeople, 1, theta.mu=input$people.mu, theta.sig=input$people.sig)$people
      }
      return(people)
    })
    
    get.items <- reactive({
      if(input$items == 'Import'){
        validate(need(input$itemsfile, "Please upload items parameters"))
        items <- read.csv(input$itemsfile$datapath, header=TRUE, as.is=TRUE)
      } else {
        items <- irt.model.3pl()$gen.data(1, input$nitems, a.mu=input$a.mu, a.sig=input$a.sig, b.mu=input$b.mu, b.sig=input$b.sig, c.alpha=input$c.alpha, c.beta=input$c.beta)$items
      }
      return(items)
    })
    
    get.responses <- reactive({
      if(input$responses == 'Import'){
        validate(need(input$responsefile, "Please upload responses"))
        responses <- read.csv(input$responsefile$datapath, header=TRUE, as.is=TRUE)
      } else {
        validate(need(get.people(), "No people parameters"))
        validate(need(get.items(), "No item parameters"))
        responses <- irt.model.3pl()$gen.data(people=get.people(), items=get.items())$responses
      }
      return(responses)
    })
    
    get.data <- reactive({
      irt.model.3pl(get.people(), get.items(), get.responses())
    })
    
    output$console <- renderPrint({
      get.data()
    })
    
    # stats
    output$stats <- renderDataTable({
      if(input$summary == "None") {
        stats <- irt.stats(get.data(), stats=input$stats)
      } else {
        stats <- irt.stats(get.data(), stats=input$stats, input$summary, sum)
        stats <- data.frame(Statistic=stats)
      }
      round(stats, 2)
    }, options=list(pageLength=20, dom='tip'))
    
    # plot
    output$plots <- renderPlot({
      plot(get.data(), stats=input$stats, total=input$plot)
    })
    
    # download
    output$download <- downloadHandler(
      filename=function(){
        paste("gui_irt_", tolower(input$downloadtype), ".txt", sep="")
      }, content=function(file){
        write.table(get.data()[[tolower(input$downloadtype)]], file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
  })
  
  shinyApp(ui=ui, server=server)  
}
