#' Graphical User Interface
#' @description \code{catGUI} creates a shiny app for \code{cat.sim}
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
catGUI <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),
    
    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/resources/img/spaceship-png-icon-9.png", height=50, width=50),
      span("CAT Simulation", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    
    # layout               
    sidebarLayout(
      # sidebar panel
      sidebarPanel(
        fileInput("pool", "Item Pool", accept="text/plain"),
        hr(),
        numericInput('theta', 'True Ability', 0, min=-3, max=3, step=0.1),
        fluidRow(
          column(6, numericInput('min', 'Min Length', 10)),
          column(6, numericInput('max', 'Max Length', 30))
        ),
        hr(),      
        selectInput('select', 'Selection Rule', choices=list("Default"="cat.select.default", "C-CAT"="cat.select.ccat")),
        numericInput("randomesque", "Item Exposure Control", 1, min=1),
        conditionalPanel("input.select == 'cat.select.ccat'", 
                         textInput('ccat.target', 'Content Distribution', "", placeholder="e.g., 0.5, 0.3, 0.2"),
                         numericInput('ccat.random', 'Random', 5, min=0)),
        hr(),
        selectInput('estimate', 'Estimation Rule', choices=list("Default"="cat.estimate.default")),
        hr(),
        selectInput('stop', 'Termination Rule', choices=list("Default"="cat.stop.default")),
        conditionalPanel("input.stop == 'cat.stop.default'",
                         selectInput('stop.type', 'Criterion', choices=list('Standard Error'='stop.se', 'Minimum Information'='stop.mi', 'Cut Score'='stop.cut')),
                         numericInput('stop.value', 'Value', 0.3))
      ), # end of sidebar panel
      # main panel
      mainPanel(
        tabsetPanel(
          # tabPanel("Console", verbatimTextOutput("console")),
          tabPanel("Table", dataTableOutput("table")),
          tabPanel("Plot", plotOutput("plot"))
        ) # end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    cat <- reactive({
      validate(
        need(input$pool, "Please import item pool file."),
        need(input$min > 0 && input$min <= input$max, "Please check the minimum and maximum lengths.")
      )
      
      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      theta <- input$theta
      opts <- list(min=input$min, max=input$max, select.random=input$randomesque)
      
      if(input$stop == "cat.stop.default") opts[[input$stop.type]] <- input$stop.value
      
      if(input$select == "cat.select.ccat") {
        validate(
          need(input$ccat.target != "", "Please set the desired content percentages"),
          need(grepl("^([.,0-9 ]*)*$", input$ccat.target) && as.numeric(unlist(strsplit(input$ccat.target, ","))), 
               "Please use correct format for content percentage: e.g., .50, .30, .20")
        )
        opts$ccat.target <- as.numeric(unlist(strsplit(input$ccat.target, ",")))
        opts$ccat.random <- input$ccat.random
      }
      
      cat.sim(theta, pool, opts, cat.select=match.fun(input$select), cat.estimate=match.fun(input$estimate), cat.stop=match.fun(input$stop))
    })
    
    output$console <- renderPrint({
      cat()
    })
    
    output$table <- renderDataTable({
      x <- as.data.frame(cat()$admin, stringsAsFactors=FALSE)
      x <- round(x, 2)
      colnames(x)[1:3] <- c("Response", "Theta", "SE")
      x <- cbind(Position=1:nrow(x), x)
      x
    }, options=list(pageLength=30, dom='tip'))
    
    output$plot <- renderPlot({
      plot.cat(cat())
    })
  })
  
  shinyApp(ui=ui, server=server)  
}
