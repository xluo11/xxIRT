#' Graphical User Interface for Automated Test Assembly
#' @description \code{gui.ata} is a shiny app for automated test assembly
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
gui.ata <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),

    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/res/img/logo.png", height=50, width=50),
      span("Automated Test Assembly", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    
    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width=4,
        # items
        wellPanel(
          fileInput("pool", "Item Pool", accept="text/plain"),
          numericInput("nform", "Number of Forms", 1, min=1),
          fluidRow(column(6, numericInput("minlength", "Min. Length", 10)),
                   column(6, numericInput("maxlength", "Max. Length.", 10)))
        ),
        
        # objectives
        wellPanel(
          h6("Set Objectives"),
          selectInput("objtype", "Objective Type", choices=list("Optimize Value"="relative", "Approach Targets"="absolute")),
          textInput("objcoef", "Coefficients", ""),
          conditionalPanel("input.objtype == 'absolute'", 
                           numericInput("objtarget", "Target Value", 0, step=0.1)),
          conditionalPanel("input.objtype == 'relative'", 
                           radioButtons("objmode", "Mode", choices=list("Maximization"="max", "Minimization"="min"), inline=FALSE),
                           checkboxInput("objnegative", "Expect a negative optimal value?")),
          actionButton("addobj", "Set Objective")
        ),
        
        # constraints
        wellPanel(
          h6("Add Constraints"),
          fluidRow(column(6, textInput("consname", "Variable", "")),
                   column(6, textInput("conslevel", "Level", ""))),
          fluidRow(column(6, numericInput("consmin", "Minimal", value=0)),
                   column(6,  numericInput("consmax", "Maximal", value=0))),
          actionButton("addcons", "Add Constraint")
        ),
        
        # assemble button
        wellPanel(
          tags$button(id="assemble", class="btn action-button btn-primary", HTML("<span class='glyphicon glyphicon-shopping-cart'></span>&nbsp;&nbsp;Assemble!"))
        )
      ), # end of sidebar panel
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   verbatimTextOutput("console0"),
                   verbatimTextOutput("console1"),
                   verbatimTextOutput("console2"),
                   verbatimTextOutput("console3")),
          tabPanel("Results", 
                   dataTableOutput("results"), 
                   downloadButton("download")),
          tabPanel("Plots",
                   plotOutput("plots"))
        ) # end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    # a reactive variable to hold data
    v <- reactiveValues(ata=NULL, objectives=NULL, constraints=NULL, items=NULL)
    
    # initiate ata object
    observeEvent({input$pool; input$nform}, {
      validate(need(input$pool, "Items haven't been imported yet"))
      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      colnames(pool) <- tolower(colnames(pool))
      nform <- input$nform
      v$ata <- ata(pool, nform, len=c(input$minlength, input$maxlength), maxselect=1)
      v$objectives <- NULL
      v$constraints <- NULL
      v$items <- NULL
    })
    
    # set objective
    observeEvent(input$addobj, {
      validate(need(v$ata, "No ata object"))
      validate(need(grepl("^[0-9a-zA-Z.-]+$", input$objcoef), "Improper objective coefficients"))
      coef <- tolower(input$objcoef)
      if(!is.na(as.numeric(coef))) coef <- as.numeric(coef)
      if(input$objtype == 'absolute'){
        v$ata <- ata.obj.absolute(v$ata, coef, input$objtarget)
        v$objectives <- rbind(v$objectives, data.frame(type="abs", coef=coef, value=input$objtarget))
      } else if (input$objtype == 'relative'){
        v$ata <- ata.obj.relative(v$ata, coef, input$objmode, input$objnegative)
        v$objectives <- rbind(v$objectives, data.frame(type=input$objmode, coef=coef, value=NA))
      }
    })
    
    # add constraint
    observeEvent(input$addcons, {
      validate(need(v$ata, "No ata object"))
      validate(need(grepl("^[0-9a-zA-Z.-]+$", input$consname), "Improper constraint variable name"))
      coef <- tolower(input$consname)
      level <- ifelse(input$conslevel == "", NULL, as.numeric(input$conslevel))
      v$ata <- ata.constraint(v$ata, coef, input$consmin, input$consmax, level=level)
      v$constraints <- rbind(v$constraints, data.frame(coef=coef, level=level, min=input$consmin, max=input$consmax))
    })
    
    # assemble
    observeEvent(input$assemble, {
      validate(need(v$ata, "No ata object"))
      v$ata <- ata.solve(v$ata)
      if(!is.null(v$ata$result)) v$items <- ata.get.items(v$ata)
    })

    # console0: ata objectives
    output$console0 <- renderPrint({
      validate(need(v$objectives, "No objectives yet"))
      v$objectives
    })
    
    # console1: ata constraints
    output$console1 <- renderPrint({
      validate(need(v$constraints, "No objectives yet"))
      v$constraints
    })

    # console2: ata lp
    output$console2 <- renderPrint({
      validate(need(v$ata, "The ATA object hasn't been initated yet"))
      invisible(v$constraints)
      invisible(v$objectives)
      v$ata
    })
    
    # console3: ata results
    output$console3 <- renderPrint({
      validate(need(v$items, "The ATA hasn't been solved yet"))
      v$items
    })
    
    # table: results
    output$results <- renderDataTable({
      validate(need(v$ata$result, "No results."))
      items <- ata.get.items(v$ata, FALSE)
      round(items, 2)
    }, options=list(pageLength=30, dom='tip'))
    
    # download: items
    output$download <- downloadHandler(
      filename=function(){
        paste(input$pool$name, "_assembled_items.txt", sep="")
      }, content=function(file){
        items <- ata.get.items(v$ata)
        write.table(items, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    # plot: TIFs
    output$plots <- renderPlot({
      validate(need(v$items, "The ATA hasn't been solved yet"))
      plot(v$ata)
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}

