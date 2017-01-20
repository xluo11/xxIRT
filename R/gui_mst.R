#' Graphical User Interface for Multistage Testing
#' @description \code{gui.mst} is a shiny app for multistage testing
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
gui.mst <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),

    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/res/img/logo.png", height=50, width=50),
      span("Multistage Testing", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    
    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width=4,
        # pool and design
        wellPanel(
          fileInput("pool", "Item Pool", accept="text/plain"),
          fluidRow(column(6, textInput("mstDesign", "MST Design", "1-2-2")),
                   column(6, numericInput("numPanel", "Number of Panels", 1, min=1))),
          selectInput("mstMethod", "Design Method", choices=list("Top-down"='topdown', "Bottom-up"='bottomup'))
        ),
        
        # route management
        wellPanel(
          h6("Routes Management"),
          fluidRow(column(6, textInput("routeName", "Route Vector", "")),
                   column(6, selectInput("routeOp", "Operation", list("Add"="+", "Remove"="-")))),
          actionButton("routeAction", "Add/Remove Routes")
        ),
        
        wellPanel(
          selectInput("type", "Constraint Type", list("Objectives"="obj", "Constraints"="cons", "Test length"="testlen", "Stage Length"="stagelen")),
          conditionalPanel("input.type == 'obj'",
                           fluidRow(column(6, numericInput("objTheta", "Theta", "")),
                                    column(6, numericInput("objTarget", "Target", "")))),
          conditionalPanel("input.type == 'cons'",
                           fluidRow(column(6, textInput("consName", "Variable", "")),
                                    column(6, textInput("consLevel", "Level", ""))),
                           fluidRow(column(6, numericInput("consMin", "Lower Bound", "")),
                                    column(6, numericInput("consMax", "Upper Bound", "")))),
          conditionalPanel("input.type == 'testlen'",
                           fluidRow(column(6, numericInput("testMin", "Minimal Length", 20, min=1)),
                                    column(6, numericInput("testMax", "Maximal Length", 20, min=1)))),
          conditionalPanel("input.type == 'stagelen'",
                           numericInput("stageIndex", "Stage", ""),
                           fluidRow(column(6, numericInput("stageMin", "Minimal Length", "")),
                                    column(6, numericInput("stageMax", "Maximal Length", "")))),
          conditionalPanel("input.type != 'stagelen'",
                           conditionalPanel("input.mstMethod == 'topdown'",  uiOutput("routesUI")),
                           conditionalPanel("input.mstMethod == 'bottomup'", uiOutput("modulesUI"))),
          actionButton("constraintAction", "Add Constraints/Objectives")
        ),
        
        # assembly
        wellPanel(
          # checkboxInput("parallelassemble", "Simulataneous Assembly?", TRUE),
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
                   verbatimTextOutput("console2")),
          tabPanel("Items", 
                   dataTableOutput("items"), 
                   downloadButton("download")),
          tabPanel("TIFs",
                   plotOutput("routeTIFs"),
                   plotOutput("moduleTIFs")),
          tabPanel("Simulation", 
                   wellPanel(numericInput("trueTheta", "True Ability", 0, step=0.1)),
                   verbatimTextOutput("simTable"))
        ) # end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    # a reactive variable to hold data
    v <- reactiveValues(mst=NULL, objectives=NULL, constraints=NULL)
  
    # render routes ui
    output$routesUI <- renderUI({
      validate(need(v$mst, "No routes"))
      x <- v$mst
      routes <- x$route
      route <- routes[, 1:x$nstage]
      route <- apply(route, 1, paste, collapse="-")
      index <- routes[, x$nstage + 1]
      x <- as.list(index)
      names(x) <- route
      checkboxGroupInput("routeIndex", "Select Routes", choices=x)
    })
    
    # render module ui
    output$modulesUI <- renderUI({
      validate(need(v$mst, "No modules"))
      x <- v$mst
      modules <- x$module
      module <- paste("stage", modules$stage, "module", modules$module)
      index <- modules$index
      x <- as.list(index)
      names(x) <- module
      checkboxGroupInput("moduleIndex", "Select Modules", choices=x)
    })
    
    # initiate mst object
    observeEvent({input$pool; input$mstDesign; input$numPanel; input$mstMethod}, {
      # pool
      validate(need(input$pool, "No item pool"))
      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      colnames(pool) <- tolower(colnames(pool))
      # design
      validate(need(grepl("^[0-9|-]*$", input$mstDesign), "Invalid MST design"))
      design <- as.integer(unlist(strsplit(input$mstDesign, "-")))
      # create a MST
      v$mst <- mst(pool, design, input$numPanel, input$mstMethod)
      v$objectives <- NULL
      v$constraints <- NULL
    })
    
    # console0: mst object and route & module map
    output$console0 <- renderPrint({
      validate(need(v$mst, "The MST object hasn't been created yet"))
      list(Routes=v$mst$route, Modules=v$mst$module, ATA=v$mst$assembler)
    })
    
    # console1: objectives & constraints
    output$console1 <- renderPrint({
      validate(need(!is.null(v$objectives) || !is.null(v$constraints), "No objectives or constraints"))
      list(Objectives=v$objectives, Constraints=v$constraints)
    })

    # console2: assembled items
    output$console2 <- renderPrint({
      validate(need(v$mst$items, "The MST object hasn't been assembled yet"))
      v$mst$items
    })
    
    # route action
    observeEvent(input$routeAction, {
      validate(need(v$mst, ""))
      validate(need(grepl("^[0-9|-]*$", input$routeName), "Invalid route"))
      route <- as.integer(unlist(strsplit(input$routeName, "-")))
      v$mst <- mst.route(v$mst, route, input$routeOp)
    })
    
    # constraint action
    observeEvent(input$constraintAction, {
      validate(need(v$mst, ""))
      # route/module indices
      if(input$mstMethod == "topdown") {
        indices <- input$routeIndex
      } else if(input$mstMethod == "bottomup") {
        indices <- input$moduleIndex
      }
      # objectives/constraints
      if(input$type == "obj") {
        validate(need(indices, "No route/module indices"))
        v$mst <- mst.objective(v$mst, input$objTheta, target=input$objTarget, indices=indices)
        v$objectives <- rbind(v$objectives, data.frame(theta=input$objTheta, target=input$objTarget, index=indices))
      } else if(input$type == "cons") {
        validate(need(indices, "No route/module indices"))
        v$mst <- mst.constraint(v$mst, input$consName, input$consMin, input$consMax, level=input$consLevel, indices=indices)
        v$constraints <- rbind(v$constraints, data.frame(name=input$consName, min=input$consMin, max=input$consMax, level=ifelse(is.null(input$consLevel), NA, input$consLevel), index=indices))
      } else if(input$type == "testlen") {
        validate(need(indices, "No route/module indices"))
        v$mst <- mst.constraint(v$mst, 1, input$testMin, input$testMax, indices=indices)
        v$constraints <- rbind(v$constraints, data.frame(name="test length", min=input$testMin, max=input$testMax, level=NA, index=indices))
      } else if(input$type == "stagelen") {
        v$mst <- mst.stage.length(v$mst, input$stageIndex, input$stageMin, input$stageMax)
        v$constraints <- rbind(v$constraints, data.frame(name=paste("stage", input$stageIndex), min=input$stageMin, max=input$stageMax, level=NA, index=NA))
      }
    })

    # assemble action
    observeEvent(input$assemble, {
      validate(need(v$mst, "The MST object hasn't been created yet"))
      validate(need(v$objectives, "No objectives"))
      validate(need(v$constraints, "No constraints"))
      v$mst <- mst.assemble(v$mst)
    })

    # Output: assembled items
    output$items <- renderDataTable({
      validate(need(v$mst$items, "No assembly results"))
      round(v$mst$items, 3)
    }, options=list(pageLength=30, dom='tip'))

    # Download: items
    output$download <- downloadHandler(
      filename=function(){
        paste(input$pool$name, "_assembled_mst.txt", sep="")
      }, content=function(file){
        write.table(v$mst$items, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )

    # plot: route tifs
    output$routeTIFs <- renderPlot({
      validate(need(v$mst$items, "No assembly results."))
      plot(v$mst, byroute=TRUE)
    })

    # plot: module tifs
    output$moduleTIFs <- renderPlot({
      validate(need(v$mst$items, "No assembly results."))
      plot(v$mst, byroute=FALSE)
    })

    # MST Simulation
    output$simTable <- renderPrint({
      validate(need(v$mst$items, "Please assembl MST first."))
      mst.sim(v$mst, input$trueTheta[1])
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}

gui.mst()

