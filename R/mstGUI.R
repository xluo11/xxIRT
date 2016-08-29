#' Graphical User Interface
#' @description \code{mstGUI} creates a shiny app for multistage testing
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
mstGUI <- function(){
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
      mainPanel(
        width=4,
        # pool and design
        wellPanel(
          fileInput("pool", "Item Pool", accept="text/plain"),
          fluidRow(
            column(6, textInput("design", "MST Structure", "", placeholder="1-2-3")),
            column(6, numericInput("npanel", "# of Panels", 1, min=1))
          )
        ),
        # route management
        wellPanel(
          h6("Route Management"),
          fluidRow(
            column(6, textInput("routename", "Route", "", placeholder="1-2-6")),
            column(6, selectInput("routeop", "Operation", choices=list("Add"="+", "Remove"="-")))
          ),
          actionButton("btnroute", "Update")
        ),
        # test length
        wellPanel(
          h6("Test Length"),
          fluidRow(
            column(6, numericInput("minlength", "Min.", 30)),
            column(6, numericInput("maxlength", "Max.", 30))
          )
        ),
        # objectives
        wellPanel(
          h6("Objective"),
          fluidRow(
            column(6, textInput("objtheta", "Theta", "")),
            column(6, textInput("objtarget", "Target", "Inf"))
          ),
          uiOutput("objroutesui"),
          actionButton("btnobj", "Set")
        ),
        # constraints
        wellPanel(
          h6("Constraint"),
          fluidRow(
            column(6, textInput("constrvar", "Variable", "")),
            column(6, textInput("constrlevel", "Level", ""))
          ),
          fluidRow(
            column(6, numericInput("constrmin", "Lower Bound", value=0)),
            column(6,  numericInput("constrmax", "Upper Bound", value=0))
          ),
          uiOutput("constrroutesui"),
          actionButton("btnconstr", "Set")
        ),
        # stage
        wellPanel(
          h6("Stage Size"),
          fluidRow(
            column(4, numericInput("stageindex", "Stage", value=1, min=1)),
            column(4, numericInput("stagemin", "Min.", value=0)),
            column(4, numericInput("stagemax", "Max.", value=5))
          ),
          actionButton("btnstage", "Set")
        ),
        # assembly
        wellPanel(
          checkboxInput("parallelassemble", "Simulataneous Assembly?", TRUE),
          tags$button(id="assemble", class="btn action-button btn-primary", HTML("<span class='glyphicon glyphicon-shopping-cart'></span>&nbsp;&nbsp;Assemble!"))
        )
      ), # end of sidebar panel
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   htmlOutput("message"), 
                   verbatimTextOutput("console")),
          tabPanel("Results", 
                   dataTableOutput("results"), 
                   downloadButton("download")),
          tabPanel("Plots",
                   plotOutput("plotroutes"),
                   plotOutput("plotmodules")),
          tabPanel("Simulation", 
                   wellPanel(numericInput("truetheta", "True Ability", 0, step=0.1)),
                   verbatimTextOutput("simtable"))
        )# end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    v <- reactiveValues(msg=NULL, mst=NULL)
  
    # initiate mst object
    observeEvent({input$pool; input$design; input$npanel}, {
      #item pool
      if(is.null(input$pool)) {
        v$msg <- "Please import item pool file."
        return()
      }
      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      colnames(pool) <- tolower(colnames(pool))
      # mst design
      if(!grepl("^[0-9]+(-[0-9]+)+$", input$design)) {
        v$msg <- "Please enter a MST design: e.g., 1-2-3."
        return()
      }
      design <- as.integer(unlist(strsplit(input$design, "-")))
      # create mst
      v$mst <- mst(pool, design, input$npanel)
      v$msg <- "A MST object was created."
    })
    
    # route mangement
    observeEvent(input$btnroute, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      if(!grepl("^[0-9]+(-[0-9]+)+$", input$routename)) {
        v$msg <- "Please enter a valid route: e.g., 1-2-6."
        return()
      }
      routename <- as.integer(unlist(strsplit(input$routename, "-")))
      tryCatch({
        v$mst <- mst.route(v$mst, routename, input$routeop)
        v$msg <- "Added/removed a route."
      }, error = function(e){
       v$msg <- "Failed to add/remove a route."
      })
    })
    
    getRoutes <- reactive({
      validate(need(v$mst, "No MST object."))
      r <- apply(v$mst$route, 1, paste, sep="", collapse="-")
      r.list <- 1:length(r)
      names(r.list) <- r
      r.list      
    })
    
    # render objroutes
    output$objroutesui <- renderUI({
      r <- getRoutes()
      r <- c(r, All=0)
      selectInput("objroute", "Routes", choices=r)
    })
    
    # render constrroutes
    output$constrroutesui <- renderUI({
      r <- getRoutes()
      r <- c(r, All=0)
      selectInput("constrroute", "Routes", choices=r)
    })
    
    # set objective
    observeEvent(input$btnobj, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      if(!grepl("^[0-9.-]+( *, *[0-9.-]+)*$", input$objtheta)){
        v$msg <- "Please enter theta for setting objective."
        return()
      }
      theta <- as.numeric(unlist(strsplit(gsub("\\s", "", input$objtheta), ",")))

      if(!grepl("^([0-9.-]+)|Inf$", input$objtarget)){
        v$msg <- "Please enter target for setting objective."
        return()
      }
      target <- as.numeric(input$objtarget)
      
      route <- ifelse(input$objroutes == 0, NULL, input$objroutes)
      
      tryCatch({
        v$mst <- mst.objective(v$mst, theta, target, route)
        v$msg <- "Setting objective succeeded."
      }, error=function(e){
        v$msg <- "Setting objective failed."
      })
    })
    
    # add constraint
    observeEvent(input$btnconstr, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      if(input$constrvar == ""){
        v$msg <- "Please enter a constraint variable."
        return()
      }
      var <- tolower(gsub("\\s","", input$constrvar))
      if(!var %in% colnames(v$mst$pool)){
        v$msg <- "Constraint variable is not found in the item pool."
        return()
      }
      
      if(input$constrmin > input$constrmax){
        v$msg <- "Lower-bound is greater than the upper-bound."
        return()
      }
      
      level <- as.numeric(input$constrlevel)
      route <- ifelse(input$constrroutes == 0, NULL, input$constrroutes)
      
      tryCatch({
        v$mst <- mst.constraint(v$mst, var, level, input$constrmin, input$constrmax, route)
        v$msg <- "Adding constraint succeeded."
      }, error=function(e){
        v$msg <- "Adding constriant failed."
      })
    })
    
    # stage
    observeEvent(input$btnstage, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      tryCatch({
        v$mst <- mst.stagelength(v$mst, input$stageindex, input$stagemin, input$stagemax)
        v$msg <- "Setting stage size succeeded."
      }, error=function(e){
        v$msg <- "Setting stage size failed."
      })
    })
    
    # assemble
    observeEvent(input$assemble, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      if(input$minlength > input$maxlength){
        v$msg <- "Minimal length is greater than the maximal length."
        return()
      }
      
      tryCatch({
        v$mst <- mst.constraint(v$mst, "len", NA, input$minlength, input$maxlength)
        v$mst <- mst.assemble(v$mst)
        v$msg <- "Assembly succeeded."
      }, error=function(e){
        v$msg <- "Assembly failed."
      })
    })
    
    # message
    output$message <- renderPrint({
      tags$p(v$msg, class="lead")
    })
    
    # console
    output$console <- renderPrint({
      validate(need(v$mst, "No MST object."))
      v$mst
    })
    
    # table: results
    output$results <- renderDataTable({
      validate(need(v$mst$items, "No results."))
      v$mst$items
    }, options=list(pageLength=30, dom='tip'))
    
    # download: items
    output$download <- downloadHandler(
      filename=function(){
        paste(input$pool$name, "_assembled_mst.txt", sep="")
      },
      content=function(file){
        write.table(v$mst$items, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    # plot: route tifs
    output$plotroutes <- renderPlot({
      validate(need(v$mst$items, "No results."))
      plot.mst(v$mst, by.route=TRUE)
    })

    # plot: module tifs
    output$plotmodules <- renderPlot({
      validate(need(v$mst$items, "No results."))
      plot.mst(v$mst, by.route=FALSE)
    })
    
    # table: simulation results
    output$simtable <- renderPrint({
      validate(need(v$mst$items, "Please assembl MST first."))
      mst.sim(v$mst, input$truetheta[1])
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}