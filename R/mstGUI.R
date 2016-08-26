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
      span("Automated Test Assembly", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width=4,
        wellPanel(
          fileInput("pool", "Item Pool", accept="text/plain"),
          fluidRow(
            column(6, textInput("design", "MST Structure", "1-2-3")),
            column(6, numericInput("npanel", "# of Panels", 1, min=1))
          )
        ),
        wellPanel(
          h6("Route Management"),
          fluidRow(
            column(6, textInput("routename", "Route", "1-2-6")),
            column(6, selectInput("routeop", "Operation", choices=list("Add"="+", "Remove"="-")))
          ),
          actionButton("btnroute", "Update")
        ),
        wellPanel(
          h6("Test Length"),
          fluidRow(
            column(6, numericInput("minlength", "Min.", 30)),
            column(6, numericInput("maxlength", "Max.", 30))
          )
        ),
        wellPanel(
          h6("Objective"),
          fluidRow(
            column(4, textInput("objtheta", "Theta", "")),
            column(4, textInput("objtarget", "Target", "Inf")),
            column(4, textInput("objroute", "Route Index", "All"))
          ),
          actionButton("btnobj", "Set")
        ),
        wellPanel(
          h6("Constraint"),
          fluidRow(
            column(6, textInput("constrvar", "Variable", "", placeholder="e.g., content")),
            column(6, textInput("constrlevel", "Level", "NA"))
          ),
          fluidRow(
            column(6, numericInput("constrmin", "Lower Bound", value=0)),
            column(6,  numericInput("constrmax", "Upper Bound", value=0))
          ),
          actionButton("btnconstr", "Set")
        ),
        wellPanel(
          h6("Stage Size"),
          fluidRow(
            column(4, numericInput("stageindex", "Stage", value=1, min=1)),
            column(4, numericInput("stagemin", "Min.", value=0)),
            column(4, numericInput("stagemax", "Max.", value=5))
          ),
          actionButton("btnstage", "Set")
        ),
        wellPanel(
          checkboxInput("parallelassemble", "Simulataneous Assembly?", TRUE),
          actionButton("assemble", "Assemble")
        )
      ), # end of sidebar panel
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", htmlOutput("console"), tableOutput("routetable")),
          tabPanel("Results", dataTableOutput("results"), downloadButton("download")),
          tabPanel("Simulation", 
                   wellPanel(numericInput("truetheta", "True Ability", 0, step=0.1)),
                   dataTableOutput("simtable"))
        )# end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    v <- reactiveValues(msg=NULL, mst=NULL)
    
    # initiate mst object
    observeEvent({input$pool; input$design; input$npanel}, {
      if(is.null(input$pool)){
        v$msg <- "Please import item pool file."
        return()
      }
      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      colnames(pool) <- tolower(colnames(pool))
      design <- as.integer(unlist(strsplit(gsub("\\s","",input$design), "-")))
      v$mst <- mst(pool, design, input$npanel)
      v$msg <- "A MST is created."
    })
    
    # route mangement
    observeEvent(input$btnroute, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      routename <- as.integer(unlist(strsplit(gsub("\\s", "", input$routename), "-")))
      tryCatch({
        v$mst <- mst.route(v$mst, routename, input$routeop)
      }, error = function(e){
       v$msg <- "Operation failed: Could add route that has already existed or remove route that does not exist."   
      })
      v$msg <- "Operation succeeded."
    })
    
    # set objective
    observeEvent(input$btnobj, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      if(input$objtheta == ""){
        v$msg <- "Please enter theta for setting objective."
        return()
      }
      theta <- as.numeric(unlist(strsplit(gsub("\\s", "", input$objtheta), ",")))

      if(input$objtarget == ""){
        v$msg <- "Please enter target for setting objective."
        return()
      }
      target <- tolower(gsub("\\s", "", input$objtarget))
      if(target == "all")
        target <- Inf
      else
        target <- as.numeric(target)
      
      if(input$objroute == ""){
        v$msg <- "Please enter route index for setting objective."
        return()
      }
      route <- tolower(gsub("\\s", "", input$objroute))
      if(route == "all") 
        route <- NULL
      else
        route <- as.integer(unlist(strsplit(route, ",")))
      
      tryCatch({
        v$mst <- mst.objective(v$mst, theta, target, route)
      }, error=function(e){
        v$msg <- "Setting objective failed."
        return()
      })
      v$msg <- "Setting objective succeeded."
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
      
      tryCatch({
        v$mst <- mst.constraint(v$mst, var, level, input$constrmin, input$constrmax)
      }, error=function(e){
        v$msg <- "Adding constriant failed."
        return()
      })
      v$msg <- "Adding constraint succeeded."
    })
    
    # stage
    observeEvent(input$btnstage, {
      if(is.null(v$mst)){
        v$msg <- "Please import the item file to create a MST."
        return()
      }
      
      tryCatch({
        v$mst <- mst.stagelength(v$mst, input$stageindex, input$stagemin, input$stagemax)
      }, error=function(e){
        v$msg <- "Setting stage size failed."
        return()
      })
      v$msg <- "Setting stage size succeeded."
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
      }, error=function(e){
        v$msg <- "Assembly failed."
        return()
      })
      v$msg <- "Assembly succeeded."
    })
    
    output$console <- renderPrint({
      tags$p(v$msg, class="lead")
    })
    
    output$routetable <- renderTable({
      v$mst$route
    })
    
    output$results <- renderDataTable({
      v$mst$items
    }, options=list(pageLength=30, dom='tip'))
    
    output$download <- downloadHandler(
      filename=function(){
        paste(input$pool$name, "_assembled_mst.txt", sep="")
      },
      content=function(file){
        write.table(v$mst$items, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    output$simtable <- renderDataTable({
      validate(need(v$mst$items, "Please assembl MST first."))
      mst.sim(v$mst, input$truetheta)
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}

