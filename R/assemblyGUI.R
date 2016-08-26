#' Graphical User Interface
#' @description \code{ataGUI} creates a shiny app for automated test assembly
#' @export
#' @import shiny
#' @importFrom utils write.table read.csv
ataGUI <- function(){
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
          numericInput("nform", "Number of Forms", 1, min=1)
        ),
        wellPanel(
          h6("Test Length"),
          fluidRow(
            column(6, numericInput("minlength", "Min.", 10)),
            column(6, numericInput("maxlength", "Max.", 10))
          )
        ),
        wellPanel(
          h6("Set Objective"),
          selectInput("objtype", "Objective", choices=list("Approach Targets"="abs", "Maximization"="max", "Minimization"="min")),
          textInput("objvalue", "Variable", "", placeholder="E.g, -0.5 or b"),
          conditionalPanel("input.objtype == 'abs'", numericInput("objtarget", "Target Value", 0, step=0.1)),
          conditionalPanel("input.objtype == 'min' || input.objtype =='max'", checkboxInput("objnegative", "Expect a negative optimal value?")),
          actionButton("addobj", "Set Objective")
        ),
        wellPanel(
          h6("Add Constraint"),
          fluidRow(
            column(6, textInput("constrvar", "Variable", "")),
            column(6, textInput("constrlevel", "Level", ""))
          ),
          fluidRow(
            column(6, numericInput("constrmin", "Lower Bound", value=0)),
            column(6,  numericInput("constrmax", "Upper Bound", value=0))
          ),
          actionButton("addconstr", "Add Constraint")
        ),
        actionButton("assemble", "Assemble")
      ), # end of sidebar panel
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", htmlOutput("console")),
          tabPanel("Results", dataTableOutput("results"), downloadButton("download"))
        )# end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    v <- reactiveValues(msg=NULL, ata=NULL)
    
    # initiate ata object
    observeEvent({input$pool; input$nform}, {
      if(is.null(input$pool)){
        v$msg <- "Please import item pool file."
        return()
      }

      pool <- read.csv(input$pool$datapath, header=TRUE, as.is=TRUE)
      colnames(pool) <- tolower(colnames(pool))
      nform <- input$nform
      v$ata <- ata(pool, nform)
      v$msg <- "An ATA task is created."
    })
    
    # set objective
    observeEvent(input$addobj, {
      if(is.null(v$ata)){
        v$msg <- "Please import the item file to create an ATA task."
        return()
      }
      
      if(input$objvalue == ""){
        v$msg <- "Please enter an objective variable."
        return()
      }
      value <- tolower(gsub("\\s", "", input$objvalue))
      if(!is.na(as.numeric(value))){
        value <- as.numeric(value)
      } else if(!value %in% colnames(v$ata$pool)){
        v$msg <- "The objective variable is not found in item pool."
        return()
      }
      
      tryCatch({
        if(input$objtype == 'abs'){
          v$ata <- ata.obj.abs(v$ata, value, input$objtarget)
        } else if (input$objtype == 'max'){
          v$ata <- ata.obj.rel(v$ata, value, "max", input$objnegative)
        } else if (input$objtype == 'min'){
          v$ata <- ata.obj.rel(v$ata, value, "max", input$objnegative)
        }        
      }, error=function(e){
        v$msg <- "Setting objective failed."
        return()
      })
      v$msg <- "Setting objective succedded."
    })
    
    # add constraint
    observeEvent(input$addconstr, {
      if(is.null(v$ata)){
        v$msg <- "Please import the item file to create an ATA task."
        return()
      }
      
      if(input$constrvar == ""){
        v$msg <- "Please enter a constraint variable."
        return()
      }
      var <- tolower(gsub("\\s","", input$constrvar))
      if(!var %in% colnames(v$ata$pool)){
        v$msg <- "Constraint variable is not found in the item pool."
        return()
      }
      
      if(input$constrmin > input$constrmax){
        v$msg <- "Lower-bound is greater than the upper-bound."
        return()
      }
      
      level <- as.numeric(input$constrlevel)
      
      tryCatch({
        v$ata <- ata.constraint(v$ata, var, level, input$constrmin, input$constrmax)
      }, error=function(e){
        v$msg <- "Adding constriant failed."
        return()
      })
      v$msg <- "Adding constraint succeeded."
    })
    
    # assemble
    observeEvent(input$assemble, {
      if(is.null(v$ata)){
        v$msg <- "Please import the item file to create an ATA task."
        return()
      }
      
      if(input$minlength > input$maxlength){
        v$msg <- "Minimal length is greater than the maximal length."
        return()
      }
      
      tryCatch({
        v$ata <- ata.constraint(v$ata, "len", NA, input$minlength, input$maxlength)
        v$ata <- ata.maxselect(v$ata, 1)
        v$ata <- ata.solve(v$ata)
        v$ata <- ata.collapse.rs(v$ata)
      }, error=function(e){
        v$msg <- "Assembly failed."
      })
      v$msg <- ifelse(is.null(v$ata$rs.items), "Assembly failed.", "Assembly succeeded.")
    })

    output$console <- renderPrint({
      paste("<p class='lead'>", v$msg, "</p>")
    })
    
    output$results <- renderDataTable({
      v$ata$rs.items
    }, options=list(pageLength=30, dom='tip'))
    
    output$download <- downloadHandler(
      filename=function(){
        paste(input$pool$name, "_assembled_items.txt", sep="")
      },
      content=function(file){
        write.table(v$ata$rs.items, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
  })
  
  shinyApp(ui=ui, server=server)  
}