#' Graphical User Interface for Parameter Estimation
#' @description \code{gui.estimation} is a shiny app for parameter estimation
#' @import shiny
#' @importFrom utils write.table read.csv
#' @export
gui.estimation <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),
    
    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/res/img/logo.png", height=50, width=50),
      span("Parameter Estimation", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),

    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width = 4,
        # response
        wellPanel(
          fileInput("response", "Upload Responses", accept="text/plain"),
          selectInput("model", "IRT Model", choices=list("3PL", "2PL", "1PL", "Rasch"))
        ),
        # estimation options
        wellPanel(
          radioButtons("people", "People Parameters", choices=list("Import", "Estimate")),
          radioButtons("items", "Item Parameters", choices=list("Import", "Estimate"))
        ),
        # estimate thetas
        wellPanel(
          conditionalPanel("input.people == 'Import'", fileInput('peoplefile', "Upload People Parameters", accept="text/plain")),
          conditionalPanel("input.people == 'Estimate'", selectInput('peoplemethod', 'People Estimation Method', choices=list("MLE", "EAP", "MAP")))
        ),
        # estimate items
        wellPanel(
          conditionalPanel("input.items == 'Import'", fileInput('itemsfile', "Upload Item Parameters", accept="text/plain")),
          conditionalPanel("input.items == 'Estimate'", selectInput('itemsmethod', 'Items Estimation Method', choices=list("JMLE", "MMLE", "BME"))),
          conditionalPanel(
            "input.itemsmethod == 'BME'",
            h6("a-parameters (log-normal):"),
            fluidRow(column(6, numericInput("a.mu", "Mean", 0.0, step=0.1)), column(6, numericInput("a.sig", "SD", 0.2, step=0.1))),
            h6("b-parameters (normal)"),
            fluidRow(column(6, numericInput("b.mu", "Mean", 0.0, step=0.1)), column(6, numericInput("b.sig", "SD", 1.0, step=0.1))),
            h6("c-parameters (beta)"),
            fluidRow(column(6, numericInput("c.alpha", "Alpha", 5)), column(6, numericInput("c.beta", "Beta", 42)))
          )
        )
      ), # end of sidebar panel
      
      # main panel
      mainPanel(
        tabsetPanel(
          # console: final irt.model object
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   verbatimTextOutput("console")),
          # item results
          tabPanel("Items",
                   dataTableOutput("items"), 
                   downloadButton("downloaditems", "Download")),
          tabPanel("People",
                   dataTableOutput("people"), 
                   downloadButton("downloadpeople", "Donwload"))
        )# end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    # read responses from file
    get.responses <- reactive({
      validate(need(input$response, "Please import response file."))
      read.csv(input$response$datapath, header=TRUE, as.is=TRUE)
    })
    
    # import items from file or estimate item parmaeters
    get.items <- reactive({
      if(input$items == "Import"){
        validate(need(input$itemsfile, "Please import item parameters."))
        items <- read.csv(input$itemsfile$datapath, header=TRUE, as.is=TRUE)
      } else if(input$items == 'Estimate'){
        u <- get.responses()
        fixed.par <- list()
        if(input$model == "2PL"){
          fixed.par$c <- 0
        } else if(input$model == "1PL") {
          fixed.par$c <- 0
          fixed.par$a <- 1
        } else if(input$model == "Rasch") {
          fixed.par$c <- 0
          fixed.par$a <- .5882
        }
        if(input$itemsmethod == "JMLE"){
          validate(need(input$peoplefile, "Pleaes import people parameters for JMLE calibration."))
          items <- estimate.items.3pl.jmle(u, get.people(), fix=fixed.par)$items
        } else if(input$itemsmethod == 'MMLE'){
          items <- estimate.items.3pl.mmle(u, fix=fixed.par)$items
        } else if(input$itemsmethod == 'BME') {
          priors <- list(a.mu=input$a.mu, a.sig=input$a.sig, b.mu=input$b.mu, b.sig=input$b.sig, c.alpha=input$c.alpha, c.beta=input$c.beta)
          items <- estimate.items.3pl.bme(u, fix=fixed.par, prior=priors)$items
        }
      }
      return(items)
    })
    
    # import people parameters from file or estimate people parameters
    get.people <- reactive({
      if(input$people == 'Import'){
        validate(need(input$peoplefile, "Please import people parameters."))
        people <- read.csv(input$peoplefile$datapath, header=TRUE, as.is=TRUE)
      } else if(input$people == 'Estimate'){
        u <- get.responses()
        items <- get.items()
        if(input$peoplemethod == "MLE"){
          people <- estimate.people.3pl.mle(u, items)$people
        }else if(input$peoplemethod == "MAP"){
          people <- estimate.people.3pl.map(u, items)$people
        }else if(input$peoplemethod == "EAP"){
          people <- estimate.people.3pl.eap(u, items)$people
        }
      }
      return(people)
    })

    # output: console
    output$console <- renderPrint({
      rsp <- get.responses()
      irt.model(get.people(), get.items(), rsp)
    })
    
    # output: items
    results.items <- reactive({
      # items parameters
      items <- get.items()
      # additional stats: n, score, p, se, drift
      u <- get.responses()
      items$n <- colSums(!is.na(u))
      items$score <- colSums(u)
      items$p <- colMeans(u)
      people <- get.people()
      info <- irt.stats(irt.model.3pl(people, items), "information", summary="items", fun=sum)
      items$se <- 1 /sqrt(info)
      items <- round(items, 3)
      # return data
      items      
    })
    
    output$items <- renderDataTable({
      items <- results.items()
      cbind(id=1:nrow(items), items)
    }, options=list(pageLength=20, dom='tip'))
    
    # download: items
    output$downloaditems <- downloadHandler(
      filename=function(){
        paste("gui_estimation_items.txt", sep="")
      }, content=function(file) {
        write.table(results.items(), file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
  
    # table: people
    results.people <- reactive({
      # people parameters
      people <- get.people()
      # additional statistics:
      u <- get.responses()
      people$n <- rowSums(!is.na(u))
      people$score <- rowSums(u)
      people$p <- rowMeans(u)
      items <- get.items()
      info <- irt.stats(irt.model.3pl(people, items), "information", summary="people", fun=sum)
      people$se <- 1 /sqrt(info)
      people <- round(people, 3)
      # result
      people
    })
    
    output$people <- renderDataTable({
      people <- results.people()
      cbind(id=1:nrow(people), people)
    }, options=list(pageLength=20, dom='tip'))
    
    # download: people
    output$downloadpeople <- downloadHandler(
      filename=function(){
        paste("gui_estimation_thetas.txt", sep="")
      }, content=function(file){
        write.table(results.people(), file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
  })
  
  shinyApp(ui=ui, server=server)  
}
