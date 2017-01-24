#' Graphical User Interface for IRT Estimation
#' @description Launch the graphical user interface for IRT estimation
#' @import shiny shinydashbaord ggplot2
#' @importFrom utils write.csv read.csv
#' @importFrom stats sd
#' @importFrom DT dataTableOutput renderDataTable
#' @export
gui_estimation <- function(){
  shinyApp(ui=dashboardPage(
    dashboardHeader(
      title = "IRT Estimation"
    ),
    
    dashboardSidebar(
      fileInput("response_file", "Upload Response Data File", accept=c("text/plain", "text/csv")),
      hr(),
      selectInput("people_method", "People Parameter", list("Import"="import", "Estimate"="estimate")),
      conditionalPanel("input.people_method == 'import'",
                       fileInput("people_file", "Upload People Parameter File", accept=c("text/plain", "text/csv"))),
      conditionalPanel("input.people_method == 'estimate'",
                       selectInput("people_estimate_method", "Choose a Method: ", list("MLE", "MAP", "EAP"))),
      hr(),
      selectInput("item_method", "Item Parameter", list("Import"="import", "Estimate"="estimate")),
      conditionalPanel("input.item_method == 'import'",
                       fileInput("item_file", "Upload Item Parameter File", accept=c("text/plain", "text/csv"))),
      conditionalPanel("input.item_method == 'estimate'",
                       selectInput("item_estimate_method", "Choose a Method: ", list("JMLE", "MMLE", "BME")),
                       selectInput("item_estimate_model", "Choose a Model: ", list("3PL", "2PL", "1PL", "Rasch"))),
      
      div(class="shiny-input-container", HTML('<iframe width="200" height="150" src="https://www.youtube.com/embed/pcKR0LPwoYs" frameborder="0" allowfullscreen></iframe>'))
    ),
    
    dashboardBody(
      # alert when shiny is busy
      fluidRow(
        conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                         tags$div(class="alert alert-warning",
                           tags$b("Estimation in progress ... "),
                           HTML("<i class='fa fa-spinner fa-pulse fa-1x fa-fw'></i>")
                         ))
      ),
      
      fluidRow(
        infoBoxOutput("response_infobox"),
        infoBoxOutput("people_infobox"),
        infoBoxOutput("item_infobox")
      ),
      
      fluidRow(
        box(title = "People Parameters", status = "success",
            htmlOutput("people_text"),
            plotOutput("people_plot", height=400),
            downloadButton("people_download", "Save")),
        
        box(title = "Item Parameters", status = "primary",
            htmlOutput("item_text"),
            DT::dataTableOutput("item_table"),
            downloadButton("item_download", "Save"))        
      )
      
    )
    
  ), server=function(input, output, session){
    # Import responses data
    data_responses <- reactive({
      req(input$response_file$datapath)
      validate(need(input$response_file$size <= 500000, "file is too large"))
      x <- read.csv(input$response_file$datapath, header=TRUE, as.is=TRUE)
      as.matrix(x)
    })
    
    # Import/estimate people parameters
    data_people_import <- reactive({
      req(input$people_file$datapath)
      validate(need(input$people_file$size <= 500000, "file is too large"))
      x <- read.csv(input$people_file$datapath, header=TRUE, as.is=TRUE)
      colnames(x) <- tolower(colnames(x))
      data.frame(theta=x$theta)
    })
    data_people <- reactive({
      if(input$people_method == "import") {
        x <- data_people_import()
      } else if(input$people_method == "estimate") {
        req(data_responses(), data_items())
        x <- estimate_people(data_responses(), data_items(), "3pl", input$people_estimate_method)
        x <- x$people
      }
      x
    })
    
    # Import/estimate people parameters
    data_items_import <- reactive({
      req(input$item_file$datapath)
      validate(need(input$item_file$size <= 500000, "file is too large"))
      x <- read.csv(input$item_file$datapath, header=TRUE, as.is=TRUE)
      colnames(x) <- tolower(colnames(x))
      data.frame(a=x$a, b=x$b, c=x$c)
    })
    
    data_items <- reactive({
      if(input$item_method == "import") {
        x <- data_items_import()
      } else if(input$item_method == "estimate") {
        req(data_responses())
        fix <- switch(input$item_estimate_model,
                      "3PL" = list(),
                      "2PL" = list(c=0),
                      "1PL" = list(a=1, c=0),
                      "Rasch" = list(a=.5882, c=0))
        if(input$item_estimate_method == "JMLE") {
          if(input$people_method == "estimate") {
            x <- estimate_items(data_responses(), "3pl", "mmle", fix=fix)
          } else {
            x <- estimate_items(data_responses(), "3pl", input$item_estimate_method, people=data_people(), fix=fix)
          }
        } else {
          x <- estimate_items(data_responses(), "3pl", input$item_estimate_method, fix=fix)
        }
        x <- x$items        
      }
      x
    })
    
    # Output value box: responses
    output$response_infobox <- renderInfoBox({
      x <- tryCatch(data_responses(), error=function(x) NULL)
      infoBox("Responses", 
              ifelse(is.null(x), "Not Available", "Is Imported"),
              icon = icon("pencil"), fill = TRUE, 
              color=ifelse(is.null(x), "red", "green"))
    })
    
    # Output value box: people
    output$people_infobox <- renderInfoBox({
      x <- tryCatch(data_people(), error=function(x) NULL)
      infoBox("People", 
              ifelse(is.null(x), "Not Available", nrow(x)),
              icon = icon("users"), fill = TRUE, 
              color=ifelse(is.null(x), "red", "green"))
    })
    
    # Output value box: responses
    output$item_infobox <- renderInfoBox({
      x <- tryCatch(data_items(), error=function(x) NULL)
      infoBox("Items",
              ifelse(is.null(x), "Not Available", nrow(x)),
              icon = icon("newspaper-o"), fill = TRUE, 
              color=ifelse(is.null(x), "red", "green"))
    })
    
    
    # Output text: people
    output$people_text <- renderText({
      x <- data_people()$theta
      mu <- mean(x)
      sig <- stats::sd(x)
      paste("<p><b>Mean:</b> ", round(mu, 2), 
            ", <b>SD:</b> ", round(sig, 2), "</p><br>")
    })
    
    # Output text: item
    output$item_text <- renderText({
      x <- data_items()
      mu <- apply(x, 2, mean)
      sig <- apply(x, 2, sd)
      paste("<p><b>a parameters:</b> Mean = ", round(mu["a"], 2), ", SD = ", round(sig["a"], 2), 
            "<br><b>b parameters:</b> Mean = ", round(mu["b"], 2), ", SD = ", round(sig["b"], 2),
            "<br><b>c parameters:</b> Mean = ", round(mu["c"], 2), ", SD = ", round(sig["c"], 2), "</p><br>")
    })
    
    # Output plot: people histogram
    output$people_plot <- renderPlot({
      x <- data_people()
      ggplot(x, aes_string(x="theta", y="..density..")) + 
        geom_histogram(bins=15, colour="skyblue", fill="white") + 
        geom_density(colour="steelblue") +
        xlab(expression(theta)) + ylab("") + theme_bw()
    })
    
    # Output table: items
    output$item_table <- DT::renderDataTable({
      x <- round(data_items(), 2)
      x
    }, rownames=FALSE, options=list(pageLength=10, dom='tip'))
    
    
    # Download: people
    output$people_download <- downloadHandler(
      filename=function(){
        paste("xxirt_people_parameters_", gsub("-","", Sys.Date()), ".txt", sep="")
      }, content=function(file){
        write.csv(data_people(), file, quote=FALSE, row.names=FALSE)
      }
    )
    
    # Download: item
    output$item_download <- downloadHandler(
      filename=function(){
        paste("xxirt_item_parameters_", gsub("-","", Sys.Date()), ".txt", sep="")
      }, content=function(file){
        write.csv(data_items(), file, quote=FALSE, row.names=FALSE)
      }
    )
    
  })
  
}



