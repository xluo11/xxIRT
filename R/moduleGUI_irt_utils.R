library(shiny)
library(shinydashboard)
library(xxIRT)

shinyApp(ui=dashboardPage(
  dashboardHeader(
    title = "IRT Utils"  
  ),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      column(width = 9, 
        valueBoxOutput("n_people_valuebox"),
        valueBoxOutput("n_item_valuebox"),
        box(title="Item Characteristic Curve", status="primary", solidHeader=FALSE,
          checkboxInput("people_total", "Sum probability over items?", value=FALSE),
          plotOutput("prob")
        ),
        box(title="Item Information Function", status="primary", solidHeader=FALSE,
          checkboxInput("item_total", "Sum information over items?", value=FALSE),
          plotOutput("info")
        )
      ),
      
      column(width = 3,
             box(width = NULL, title = 'People Parameters', 
                 status = 'warning', solidHeader=TRUE, collapsible=FALSE,
                 selectInput("people_method", "How to get people parameters", list("Generate"="gen", "Type In"="enter", "Import File"="upload")),
                 uiOutput("people_parameters")),
             box(width = NULL, title = 'Items Parameters', 
                 status = 'warning', solidHeader=TRUE, collapsible=FALSE,
                 selectInput("items_method", "How to get item parameters", list("Generate"="gen", "Type In"="enter", "Import File"="upload")),
                 uiOutput("items_parameters"))        
             
      )
    )
  )
), server=function(input, output, session){
  
  # Render People Parameter Box
  output$people_parameters <- renderUI({
    if(input$people_method == "upload") {
      fileInput("people_file", "Upload People Parameters", 
                accept=c("text/csv", "text/plain"))
    } else if(input$people_method == "gen") {
      tagList(
        numericInput("people_num", "Number of People", 10, min=1),
        fluidRow(
          column(6, numericInput("people_mean", "Mean", 0)),
          column(6, numericInput("people_sd", "SD", 1, min=0))
        ),
        actionButton("people_gen_add", "Add")
      )
    } else if(input$people_method == "enter") {
      tagList(
        numericInput("people_theta", "Theta", 0),
        actionButton("people_enter_add", "Add")
      )
    }
  })
  
  # Render Item Parameter Box
  output$items_parameters <- renderUI({
    if(input$items_method == "upload") {
      fileInput("item_file", "Upload Item Parameters", accept=c("text/csv", "text/plain"))
    } else if(input$items_method == "gen") {
      tagList(
        numericInput("item_num", "Number of Items", 5, min=1),
        fluidRow(
          column(6, numericInput("item_a_mean", "a: Log Mean", 0)),
          column(6, numericInput("item_a_sd", "a: Log SD", 0.2, min=0))
        ),
        fluidRow(
          column(6, numericInput("item_b_mean", "b: Mean", 0)),
          column(6, numericInput("item_b_sd", "b: SD", 1, min=0))
        ),
        fluidRow(
          column(6, numericInput("item_c_alpha", "c: Alpha", 5, min=0)),
          column(6, numericInput("item_c_beta", "c: Beta", 42, min=0))
        ),
        actionButton("item_gen_add", "Add") 
      )
    } else if(input$items_method == "enter") {
      tagList(
        numericInput("item_a", "a (discrimination)", 1, min=0),
        numericInput("item_b", "b (difficulty)", 0),
        numericInput("item_c", "c (pseudo-guessing)", 0, min=0, max=1),
        actionButton("item_enter_add", "Add")      
      )
    }
  })
  
  # Reactive value to hold people and items parameters
  rv <- reactiveValues(people=NULL, items=NULL)
  
  # Generate people parameters
  observeEvent(input$people_gen_add, {
    req(input$people_num, input$people_mean, input$people_sd)
    t <- stats::rnorm(input$people_num, mean=input$people_mean, sd=input$people_sd)
    rv$people <- data.frame(theta=t)
  })
  
  # Generate item parameters
  observeEvent(input$item_gen_add, {
    req(input$item_num, input$item_a_mean, input$item_a_sd, input$item_b_mean, input$item_b_sd, input$item_c_alpha, input$item_c_beta)
    a <- exp(stats::rnorm(input$item_num, mean=input$item_a_mean, sd=input$item_a_sd))
    b <- stats::rnorm(input$item_num, mean=input$item_b_mean, sd=input$item_b_sd)
    c <- stats::rbeta(input$item_num, shape1=input$item_c_alpha, shape2=input$item_c_beta)
    rv$items <- data.frame(a=a, b=b, c=c)
  })
  
  # Enter people parameters 
  observeEvent(input$people_enter_add, {
    req(input$people_theta)
    rv$people <- rbind(rv$people, data.frame(theta=input$people_theta))
  })
  
  # Enter item parameters 
  observeEvent(input$item_enter_add, {
    req(input$item_a, input$item_b, input$item_c)
    rv$items <- rbind(rv$items, data.frame(a=input$item_a, b=input$item_b, c=input$item_c))
  })
  
  # Upload people parameters
  observeEvent(input$people_file, {
    req(input$people_file$datapath)
    validate(need(input$people_file$size <= 500000, "file is too large"))
    x <- read.csv(input$people_file$datapath, header=TRUE, as.is=TRUE)
    colnames(x) <- tolower(colnames(x))
    rv$people <- x[, "theta", drop=FALSE]
  })
  
  # Upload item parameters
  observeEvent(input$item_file, {
    req(input$item_file$datapath)
    validate(need(input$item_file$size <= 500000, "file is too large"))
    x <- read.csv(input$item_file$datapath, header=TRUE, as.is=TRUE)
    colnames(x) <- tolower(colnames(x))
    rv$items <- x[, c("a", "b", "c"), drop=FALSE]
  })
  
  # Render people count value box
  output$n_people_valuebox <- renderValueBox({
    n <- ifelse(is.null(rv$people), 0, nrow(rv$people))
    valueBox(n, "Test Takers", icon=icon("users"), color="orange")
  })
  
  # Render item count value box
  output$n_item_valuebox <- renderValueBox({
    n <- ifelse(is.null(rv$items), 0, nrow(rv$items))
    valueBox(n, "Items", icon=icon("newspaper-o"), color="green")
  })
  
  # Render item characteristic curves
  output$prob <- renderPlot({
    req(rv$people, rv$items)
    x <- irt_model("3pl", people=rv$people, items=rv$items)
    plot(x, stats="prob", total=input$people_total)
  })
  
  # Render item information functions
  output$info <- renderPlot({
    req(rv$people, rv$items)
    x <- irt_model("3pl", people=rv$people, items=rv$items)
    plot(x, stats="info", total=input$item_total)
  })
})