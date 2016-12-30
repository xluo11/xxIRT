#' Graphical User Interface for CAT Simulation
#' @description \code{gui.cat} is a shiny app for cat simulation
#' @import shiny ggplot2
#' @importFrom utils write.table read.csv
#' @export
gui.cat <- function(){
  ui <- shinyUI(fluidPage(
    # css theme
    tags$head(tags$link(rel="stylesheet", type="text/css", href="https://bootswatch.com/paper/bootstrap.min.css")),
    
    # title
    tags$div(
      img(src="https://raw.githubusercontent.com/xluo11/xxIRT/master/res/img/logo.png", height=50, width=50),
      span("Computerized Adaptive Testing", class="h4"),
      a(" -- package: xxIRT || author: xiao luo", href="https://github.com/xluo11/xxIRT")
    ),
    
    # layout               
    sidebarLayout(
      # sidebar panel
      mainPanel(width = 4,
        # items
        wellPanel(
          fileInput("poolfile", "Item Pool", accept="text/plain")
        ),
        
        # true theta, test length
        wellPanel(
          selectInput('simtype', 'Mass or Single Simulation?', choices=list("Single Simulation"="single", "Mass Simulation"="mass")),
          conditionalPanel("input.simtype == 'single'", numericInput('trueability', 'True Ability', 0, min=-3, max=3, step=0.1)),
          conditionalPanel("input.simtype == 'mass'", fileInput('peoplefile', "Theta Parameters", accept="text/plain")),
          fluidRow(
            column(6, numericInput('minlength', 'Min. Length', 10, min=1)),
            column(6, numericInput('maxlength', 'Max. Length', 30, min=1))
          )
        ),
        
        # selection
        wellPanel(
          selectInput('selectmethod', 'Selection Rule', choices=list("Default"="cat.select.default", "C-CAT"="cat.select.ccat", "Shadow Test"="cat.select.shadow")),
          numericInput("randomesque", "Item Exposure Control", 1, min=1),
          conditionalPanel(
            "input.selectmethod == 'cat.select.ccat'", 
            textInput('ccat.target', 'Content Targets (%)', "", placeholder="e.g., 0.5, 0.3, 0.2"),
            numericInput('ccat.random', 'Initial Randomness', 1, min=0)
          ),
          conditionalPanel(
            "input.selectmethod == 'cat.select.shadow'",
            fluidRow(column(6, textInput('shadow.variable', 'Variable', "")),
                     column(6, textInput('shadow.level', 'Level', ""))),
            fluidRow(column(6, numericInput('shadow.min', 'Min', "")),
                     column(6, numericInput('shadow.max', 'Max', ""))),
            actionButton("shadow.add", "Add Constraint")
          )
        ),
        
        # estimation
        wellPanel(
          selectInput('estimatemethod', 'Estimation Rule', choices=list("Default"="cat.estimate.default"))
        ),
        
        # stop
        wellPanel(
          selectInput('stopmethod', 'Stopping Rule', choices=list("Default"="cat.stop.default", "Projection"="cat.stop.projection")),
          conditionalPanel(
            "input.stopmethod == 'cat.stop.default'",
            selectInput('stoptype', 'Criterion', choices=list('Standard Error'='stop.se', 'Minimum Information'='stop.mi', 'Cut Score'='stop.cut')),
            numericInput('stopvalue', 'Value', 0.3)
          ),
          conditionalPanel(
            "input.stopmethod == 'cat.stop.projection'",
            selectInput('projectionmethod', 'Projection Method', choices=list('Maximal Information'='information', 'Extreme Difficulty'='difficulty')),
            numericInput('projectioncut', 'Cut Score', 0)
          )
        ),
        
        # submit button
        wellPanel(
          tags$button(id="submit", class="btn action-button btn-primary", HTML("<span class='glyphicon glyphicon-play'></span>&nbsp;&nbsp;Run!"))
        )
      ), # end of sidebar panel
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   verbatimTextOutput("console0"),
                   verbatimTextOutput("console1")),
          tabPanel("Results",
            tags$div(class="pull-right",
                 tags$button(id="prev", class="btn btn-sm action-button btn-default", HTML("<span class='glyphicon glyphicon-chevron-left'></span>")),
                 htmlOutput("index", inline=TRUE),
                 tags$button(id="next", class="btn btn-sm action-button btn-default", HTML("<span class='glyphicon glyphicon-chevron-right'></span>"))),
             dataTableOutput("table"),
             plotOutput("plot")),
          tabPanel("Summary", 
                   plotOutput("summary.theta"), 
                   plotOutput("summary.length"))
        ) # end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    # a reactive variable to hold data
    v <- reactiveValues(result=NULL, index=1, max=1, constraints=NULL)

    # get item parameters (pool)
    get.items <- reactive({
      validate(need(input$poolfile, "Please import the item pool"))
      read.csv(input$poolfile$datapath, header=TRUE, as.is=TRUE)
    })
    
    # get people parameters
    get.people <- reactive({
      if(input$simtype == 'mass'){
        validate(need(input$peoplefile, "Please import people parameters."))
        people <- read.csv(input$peoplefile$datapath, header=TRUE, as.is=TRUE)$theta
      } else if(input$simtype == 'single') {
        people <- input$trueability
      }
      people
    })
    
    # get common options
    get.options <- reactive({
      validate(need(input$minlength <= input$maxlength, "Please check the minimal and maximal test length."))
      list(min=input$minlength, max=input$maxlength, select.random=input$randomesque)
    })
    
    # get selection method options
    get.select.options <- reactive({
      opts <- list()
      if(input$selectmethod == "cat.select.ccat") {
        validate(need(grepl("^([.,0-9 ]+)+$", input$ccat.target), "Please set content targets properly--e.g, .5, .3, .2"))
        target <- as.numeric(unlist(strsplit(input$ccat.target, ",")))
        opts$ccat.target <- target
        opts$ccat.random <- input$ccat.random
        v$constraints <- data.frame(name="content", level=1:length(target), min=target*input$minlength, max=target*input$maxlength)
      } else if(input$selectmethod == "cat.select.shadow") {
        opts$shadow.constraints <- v$constraints
      }  
      opts
    })
    
    # get estimation method options
    get.estimate.options <- reactive({
      opts <- list()
      opts
    })
    
    # get stopping method options
    get.stop.options <- reactive({
      opts <- list()
      if(input$stopmethod == "cat.stop.default"){
        validate(need(input$stopvalue, "Please eneter a stopping rule threshold"))
        opts[[input$stoptype]] <- input$stopvalue
      } else if(input$stopmethod == "cat.stop.projection") {
        opts$projection.method <- input$projectionmethod
        opts$projection.constraints <- v$constraints
        opts$projection.cut <- input$projectioncut
      }
      opts
    })
    
    # run simulations
    cat <- reactive({
      # inputs
      items <- get.items()
      people <- get.people()
      opts <- get.options()
      opts <- c(opts, get.select.options())
      opts <- c(opts, get.estimate.options())
      opts <- c(opts, get.stop.options())
      
      # simulation
      v$result <- list()
      for(i in 1:length(people)){
        x <- cat.sim(people[i], items, opts, 
                     cat.select=match.fun(input$selectmethod), 
                     cat.estimate=match.fun(input$estimatemethod), 
                     cat.stop=match.fun(input$stopmethod))
        v$result[[i]] <- x
        v$index <- i
      }
      v$max <- i
      v$index <- 1
    })
    
    # buttons to add constraints to shadow-test selection method
    observeEvent(input$shadow.add, {
      validate(need(input$shadow.variable, "Enter a variable name for adding shadow test constraint"))
      validate(need(input$shadow.level, "Enter a variable level for adding shadow test constraint"))
      validate(need(input$shadow.min, "Enter a minimal value for adding shadow test constraint"))
      validate(need(input$shadow.max, "Enter a maximal value for adding shadow test constraint"))
      tmp <- data.frame(name=input$shadow.variable, level=input$shadow.level, min=input$shadow.min, max=input$shadow.max)
      v$constraints <- rbind(v$constraints, tmp)
    })                         
    
    # 'submit' button: nullify results and run simulations
    observeEvent(input$submit, {
      cat()
    })
    
    # 'prev' button: flip to previous simulation results
    observeEvent(input$prev, {
      v$index <- ifelse(v$index == 1, v$max, v$index - 1)
    })
    
    # 'next' button: flip to next simulation results
    observeEvent(input$'next', {
      v$index <- ifelse(v$index == v$max, 1, v$index + 1)
    })
    
    # html: show simulation index
    output$index <- renderPrint({
      HTML(paste("&nbsp;&nbsp;<b>#", v$index, "/", v$max, "</b>&nbsp;&nbsp;", sep=""))
    })

    # console0: cat constraints
    output$console0 <- renderPrint({
      validate(need(v$constraints, "No Constraints"))
      v$constraints
    })
    
    # console1: results
    output$console1 <- renderPrint({
      get.items()
      validate(need(v$result, "No results yet"))
      v$result[[v$index]]
    })
    
    # table: tabulate cat results
    output$table <- renderDataTable({
      validate(need(v$result, "No results."))
      x <- v$result[[v$index]]$admin
      x <- round(x, 2)
      colnames(x)[1:3] <- c("responses", "theta", "se")
      x <- cbind(position=1:nrow(x), x)
      x
    }, options=list(pageLength=20, dom='tip'))
    
    # plot: draw cat results
    output$plot <- renderPlot({
      validate(need(v$result, ""))
      x <- v$result[[v$index]]
      plot(x)
    })
    
    # summary.theta
    output$summary.theta <- renderPlot({
      validate(need(input$simtype == 'mass', "No summary for single simulation."))
      validate(need(v$result, "No results."))
      
      t.true <- sapply(v$result, function(x){x$true})
      t.est  <- sapply(v$result, function(x){x$est})
      t.se <- sapply(v$result, function(x){x$stats[,"se"][x$len]})
      t <- data.frame(true=t.true, est=t.est, se=t.se)
      
      ggplot(t, aes_string(x="true", y="est")) + 
        geom_point(aes_string(color=1, alpha=.3, size="se")) + geom_smooth(linetype=2) +
        xlab(expression(paste("True ", theta))) + ylab(expression(paste("Est. ", theta))) + 
        coord_cartesian(xlim=c(-4, 4), ylim=c(-4, 4)) + guides(color=FALSE, alpha=FALSE, size=FALSE) + 
        theme_bw() + theme(legend.key=element_blank())    
    })
    
    # summary.length
    output$summary.length <- renderPlot({
      validate(need(input$simtype == 'mass', ""))
      validate(need(v$result, ""))
      
      t.true <- sapply(v$result, function(x){x$true})
      t.len <- sapply(v$result, function(x){x$len})
      t.se <- sapply(v$result, function(x){x$stats[,"se"][x$len]})
      t <- data.frame(true=t.true, len=t.len, se=t.se)
      
      ggplot(t, aes_string(x="true", y="len")) + 
        geom_point(aes_string(color=1, alpha=.5, size="se")) + geom_smooth(linetype=2) +
        xlab(expression(paste("True ", theta))) + ylab("Test Length") + 
        coord_cartesian(xlim=c(-4, 4)) + guides(color=FALSE, alpha=FALSE, size=FALSE) + 
        theme_bw() + theme(legend.key=element_blank())
    })
  })
  
  shinyApp(ui=ui, server=server)  
}
