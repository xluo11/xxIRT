#' Graphical User Interface
#' @description \code{catGUI} creates a shiny app for \code{cat.sim}
#' @details 
#' Imported item files and theta files must be comma-delimited and have headers
#' @export
#' @import shiny ggplot2
#' @importFrom utils write.table read.csv
#' @importFrom stats cor sd
catGUI <- function(){
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
      mainPanel(
        width = 4,
        # items
        wellPanel(
          fileInput("poolfile", "Item Pool", accept="text/plain")
        ),
        # true theta, test length
        wellPanel(
          selectInput('simtype', 'Mass or Single Simulation?', choices=list("Single Simulation"="single", "Mass Simulation"="mass")),
          conditionalPanel("input.simtype == 'single'", numericInput('theta', 'True Ability', 0, min=-3, max=3, step=0.1)),
          conditionalPanel("input.simtype == 'mass'", fileInput('thetafile', "Thetas", accept="text/plain")),
          fluidRow(
            column(6, numericInput('min', 'Min. Length', 10, min=1)),
            column(6, numericInput('max', 'Max. Length', 30, min=1))
          )
        ),
        # selection
        wellPanel(
          selectInput('select', 'Selection Rule', choices=list("Default"="cat.select.default", "C-CAT"="cat.select.ccat")),
          numericInput("randomesque", "Item Exposure Control", 1, min=1),
          conditionalPanel("input.select == 'cat.select.ccat'", 
                           textInput('ccat.target', 'Content Distribution', "", placeholder="e.g., 0.5, 0.3, 0.2"),
                           numericInput('ccat.random', 'Random', 5, min=0))
        ),
        # estimation
        wellPanel(
          selectInput('estimate', 'Estimation Rule', choices=list("Default"="cat.estimate.default"))
        ),
        # stop
        wellPanel(
          selectInput('stop', 'Termination Rule', choices=list("Default"="cat.stop.default")),
          conditionalPanel("input.stop == 'cat.stop.default'",
                           selectInput('stop.type', 'Criterion', choices=list('Standard Error'='stop.se', 'Minimum Information'='stop.mi', 'Cut Score'='stop.cut')),
                           numericInput('stop.value', 'Value', 0.3))
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
                   verbatimTextOutput("console")),
          tabPanel("Results",
                   fluidRow(
                     column(6, downloadButton("download")),
                     column(6, tags$div(
                       class="pull-right",
                       tags$button(id="prev", class="btn btn-sm action-button btn-default", HTML("<span class='glyphicon glyphicon-chevron-left'></span>")),
                       htmlOutput("index", inline=TRUE),
                       tags$button(id="next", class="btn btn-sm action-button btn-default", HTML("<span class='glyphicon glyphicon-chevron-right'></span>"))))
                   ),
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
    v <- reactiveValues(result=NULL, index=1, max=1)
    
    cat <- reactive({
      # items
      validate(need(input$poolfile, "Please import item pool file."))
      pool <- read.csv(input$poolfile$datapath, header=TRUE, as.is=TRUE)
      # thetas
      if(input$simtype == 'mass'){
        validate(need(input$thetafile, "Please import theta file."))
        thetas <- read.csv(input$thetafile$datapath, header=TRUE, as.is=TRUE)[,1]
      } else if(input$simtype == 'single') {
        thetas <- input$theta
      }
      
      # options
      validate(need(input$min <= input$max, "Please check the min and max test length."))
      opts <- list(min=input$min, max=input$max, select.random=input$randomesque)
      # selection rule
      if(input$select == "cat.select.ccat") {
        validate(
          need(grepl("^([.,0-9 ]+)+$", input$ccat.target) && as.numeric(unlist(strsplit(input$ccat.target, ","))), 
               "Please set content distribution target properly: e.g., .50, .30, .20")
        )
        opts$ccat.target <- as.numeric(unlist(strsplit(input$ccat.target, ",")))
        opts$ccat.random <- input$ccat.random
      }
      # stop rule
      if(input$stop == "cat.stop.default")
        opts[[input$stop.type]] <- input$stop.value
      # simulation
      results <- list()
      for(i in 1:length(thetas)){
        x <- cat.sim(thetas[i], pool, opts, cat.select=match.fun(input$select), cat.estimate=match.fun(input$estimate), cat.stop=match.fun(input$stop))
        results[[i]] <- x
      }
      v$max <- i
      return(results)
    })
    
    # button: submit
    observeEvent(input$submit, {
      if(!is.null(v$result)) v$result <- NULL
      v$result <- cat()
    })
    
    # button: previous simulation
    observeEvent(input$prev, {
      v$index <- ifelse(v$index == 1, v$max, v$index - 1)
    })
    
    # button: next simulation
    observeEvent(input$'next', {
      v$index <- ifelse(v$index == v$max, 1, v$index + 1)
    })
    
    # html: show simulation index
    output$index <- renderPrint({
      HTML(paste("&nbsp;&nbsp;<b>#", v$index, "/", v$max, "</b>&nbsp;&nbsp;", sep=""))
    })
    
    # html: console output
    output$console <- renderPrint({
      validate(need(v$result, "No Results."))
      v$result[[v$index]]
    })
    
    # table: list of cat results
    output$table <- renderDataTable({
      validate(need(v$result, "No results."))
      x <- v$result[[v$index]]$admin
      x <- round(x, 2)
      colnames(x)[1:3] <- c("Response", "Theta", "SE")
      x <- cbind(Position=1:nrow(x), x)
      x
    }, options=list(pageLength=20, dom='tip'))
    
    # plot: plot of cat results
    output$plot <- renderPlot({
      validate(need(v$result, ""))
      x <- v$result[[v$index]]
      plot.cat(x)
    })
    
    # download
    output$download <- downloadHandler(
      filename = function(){
        "catGUI_simulation_results.txt"
      },
      content = function(file){
        validate(need(v$result, ""))
        rs <- NULL
        for(i in 1:length(v$result)){
          x <- v$result[[i]]$admin
          x <- cbind(sim=i, position=1:nrow(x), round(x, 3))
          rs <- rbind(rs, x)
        }
        write.table(rs, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    # summary.theta
    output$summary.theta <- renderPlot({
      validate(need(input$simtype == 'mass', "No summary for single simulation."))
      validate(need(v$result, "No results."))
      t.true <- sapply(v$result, function(x){x$true})
      t.est  <- sapply(v$result, function(x){x$est})
      t.se <- sapply(v$result, function(x){x$stats[,"se"][x$len]})
      t <- data.frame(true=t.true, est=t.est, se=t.se)
      ggplot(t, aes_string(x="true", y="est")) + 
        geom_point(aes_string(color=1, alpha=.3, size="se")) + 
        geom_smooth(linetype=2) +
        annotate("text", x=-3, y=3.8, label=paste("Corr. =", round(cor(t.true, t.est), 2))) +
        annotate("text", x=-3, y=3, label=paste("RMSE =", round(rmse(t.true, t.est), 2))) +
        xlab(expression(paste("True ", theta))) + 
        ylab(expression(paste("Est. ", theta))) + 
        coord_cartesian(xlim=c(-4, 4), ylim=c(-4, 4)) + 
        guides(color=FALSE, alpha=FALSE, size=FALSE) + 
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
        geom_point(aes_string(color=1, alpha=.5, size="se")) + 
        geom_smooth(linetype=2) +
        annotate("text", x=-3, y=max(t.len) - 2, label=paste("Mean =", round(mean(t.len), 2))) +
        annotate("text", x=-3, y=max(t.len) - 4, label=paste("SD =", round(sd(t.len), 2))) +
        xlab(expression(paste("True ", theta))) + ylab("Test Length") + 
        coord_cartesian(xlim=c(-4, 4)) + 
        guides(color=FALSE, alpha=FALSE, size=FALSE) + 
        theme_bw() + theme(legend.key=element_blank())
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}