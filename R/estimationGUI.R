#' Graphical User Interface
#' @description \code{estimationGUI} creates a shiny app for parameter estimation
#' @export
#' @import shiny ggplot2
#' @importFrom utils write.table read.csv
#' @importFrom stats sd
estimationGUI <- function(){
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
      mainPanel(
        width = 4,
        # response
        wellPanel(
          fileInput("rsp", "Response File", accept="text/plain"),
          selectInput("model", "IRT Model", choices=list("3PL", "2PL", "1PL"))
        ),
        # estimation options
        wellPanel(
          radioButtons("theta", "Theta Parameters", choices=list("Import", "Estimate")),
          radioButtons("item", "Item Parameters", choices=list("Import", "Calibrate"))
        ),
        # estimate thetas
        wellPanel(
          conditionalPanel("input.theta == 'Import'", fileInput('thetafile', "Theta Parameter File", accept="text/plain")),
          conditionalPanel("input.theta == 'Estimate'", selectInput('thetamethod', 'Theta Estimation Method', choices=list("MLE"="estimate.theta.mle","EAP"="estimate.theta.eap","MAP"="estimate.theta.map")))
        ),
        # estimate items
        wellPanel(
          conditionalPanel("input.item == 'Import'", fileInput('itemfile', "Item Parameter File", accept="text/plain")),
          conditionalPanel("input.item == 'Calibrate'", selectInput('itemmethod', 'Item Calibration Method', choices=list("JMLE","MMLE","BME"))),
          conditionalPanel(
            "input.itemmethod == 'BME'",
            h6("Priors for a-parameters (lognormal distr.)"),
            fluidRow(column(6, numericInput("a.mu", "Mean", 0.0, step=0.1)), column(6, numericInput("a.sd", "SD", 0.2, step=0.1))),
            h6("Priors for b-parameters (normal distr.)"),
            fluidRow(column(6, numericInput("b.mu", "Mean", 0.0, step=0.1)), column(6, numericInput("b.sd", "SD", 1.0, step=0.1))),
            h6("Priors for c-parameters (beta distr.)"),
            fluidRow(column(6, numericInput("c.alpha", "Alpha", 5)), column(6, numericInput("c.beta", "Beta", 42)))
          )
        )
      ), # end of sidebar panel
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Console", 
                   conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(HTML("<i class='fa fa-spinner fa-spin fa-5x fa-fw'></i>"))),
                   verbatimTextOutput("console")),
          tabPanel("Results",
                   dataTableOutput("items"), 
                   downloadButton("downloaditems", "Download Items"),
                   dataTableOutput("thetas"), 
                   downloadButton("downloadthetas", "Donwload Thetas")),
          tabPanel("Summary",
                   dataTableOutput("stats"),
                   plotOutput("residual")
                   )
        )# end of tabsetPanel
      ) # end of mainPanel
    ) # end of sidebarLayout
  ))
  
  server <- shinyServer(function(input, output) {
    # responses
    getRsp <- reactive({
      validate(need(input$rsp, "Please import response file."))
      read.csv(input$rsp$datapath, header=TRUE, as.is=TRUE)
    })
    
    # items
    getItem <- reactive({
      if(input$item == 'Import'){
        validate(need(input$itemfile, "Please import item file."))
        items <- read.csv(input$itemfile$datapath, header=TRUE, as.is=TRUE)
      } else if(input$item == 'Calibrate'){
        u <- getRsp()
        if(input$itemmethod == 'JMLE'){
          validate(need(input$thetafile, "Pleaes import theta file for JMLE calibration."))
          items <- estimate.item.jmle(u, getTheta(), model=input$model)$parameters
        } else if(input$itemmethod == 'MMLE'){
          items <- estimate.item.mmle(u, model=input$model)$parameters
        } else if(input$itemmethod == 'BME') {
          items <- estimate.item.bme(u, input$model, input$a.mu, input$a.sd, input$b.mu, input$b.sd, input$c.alpha, input$c.beta)$parameters
        }
      }
      items
    })
    
    # thetas
    getTheta <- reactive({
      if(input$theta == 'Import'){
        validate(need(input$thetafile, "Please import theta file."))
        thetas <- read.csv(input$thetafile$datapath, header=TRUE, as.is=TRUE)
        thetas <- as.vector(thetas[,1])
      } else if(input$theta == 'Estimate'){
        items <- getItem()
        thetas <- match.fun(input$thetamethod)(getRsp(), items$a, items$b, items$c)
      }
      thetas
    })

    # console
    output$console <- renderPrint({
      items <- getItem()
      thetas <- getTheta()
      list(items=items, thetas=thetas)
    })
    
    # table: items
    output$items <- renderDataTable({
      x <- round(getItem(), 3)
      cbind(ID=1:nrow(x), x)
    }, options=list(pageLength=10, dom='tip'))
    
    # table: thetas
    output$thetas <- renderDataTable({
      thetas <- getTheta()
      items <- getItem()
      information <- info(irt(thetas, items$a,items$b, items$c), summary=1, fun=sum)
      se <- 1 / sqrt(information)
      round(data.frame(ID=1:length(thetas), theta=thetas, se=se), 3)
    }, options=list(pageLength=10, dom='tip'))
    
    # download: items
    output$downloaditems <- downloadHandler(
      filename=function(){
        paste(input$rsp$name, "_estimated_items.txt", sep="")
      },
      content=function(file){
        write.table(getItem(), file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    # download: thetas
    output$downloadthetas <- downloadHandler(
      filename=function(){
        paste(input$rsp$name, "_estimated_thetas.txt", sep="")
      },
      content=function(file){
        thetas <- getTheta()
        items <- getItem()
        information <- info(irt(thetas, items$a,items$b, items$c), summary=1, fun=sum)
        se <- 1 / sqrt(information)
        rs <- data.frame(theta=thetas, se=se)
        write.table(rs, file, sep=",", quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
    )
    
    # plot: stats
    output$stats <- renderDataTable({
      rsp <- as.matrix(getRsp())
      thetas <- getTheta()
      items <- getItem()
      
      stat.item <- apply(items, 2, function(x){c(mean(x), sd(x), min(x), max(x))})
      stat.theta <- c(mean(thetas), sd(thetas), min(thetas), max(thetas))
      stat <- t(cbind(stat.item, theta=stat.theta))
      stat <- round(stat, 3)
      stat <- as.data.frame(stat)
      stat <- cbind(par=rownames(stat), stat)
      colnames(stat) <- c("Parameter", "Mean", "SD", "Min.", "Max")
      stat
    }, options=list(dom='tip'))

    # plot: residual
    output$residual <- renderPlot({
      rsp <- as.matrix(getRsp())
      thetas <- getTheta()
      items <- getItem()
      
      x <- irt(thetas, items$a, items$b, items$c)
      res <- rsp - prob(irt(thetas, items$a, items$b, items$c))
      res <- res^2
      res.item <- sqrt(colMeans(res))
      res.theta <- sqrt(rowMeans(res))
      x <- rbind(data.frame(x=items$b, res=res.item, type="Items"),
                 data.frame(x=thetas, res=res.theta, type="Thetas"))
      
      ggplot(x, aes_string(x="x", y="res", color="type")) + 
        geom_point(alpha=.5) + facet_wrap(~type) +
        xlab(expression(paste("b/",theta," parameter"))) + 
        ylab("Residuals (RMSEs)") + 
        coord_cartesian(xlim=c(-4, 4)) +
        guides(color=FALSE, alpha=FALSE) +
        theme_bw() + theme(legend.key=element_blank())
    })
    
  })
  
  shinyApp(ui=ui, server=server)  
}
