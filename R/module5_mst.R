#' Computerized Multistage Testing Simulation
#' @description \code{mst} creates a multistage (MST) object
#' @param pool a data frame of items
#' @param design a numeric vector to specify the MST design (e.g., 1-2-3, or 1-2-2)
#' @param npanel the number of panels
#' @param method the design method (i.e., 'topdown' or 'bottomup')
#' @details
#' The \code{mst} object contains an item pool (\code{pool}), a test assembler (\code{assembler}), 
#' a route map (\code{route}), a stage-module map (\code{module}), a design method (\code{method}),
#' and several constants such as \code{npanel}, \code{nstage}, \code{nmodule}, \code{nroute}.\cr
#' Two indices are used to index modules/testlets. \code{form} number is a unique identifier for all modules,
#' which is used for automated test assembly. \code{index} is a panel-wise unique identifier, which is used for
#' routing. \cr
#' There are two design methods for mst: \code{'bottomup'} and \code{'topdown'}. In the bottomup approach, 
#' constraitns are imposed on individual modules; however, in the topdown approach, constraints are imposed on routes.\cr
#' @return \code{mst} returns a \code{mst} object
#' @examples
#' \dontrun{
#' # ex. 1: 1-2-2 MST, 2 panels, topdown, 20 items, content = c(10, 5, 5)
#' # maximize information at -1 for easy routes and 1 for hard routes
#' pool <- irt.model()$gen.data(1,300)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
#' x <- mst(pool, design=c(1,2,2), npanel=2, method='topdown')
#' x$route
#' x <- mst.objective(x, theta=-1, indices=1:2)
#' x <- mst.objective(x, theta=1, indices=3:4)
#' x <- mst.constraint(x, 1, 20, 20)
#' x <- mst.constraint(x, "content", 10, 10, level=1)
#' x <- mst.constraint(x, "content", 5, 5, level=2)
#' x <- mst.constraint(x, "content", 5, 5, level=3)
#' x <- mst.stage.length(x, c(1,2,3), min=5)
#' x <- mst.assemble(x)
#' freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
#' freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
#' plot(x)
#' plot(x, byroute=TRUE)
#' mst.sim(x, 1)
#' mst.sim(x, -1)
#' # ex. 2: 1-2-2 MST, 2 panels, bottomup, 10 items per stage, content = c(4, 3, 3)
#' # maximize information at -1 for easy modules, 0 for medium modules and 1 for hard modules
#' x <- mst(pool, design=c(1,2,2), npanel=2, method='bottomup')
#' x$module
#' x <- mst.objective(x, theta=0, indices=1)
#' x <- mst.objective(x, theta=-1, indices=c(2,4))
#' x <- mst.objective(x, theta=1, indices=c(3,5))
#' x <- mst.constraint(x, 1, 10, 10)
#' x <- mst.constraint(x, "content", 4, 4, level=1)
#' x <- mst.constraint(x, "content", 3, 3, level=2)
#' x <- mst.constraint(x, "content", 3, 3, level=3)
#' x <- mst.assemble(x)
#' freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
#' freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
#' plot(x)
#' plot(x, byroute=TRUE)
#' mst.sim(x, 1)
#' mst.sim(x, -1)
#' # ex. 3: 1-2-3 MST, 2 panels, topdown, 30 items, 
#' # content = c(10, 10, 10), prohibit routes with radical changes
#' # maximize information at -1 for easy, 0 for medium and 1 for hard routes
#' x <- mst(pool, design=c(1,2,3), npanel=2, method='topdown')
#' x <- mst.route(x, c(1,2,6), "-")
#' x <- mst.route(x, c(1,3,4), "-")
#' x$route
#' x <- mst.objective(x, -1, indices=1)
#' x <- mst.objective(x,  0, indices=2:3)
#' x <- mst.objective(x,  1, indices=4)
#' x <- mst.constraint(x, 1, 30, 30)
#' x <- mst.constraint(x, "content", 10, 10, level=1)
#' x <- mst.constraint(x, "content", 10, 10, level=2)
#' x <- mst.constraint(x, "content", 10, 10, level=3)
#' x <- mst.stage.length(x, c(1,2,3), min=3)
#' x <- mst.assemble(x, timeout=5*60)
#' freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
#' freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
#' plot(x)
#' plot(x, byroute=TRUE)
#' mst.sim(x, 1)
#' mst.sim(x, .2)
#' mst.sim(x, -.2)
#' mst.sim(x, -1)
#' # ex. 4: 1-2-3 MST, 2 panels, bottomup, 10 items per stage, 
#' # content = c(4, 3, 3), prohibit routes with radical changes
#' # target information at 6 (theta=-1) for easy, 6 (theta=0) for medium 
#' # and 6 (theta=1) for hard modules
#' x <- mst(pool, design=c(1,2,3), npanel=2, method='bottomup')
#' x <- mst.route(x, c(1,2,6), "-")
#' x <- mst.route(x, c(1,3,4), "-")
#' x$module
#' x <- mst.objective(x, -1, indices=c(2,4), target=6)
#' x <- mst.objective(x,  0, indices=c(1,5), target=6)
#' x <- mst.objective(x,  1, indices=c(3,6), target=6)
#' x <- mst.constraint(x, 1, 10, 10)
#' x <- mst.constraint(x, "content", 4, 4, level=1)
#' x <- mst.constraint(x, "content", 3, 3, level=2)
#' x <- mst.constraint(x, "content", 3, 3, level=3)
#' x <- mst.stage.length(x, c(1,2,3), min=3)
#' x <- mst.assemble(x, timeout=5*60)
#' freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
#' freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
#' plot(x)
#' plot(x, byroute=TRUE)
#' mst.sim(x, 1)
#' mst.sim(x, .2)
#' mst.sim(x, -.2)
#' mst.sim(x, -1)
#' }
#' @import lpSolveAPI
#' @export
mst <- function(pool, design, npanel, method){
  # validate pool
  pool <- as.data.frame(pool)
  if(any(!c("a","b","c") %in% colnames(pool))) stop("cannot find a-, b-, or c-parameters in item pool")
  # validate design
  if(any(design < 1)) stop("invalid design.")
  # validate method
  if(!method %in% c('topdown', 'bottomup')) stop("invalid method. use 'topdown' or 'bottomup'")
  # constants
  nstage <- length(design)
  nmodule <- sum(design)
  # module-index map
  module <- NULL
  for(s in 1:nstage){
    for(m in 1:design[s]){
      module <- rbind(module, c(stage=s, module=m))
    }
  }
  module <- data.frame(cbind(module, index=1:nrow(module)))
  # route-index map
  route <- list()
  for(i in 1:nstage) route[[i]] <- module[module$stage == i, "index"]
  route <- expand.grid(route)
  colnames(route) <- paste("stage", 1:nstage, sep="")
  route$index <- 1:nrow(route)
  nroute <- nrow(route)
  # assmebler: maxselection = 1
  assembler <- ata(pool, npanel * nmodule, maxselect=1)
  x <- list(calls=list(), design=design, pool=pool, nitem=nrow(pool), npanel=npanel, 
            nstage=nstage, nmodule=nmodule, nroute=nroute, module=module, 
            route=route, assembler=assembler, method=method)
  class(x) <- "mst"
  return(x)
}

#' @rdname mst
#' @description \code{mst.route.index.search} is a helper function for finding the index of a given route vector
mst.route.index.search <- function(x, route){
  if(length(route) == 1 && route %in% 1:x$nroute){
    index <- route
  } else if(length(route) == x$nstage) {
    find.route <- apply(x$route[,1:x$nstage], 1, function(x){ all(x == route) })
    index <- x$route$index[find.route]
  } else {
    stop("invaid route input. use the route index or route vector")
  }
  return(index)
}

#' @rdname mst
#' @description \code{mst.get.index} is a helper function for getitng route (in topdown method) or module (in bottomup method) indices
#' @return \code{mst.get.index} returns a selected route- or module-map in the mst
mst.get.index <- function(x, indices){
  if(x$method == 'topdown'){
    if(is.null(indices)){
      indices <- x$route[,1:x$nstage]
    } else {
      indices <- subset(x$route, x$route$index %in% indices)[,1:x$nstage]
    }
  } else if(x$method == 'bottomup') {
    if(is.null(indices)){
      indices <- data.frame(module=1:x$nmodule)
    } else {
      indices <- data.frame(module=indices)
    }
  }
  return(indices)
}

#' @rdname mst
#' @description \code{mst.route} manipulates routes in the mst
#' @param x the mst object
#' @param route a index number or a vector of module index
#' @param op \code{"+"} for adding a route and \code{"-"} for removing a route
#' @export
mst.route <- function(x, route, op){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  index <- mst.route.index.search(x, route)
  if(op == "+") {
    if(length(index) != 0) stop("the route has been already added")
    x$route <- rbind(x$route, c(route, NA))
  } else if(op == "-") {
    if(length(index) == 0) stop("the route hasn't been added yet")
    x$route <- x$route[-index, ]
  } else {
    stop("invalid operation. use '+' to add a new route or '-' to delete an existing route")
  }
  x$nroute <- nrow(x$route)
  x$route$index <- 1:x$nroute
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.objective} adds objective functions to the mst
#' @param theta a theta point or range for optimization
#' @param indices the index of the route (topdown) or the module (bottomup) for adding objectives
#' @param target the target valus of the TIF objectives. \code{NULL} for maximization
#' @param flatten the parameter for getting a flat TIF
#' @param theta.step the step parameters to bridge \code{theta.lb} and \code{theta.ub}
#' @import lpSolveAPI
#' @export
mst.objective <- function(x, theta, indices=NULL, target=NULL, flatten=NULL, theta.step=2){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst.get.index(x, indices)
  if(length(theta) == 2) {
    theta <- seq(theta[1], theta[2], length.out=theta.step)
  } else if(length(theta) > 2) {
    stop("invalid theta input. use one or two numbers")
  }
  theta <- round(theta, 2)
  for(i in 1:x$npanel){
    for(j in 1:nrow(indices)){
      forms <- unlist(indices[j,]) + (i - 1) * x$nmodule
      if(is.null(target) || is.na(target)){
        x$assembler <- ata.obj.relative(x$assembler, theta, "max", flatten=flatten, forms=forms, collapse=TRUE)
      } else {
        x$assembler <- ata.obj.absolute(x$assembler, theta, target, forms=forms, collapse=TRUE)
      }      
    }
  }
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.constraint} adds constraints to the mst
#' @param coef the coefficients of the constraint
#' @param level the level value for a categorical constraint
#' @param min the minimal value of the contraint
#' @param max the maximal value of the contraint
#' @import lpSolveAPI
#' @export
mst.constraint <- function(x, coef, min=NA, max=NA, level=NULL, indices=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst.get.index(x, indices)
  for(i in 1:x$npanel){
    for(j in 1:nrow(indices)){
      f <- unlist(indices[j,] + (i - 1) * x$nmodule)
      x$assembler <- ata.constraint(x$assembler, coef, min, max, level, forms=f, collapse=TRUE)
    }
  }
  x$calls <- c(x$calls, match.call())
  return(x)  
}

#' @rdname mst
#' @description \code{mst.stage.length} sets the minimal and maximal length for a stage
#' @param stages the stage index
#' @import lpSolveAPI
#' @export
mst.stage.length <- function(x, stages, min=NA, max=NA){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(min) == 1) min <- rep(min, length(stages))
  if(length(max) == 1) max <- rep(max, length(stages))
  if(length(stages) != length(min) || length(stages) != length(max))
    stop("different lengths in stage, min and max")
  for(i in 1:length(stages)){
    if(!stages[i] %in% 1:x$nstage) stop("invalid stage input")
    f <- subset(x$module, x$module$stage == stages[i])$index
    f <- as.vector(outer(f, (1:x$npanel - 1) * x$nmodule, "+"))
    x$assembler <- ata.constraint(x$assembler, 1, min[i], max[i], forms=f, collapse=FALSE)
  }
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.assemble} assembles items
#' @import lpSolveAPI
#' @export
mst.assemble <- function(x, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  x$assembler <- ata.solve(x$assembler, ...)
  if(!is.null(x$assembler$result)) {
    items <- ata.get.items(x$assembler)
    items$panel <- ceiling(items$form / x$nmodule)
    items$index <- (items$form - 1) %% x$nmodule + 1
    items$stage <- x$module$stage[match(items$index, x$module$index)]
    items$module <- x$module$module[match(items$index, x$module$index)]
    x$items <- items
  }
  return(x)  
}

#' @rdname mst
#' @param ... further arguments
#' @export
print.mst <- function(x, ...){
  cat("The MST design has", x$nstage, "stages,", x$nmodule, "modules, and", x$nroute, "routes:\n")
  cat("route map:\n")
  print(x$route)
  cat("stage-module map:\n")
  print(x$module)
  cat("\n")
  cat("ATA assembles", x$npanel, "panel(s) from a pool of", x$nitem, "items:\n\n")
  print(x$assembler)
  cat("\n\n")
  if(is.null(x$items)) {
    cat("It has been assembled yet.\n")
  } else {
    cat("It has been assembled. The first and last 5 items in the results are:\n")
    print(x$items[1:5,])
    cat("...")
    print(x$items[1:5+nrow(x$items)-5,])
  }
  invisible(x)
}

#' @rdname mst
#' @details 
#' \code{plot.mst} draws module information functions if \code{byroute=FALSE}
#' and route information functions if \code{byroute=TRUE}
#' @import ggplot2
#' @export
plot.mst <- function(x, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop('the mst has not been assembled yet.')
  opts <- list(...)
  byroute <- ifelse(is.null(opts$byroute), FALSE, opts$byroute)
    
  theta <- round(seq(-3, 3, .1), 1)
  data <- NULL
  
  if(byroute) {
    for(i in 1:x$nroute){
      r <- subset(x$route, x$route$index == i)[1:x$nstage]
      for(j in 1:x$npanel){
        items <- subset(x$items, x$items$index %in% r & x$items$panel == j)
        info <- irt.stats(irt.model.3pl(data.frame(theta=theta), items), "information")
        info <- rowSums(info)
        data <- rbind(data, data.frame(t=theta, info=info, panel=j, route=i))
      }
    }
    data$panel <- factor(paste("Panel", data$panel))
    data$route <- factor(data$route, levels=1:x$nroute, labels=apply(x$route[,1:x$nstage], 1, paste, collapse="-"))
    g <- ggplot(data, aes_string(x="t", y="info", color="route")) + 
      geom_line() + xlab(expression(theta)) + ylab("Information") + 
      theme_bw() + theme(legend.key=element_blank()) +
      guides(color=guide_legend("Routes")) +
      facet_grid(. ~ panel)
  } else {
    for(i in unique(x$items$form)){
      items <- subset(x$items, x$items$form == i)
      info <- irt.stats(irt.model.3pl(data.frame(theta=theta), items), "information")
      info <- rowSums(info)
      data <- rbind(data, data.frame(t=theta, info=info, form=i, panel=items$panel[1], stage=items$stage[1], module=items$module[1]))
    }
    data$panel <- factor(paste("Panel", data$panel))
    data$stage <- factor(paste("Stage", data$stage))
    data$form <- factor(paste("Form", data$form))
    g <- ggplot(data, aes_string(x="t", y="info", color="form")) + 
      geom_line() + xlab(expression(theta)) + ylab("Information") + 
      theme_bw() + theme(legend.key=element_blank()) +
      guides(color=guide_legend("Modules")) +
      facet_grid(panel ~ stage)
  }
  return(g)
}

#' @rdname mst
#' @description \code{mst.get.items} extracts items from results
#' @param panel the index of panel
#' @param stage the index of stage
#' @param module the index of module
#' @export
mst.get.items <- function(x, panel, stage=NULL, module=NULL, route=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop('the mst has not been assembled yet.')
  input <- list(p=panel, s=stage, m=module, r=route)
  items <- subset(x$items, x$items$panel == input$p)
  if(xor(is.null(stage), is.null(module))) {
    stop("invalid stage and module input")
  } else if(!is.null(stage) && !is.null(module)){
    m <- subset(x$module, x$module$stage == input$s & x$module$module == input$m)$index
    items <- subset(items, items$index %in% m)
  } else if(!is.null(route)) {
    r <- as.vector(subset(x$route, x$route$index == input$r)[,1:x$nstage])
    items <- subset(items, items$index %in% r)
  }
  return(items)
}

#' @rdname mst
#' @description \code{mst.sim} performs a simulation for an assembled mst
#' @param theta.true the true value of theta parameter
#' @importFrom stats runif
#' @export
mst.sim <- function(x, theta.true){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop("the mst has not been assembled yet")
  
  theta.est <- 0
  items <- NULL
  response <- NULL
  route <- rep(NA, x$nstage)
  items.panel <- mst.get.items(x, panel=sample(1:x$npanel, 1))
  for(i in 1:x$nstage){
    # items in the current stage and connetable modules
    if(i == 1) {
      modules <- unique(x$route[,i])
    } else {
      modules <- unique(x$route[x$route[,i-1] ==  route[i-1], i])
    }
    items.stage <- subset(items.panel, items.panel$stage == i & items.panel$index %in% modules)
    # module information
    info <- irt.stats(irt.model.3pl(data.frame(theta=theta.est), items.stage), "information")[1,]
    info <- aggregate(info, by=list(items.stage$index), sum)
    # items in the module with greatest information
    next.module <- info[which.max(info[, 2]), 1]
    items.module <- subset(items.stage, items.stage$index == next.module)
    # generate responses
    p <- irt.stats(irt.model.3pl(data.frame(theta=theta.true), items.module), "probability")[1,]
    u <- (p >= runif(length(p))) * 1
    # append module, items and responses
    route[i] <- next.module
    items <- rbind(items, items.module)
    response <- c(response, u)
    # estimate ability
    theta.est <- estimate.people.3pl.mle(response, items)$people[1,]
  }
  items <- data.frame(response=response, items)
  return(list(panel=items.panel, items=items, true=theta.true, est=theta.est, route=route))
}


