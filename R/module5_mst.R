#' Computerized Multistage Testing (MST)
#' @description \code{mst} creates a multistage (MST) object
#' @param pool a data frame of item parameters
#' @param design a numeric vector of the MST design (e.g., 1-2-3, or 1-2-2)
#' @param npanel the number of panels
#' @param method the design method (i.e., 'topdown' or 'bottomup')
#' @param len the module/route length
#' @param maxselect the maximum selection of items
#' @details
#' The \code{mst} object contains an item pool (\code{pool}), a test assembler (\code{assembler}), 
#' a route map (\code{route}), a stage-module map (\code{module}), a design method (\code{method}),
#' and several constants such as \code{npanel}, \code{nstage}, \code{nmodule}, \code{nroute}.\cr
#' Two indices are used to index modules/testlets. \code{form} number is a unique identifier for all modules,
#' which is used for automated test assembly. \code{index} is a panel-wise unique identifier, which is used for
#' routing. \cr
#' 
#' There are two design methods for mst: \code{'bottomup'} and \code{'topdown'}. In the bottomup approach, 
#' constraitns are imposed on individual modules. Conversely, in the topdown approach, constraints are imposed on routes.\cr
#' @examples
#' \dontrun{
#' # generate a 300-item pool
#' pool <- irt_model("3pl")$gendata(1,300)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
#' 
#' ## ex. 1: 1-2-2 MST, 2 panels, topdown
#' ## 20 items, content = c(10, 5, 5)
#' ## maximize information at -1 and 1 for easy and hard routes
#' x <- mst(pool, design=c(1, 2, 2), npanel=2, method='topdown', len=20, maxselect=1)
#' x <- mst_objective(x, theta=-1, indices=1:2)
#' x <- mst_objective(x, theta= 1, indices=3:4)
#' x <- mst_constraint(x, coef="content", min=10, max=10, level=1)
#' x <- mst_constraint(x, coef="content", min=5, max=5, level=2)
#' x <- mst_constraint(x, coef="content", min=5, max=5, level=3)
#' x <- mst_stage_length(x, stages=c(1, 2, 3), min=1)
#' x <- mst_assemble(x, timeout=10)
#' x$items
#' plot(x)
#' plot(x, byroute=TRUE)
#' freq(mst_get_items(x, panel=1, route=1)$content, 1:3)$freq
#' freq(mst_get_items(x, panel=2, route=4)$content, 1:3)$freq
#' 
#' ## ex. 2: 1-2-3 MST, 2 panels, bottomup, 
#' ## 10 items in each module, content = c(4, 3, 3)
#' ## maximize information at -1, 0 and 1 for easy, medium, and hard modules
#' x <- mst(pool, design=c(1, 2, 3), npanel=2, method='bottomup', len=10, maxselect=1)
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_objective(x, theta= 0, indices=1)
#' x <- mst_objective(x, theta=-1, indices=c(2,4))
#' x <- mst_objective(x, theta= 1, indices=c(3,5))
#' x <- mst_constraint(x, coef="content", min=4, max=4, level=1)
#' x <- mst_constraint(x, coef="content", min=3, max=3, level=2)
#' x <- mst_constraint(x, coef="content", min=3, max=3, level=3)
#' x <- mst_assemble(x, timeout=10)
#' plot(x)
#' plot(x, byroute=TRUE)
#' freq(mst_get_items(x, panel=1, route=1)$content, 1:3)$freq
#' freq(mst_get_items(x, panel=2, route=4)$content, 1:3)$freq
#' 
#' ## ex. 3: 1-2-3 MST, 2 panels, topdown, 30 items, 
#' ## 10 items in each content area
#' ## target information at 18 at -1, 0, and 1 for easy, medium and hard routes
#' x <- mst(pool, design=c(1, 2, 3), npanel=2, method='topdown', len=30, maxselect=1)
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_objective(x, theta=-1, indices=1, target=16)
#' x <- mst_objective(x, theta= 0, indices=2:3, target=16)
#' x <- mst_objective(x, theta= 1, indices=4, target=16)
#' x <- mst_constraint(x, coef="content", min=10, max=10, level=1)
#' x <- mst_constraint(x, coef="content", min=10, max=10, level=2)
#' x <- mst_constraint(x, coef="content", min=10, max=10, level=3)
#' x <- mst_stage_length(x, stages=c(1, 2, 3), min=3)
#' x <- mst_assemble(x, timeout=20)
#' plot(x)
#' plot(x, byroute=TRUE)
#' freq(mst_get_items(x, panel=1, route=1)$content, 1:3)$freq
#' freq(mst_get_items(x, panel=2, route=4)$content, 1:3)$freq
#' }
#' @import lpSolveAPI
#' @export
mst <- function(pool, design, npanel, method, len=NULL, maxselect=NULL){
  # validatioin
  if(any(design < 1)) stop("invalid design.")
  method <- tolower(gsub("[- ]", "", method))
  if(!method %in% c('topdown', 'bottomup')) stop("invalid method. use 'topdown' or 'bottomup'")
  
  # Constants
  nstage <- length(design)
  nmodule <- sum(design)
  
  # Module-index map
  module <- NULL
  for(s in 1:nstage)
    for(m in 1:design[s])
      module <- rbind(module, c(stage=s, module=m))
  module <- data.frame(cbind(module, index=1:nrow(module)))
  
  # route-index map
  route <- list()
  for(i in 1:nstage) route[[i]] <- module[module$stage == i, "index"]
  route <- expand.grid(route)
  colnames(route) <- paste("stage", 1:nstage, sep="")
  route$index <- 1:nrow(route)
  nroute <- nrow(route)
  
  # ata
  assembler <- ata(pool, npanel * nmodule)
  x <- list(design=design, pool=pool, nitem=nrow(pool), npanel=npanel, 
            nstage=nstage, nmodule=nmodule, nroute=nroute, module=module, 
            route=route, assembler=assembler, method=method)
  class(x) <- "mst"
  
  # constraint: test length
  if(!is.null(len) && length(len) == 1) x <- mst_constraint(x, 1, len, len)
  if(!is.null(len) && length(len) == 2) x <- mst_constraint(x, 1, len[1], len[2])
  if(!is.null(len) && length(len) > 2) stop("invalid length. use 1 or 2 numbers.")
  
  # constraint: maxselect
  if(!is.null(maxselect)) x$assembler <- ata_item_maxselect(x$assembler, maxselect)
  
  return(x)
}


#' @rdname mst
#' @description \code{mst_route} adds and removes routes in the mst
#' @param x the mst object
#' @param route a vector of form indices to represent the route
#' @param op \code{"+"} for adding a route and \code{"-"} for removing a route
#' @export
mst_route <- function(x, route, op){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  index <- apply(x$route[, 1:x$nstage], 1, function(r) all(r == route))
  
  if(op == "+") {
    if(any(index)) stop("the route has been already added")
    if(!all(route %in% 1:x$nmodule))
      stop("invalid route. index is out of bound.")
    if(!all(with(x$module, stage[match(route, index)]) == 1:x$nstage))
      stop("invalid route. make sure it consists of one module in each stage.")
    x$route <- rbind(x$route, c(route, NA))
  } else if(op == "-") {
    if(!any(index)) stop("the route hasn't been added yet")
    x$route <- x$route[-which(index), ]
  } else {
    stop("invalid operation. use '+' to add a new route or '-' to remove an existing route.")
  }
  
  x$nroute <- nrow(x$route)
  x <- mst_reindex_routes(x)
  return(x)
}


#' @rdname mst
#' @description \code{mst_objective} adds objective functions to the mst
#' @param theta a theta point or interval for optimization
#' @param indices the index of the route (topdown) or the module (bottomup) for adding objectives
#' @param target the target valus of the TIF objectives. \code{NULL} for maximization
#' @param flatten the parameter for getting a flat TIF
#' @param theta.step the step parameters to process \code{theta} interval
#' @import lpSolveAPI
#' @export
mst_objective <- function(x, theta, indices=NULL, target=NULL, flatten=NULL, theta.step=2){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst_indices_input(x, indices)
  
  if(length(theta) == 2) {
    theta <- seq(theta[1], theta[2], length.out=theta.step + 2)
    theta <- theta[-c(1, length(theta))]
  } else if(length(theta) > 2) {
    stop("invalid theta input. use one or two numbers to set TIF objective at a point or over an interval.")
  }
  theta <- round(theta, 2)
  
  for(i in 1:x$npanel) {
    for(j in 1:nrow(indices)) {
      f <- unlist(indices[j, ]) + (i - 1) * x$nmodule
      if(is.null(target) || is.na(target)) {
        x$assembler <- ata_obj_relative(x$assembler, theta, mode="max", flatten=flatten, forms=f, collapse=TRUE)
      } else {
        x$assembler <- ata_obj_absolute(x$assembler, theta, target=target, forms=f, collapse=TRUE)
      }      
    }
  }
  
  return(x)
}


#' @rdname mst
#' @description \code{mst_constraint} adds constraints to the mst
#' @param coef the coefficients of the constraint
#' @param level the level value for a categorical constraint
#' @param min the minimum value of the contraint
#' @param max the maximum value of the contraint
#' @import lpSolveAPI
#' @export
mst_constraint <- function(x, coef, min=NA, max=NA, level=NULL, indices=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst_indices_input(x, indices)
  
  for(i in 1:x$npanel){
    for(j in 1:nrow(indices)){
      f <- unlist(indices[j,] + (i - 1) * x$nmodule)
      x$assembler <- ata_constraint(x$assembler, coef, min, max, level, forms=f, collapse=TRUE)
    }
  }
  
  return(x)  
}


#' @rdname mst
#' @description \code{mst_stage_length} sets the minimum and maximum length for a stage
#' @param stages the stage index
#' @import lpSolveAPI
#' @export
mst_stage_length <- function(x, stages, min=NA, max=NA){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(min) == 1) min <- rep(min, length(stages))
  if(length(max) == 1) max <- rep(max, length(stages))
  if(length(stages) != length(min) || length(stages) != length(max))
    stop("different lengths in stage, min and max")
  
  for(i in 1:length(stages)){
    if(!stages[i] %in% 1:x$nstage) stop("invalid stage input")
    f <- subset(x$module, x$module$stage == stages[i])$index
    f <- as.vector(outer(f, (1:x$npanel - 1) * x$nmodule, "+"))
    x$assembler <- ata_constraint(x$assembler, 1, min[i], max[i], forms=f, collapse=FALSE)
  }
  
  return(x)
}


#' @rdname mst
#' @description \code{mst_set_rdp} anchors the routing decision point (rdp) between adjacent modules
#' @param tol tolerance parameter
#' @import lpSolveAPI
#' @export
mst_set_rdp <- function(x, theta, indices, tol) {
  # validation
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(theta) != 1) stop("rdp is not a single theta point")
  if(length(indices) != 2 || abs(indices[1] - indices[2]) != 1) stop("modules are not adjacent") 
  
  info <- irt_stats(irt_model("3pl", theta=theta, items=x$pool), "info")
  info <- round(info[1,], 3)
  coef <- c(info, -1 * info)
  for(i in 1:x$npanel)
    x$assembler <- ata_constraint(x$assembler, coef, -tol, tol, forms=indices + (i - 1) * x$nmodule, collapse=TRUE)
  x
}


#' @rdname mst
#' @description \code{mst_module_mininfo} sets the minimum information for modules
#' @param mininfo the minimum information treshold
#' @export
mst_module_mininfo <- function(x, theta, mininfo, indices) {
  # validation
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(any(indices < 1 | indices > x$nmodule)) stop("invalid module index")

  coef <- irt_stats(irt_model("3pl", t=theta, items=x$pool), "info")
  coef <- round(coef, 3)
  for(i in 1:x$npanel)
    for(j in 1:length(theta))
      x$assembler <- ata_constraint(x$assembler, coef[j, ], min=mininfo, forms=indices + (i - 1) * x$nmodule)
  x
}


#' @rdname mst
#' @description \code{mst_assemble} assembles the mst
#' @param solver the ata solver
#' @param verbose the verbose parameter
#' @import lpSolveAPI
#' @export
mst_assemble <- function(x, solver="lpsolve", verbose="none", ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  x$assembler <- ata_solve(x$assembler, solver=solver, as.list=FALSE, verbose=verbose, ...)
  if(!is.null(x$assembler$items)) {
    items <- x$assembler$items
    items$panel <- ceiling(items$form / x$nmodule)
    items$index <- (items$form - 1) %% x$nmodule + 1
    items$stage <- x$module$stage[match(items$index, x$module$index)]
    items$module <- x$module$module[match(items$index, x$module$index)]
    x$items <- items
  }
  x
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
  if(is.null(opts$byroute)) opts$byroute <- FALSE
  if(is.null(opts$theta)) opts$theta <- round(seq(-3, 3, .1), 1)
  
  data <- NULL
  if(opts$byroute) {
    for(i in 1:x$nroute){
      r <- subset(x$route, x$route$index == i)[1:x$nstage]
      for(j in 1:x$npanel){
        items <- subset(x$items, x$items$index %in% r & x$items$panel == j)
        info <- irt_stats(irt_model("3pl", theta=opts$theta, items=items), "info", summary="people", fun=sum)
        data <- rbind(data, data.frame(t=opts$theta, info=info, panel=j, route=i))
      }
    }
    data$panel <- factor(paste("Panel", data$panel))
    data$route <- factor(data$route, levels=1:x$nroute, labels=apply(x$route[, 1:x$nstage], 1, paste, collapse="-"))
    g <- ggplot(data, aes_string(x="t", y="info", color="route")) + 
      geom_line() + xlab(expression(theta)) + ylab("Information") + 
      theme_bw() + theme(legend.key=element_blank()) +
      guides(color=guide_legend("Routes")) +
      facet_grid(. ~ panel)
  } else {
    for(i in unique(x$items$form)){
      items <- subset(x$items, x$items$form == i)
      info <- irt_stats(irt_model("3pl", theta=opts$theta, items=items), "info", summary="people", fun=sum)
      data <- rbind(data, data.frame(t=opts$theta, info=info, form=i, panel=items$panel[1], stage=items$stage[1], module=items$module[1]))
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
#' @description \code{mst_get_items} extracts items from results
#' @param panel the panel index
#' @param stage the stage index
#' @param module the module index
#' @export
mst_get_items <- function(x, panel, stage=NULL, module=NULL, route=NULL){
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
    r <- as.vector(subset(x$route, x$route$index == input$r)[, 1:x$nstage])
    items <- subset(items, items$index %in% r)
  }
  
  return(items)
}
