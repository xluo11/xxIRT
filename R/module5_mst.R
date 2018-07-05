#' Computerized Multistage Testing (MST)
#' @name mst
#' @examples
#' \dontrun{
#' ## generate item pool
#' num_item <- 300
#' pool <- with(model_3pl_gendata(1, num_item), data.frame(a=a, b=b, c=c))
#' pool$id <- 1:num_item
#' pool$content <- sample(1:3, num_item, replace=TRUE)
#' pool$time <- round(rlnorm(num_item, 4, .3))
#' pool$group <- sort(sample(1:round(num_item/3), num_item, replace=TRUE))
#' 
#' ## ex. 1: 1-2-2 MST, 2 panels, topdown
#' ## 20 items in total and 10 items in content area 1 in each route
#' ## maximize info. at -1 and 1 for easy and hard routes
#' x <- mst(pool, "1-2-2", 2, 'topdown', len=20, max_use=1)
#' x <- mst_obj(x, theta=-1, indices=1:2)
#' x <- mst_obj(x, theta=1, indices=3:4)
#' x <- mst_constraint(x, "content", 10, 10, level=1)
#' x <- mst_assemble(x, timeout=5)
#' plot(x, byroute=TRUE)
#' for(p in 1:x$num_panel)
#'   for(r in 1:x$num_route) {
#'      route <- paste(x$route[r, 1:x$num_stage], collapse='-')
#'      count <- sum(mst_get_items(x, panel=p, route_index=r)$content==1)
#'      cat('panel=', p, ', route=', route, ': ', count, ' items in content area #1\n', sep='')
#'   }
#' 
#' ## ex. 2: 1-2-3 MST, 2 panels, bottomup,
#' ## remove two routes with large theta change: 1-2-6, 1-3-4 
#' ## 10 items in total and 4 items in content area 2 in each module
#' ## maximize info. at -1, 0 and 1 for easy, medium, and hard modules
#' x <- mst(pool, "1-2-3", 2, 'bottomup', len=10, max_use=1)
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_obj(x, theta= 0, indices=c(1, 5))
#' x <- mst_obj(x, theta=-1, indices=c(2, 4))
#' x <- mst_obj(x, theta= 1, indices=c(3, 6))
#' x <- mst_constraint(x, "content", 4, 4, level=2)
#' x <- mst_assemble(x, timeout=10) 
#' plot(x, byroute=FALSE)
#' for(p in 1:x$num_panel)
#'   for(m in 1:x$num_module){
#'     count <- sum(mst_get_items(x, panel=p, module=m)$content==2)
#'     cat('panel=', p, ', module=', m, ': ', count, ' items in content area #2\n', sep='')
#'   }
#'  
#' ## ex.3: same with ex.2 (w/o content constraints), but group-based  
#' x <- mst(pool, "1-2-3", 2, 'bottomup', len=12, max_use=1, group="group")
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_obj(x, theta= 0, indices=c(1, 5))
#' x <- mst_obj(x, theta=-1, indices=c(2, 4))
#' x <- mst_obj(x, theta= 1, indices=c(3, 6))
#' x <- mst_assemble(x, timeout=10)
#' plot(x, byroute=FALSE)
#' for(p in 1:x$num_panel)
#'   for(m in 1:x$num_module){
#'     items <- mst_get_items(x, panel=p, module=m)
#'     cat('panel=', p, ', module=', m, ': ', length(unique(items$id)), ' items from ', 
#'         length(unique(items$group)), ' groups\n', sep='')
#'   }
#'   
#' ## ex.4: 2 panels of 1-2-3 top-down design
#' ## 20 total items and 10 items in content area 3
#' ## 6+ items in stage 1 & 2
#' x <- mst(pool, "1-2-3", 2, "topdown", len=20, max_use=1)
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_obj(x, theta=-1, indices=1)
#' x <- mst_obj(x, theta=0, indices=2:3)
#' x <- mst_obj(x, theta=1, indices=4)
#' x <- mst_constraint(x, "content", 10, 10, level=3)
#' x <- mst_stage_length(x, 1:2, min=6)
#' x <- mst_assemble(x, timeout=15)
#' head(x$items)
#' plot(x, byroute=FALSE)
#' for(p in 1:x$num_panel)
#'   for(s in 1:x$num_stage){
#'     items <- mst_get_items(x, panel=p, stage=s)
#'     cat('panel=', p, ', stage=', s, ': ', length(unique(items$id)), ' items\n', sep='')
#'   }
#' 
#' ## ex.5: same with ex.4, but use RDP instead of stage length to control routing errors
#' x <- mst(pool, "1-2-3", 2, "topdown", len=20, max_use=1)
#' x <- mst_route(x, c(1, 2, 6), "-")
#' x <- mst_route(x, c(1, 3, 4), "-")
#' x <- mst_obj(x, theta=-1, indices=1)
#' x <- mst_obj(x, theta=0, indices=2:3)
#' x <- mst_obj(x, theta=1, indices=4)
#' x <- mst_constraint(x, "content", 10, 10, level=3)
#' x <- mst_rdp(x, 0, 2:3, .1)
#' x <- mst_module_mininfo(x, 0, 5, 2:3)
#' x <- mst_assemble(x, timeout=15)
#' plot(x, byroute=FALSE)
#' }
NULL

#' @rdname mst
#' @description \code{mst} creates a multistage (MST) object for assembly
#' @param pool the item pool (data.frame)
#' @param design the MST design (string): e.g., "1-3", "1-2-2", "1-2-3"
#' @param num_panel the number of panels (integer)
#' @param method the design method (string): 'topdown' or 'bottomup'
#' @param len the module/route length (integer)
#' @param max_use the maximum selection of items (integer)
#' @param group the grouping variable (string or vector)
#' @details
#' There are two methods for designing a MST. The bottom-up approach adds objectives 
#' and constraints on individual modules, whereas the topdown approach adds objectives
#' and constraints directly on routes.
#' @export
mst <- function(pool, design, num_panel, method=c('topdown', 'bottomup'), len=NULL, max_use=NULL, group=NULL, ...){
  method <- match.arg(method)
  design <- as.integer(unlist(strsplit(design, split="-")))
  num_stage <- length(design)
  num_module <- sum(design)
  opts <- list(...)
  if(is.null(opts$D)) opts$D <- 1.702
  
  # module-index map
  module <- NULL
  for(s in 1:num_stage)
    for(m in 1:design[s])
      module <- rbind(module, c(stage=s, module=m))
  module <- data.frame(module, index=1:nrow(module))
  
  # route-index map
  route <- list()
  for(i in 1:num_stage)
    route[[i]] <- module[module$stage == i, "index"]
  route <- expand.grid(route)
  colnames(route) <- paste("stage", 1:num_stage, sep="")
  route$index <- 1:nrow(route)
  num_route <- nrow(route)
  
  # ata
  x <- list(pool=pool, design=design, method=method, num_item=nrow(pool), num_panel=num_panel, 
            num_stage=num_stage, num_module=num_module, num_route=num_route, module=module, route=route, 
            ata=ata(pool, num_form=num_panel*num_module, group=group), opts=opts)
  class(x) <- "mst"

  # constraint: test length
  if(!is.null(len) && length(len) == 1) x <- mst_constraint(x, 1, len, len)
  if(!is.null(len) && length(len) == 2) x <- mst_constraint(x, 1, len[1], len[2])
  if(!is.null(len) && length(len) > 2) stop("the length argument is too long.")
  # constraint: max_use
  if(!is.null(max_use)) x$ata <- ata_item_use(x$ata, max=max_use)
  # constraint: minimum stage length
  x <- mst_stage_length(x, 1:num_stage, min=1)
  
  x
}

#' @rdname mst
#' @description \code{mst_route} adds/removes a route to/from the MST
#' @param x the MST object
#' @param route a MST route represented by a vector of module indices 
#' @param op "+" to add a route and "-" to remove a route
#' @export
mst_route <- function(x, route, op=c("+", "-")){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  op <- match.arg(op)
  index <- apply(x$route[, 1:x$num_stage], 1, function(r) all(r == route))
  
  if(op == "+") {
    if(any(index)) stop("the route already exists")
    if(!all(route %in% 1:x$num_module)) stop("invalid route: module index is out of bound.")
    x$route <- rbind(x$route, c(route, NA))
  } else if(op == "-") {
    if(!any(index)) stop("the route hasn't been added yet")
    x$route <- x$route[!index, ]
  }
  
  # reindex routes by stages
  index <- apply(x$route[, 1:x$num_stage], 1, function(r) sum(r * 10^(x$num_stage - 1:x$num_stage)))
  x$route <- x$route[order(index), ]
  x$route$index <- 1:nrow(x$route)
  x$num_route <- nrow(x$route)
  
  x
}

#' @rdname mst
#' @description \code{mst_get_indices} maps the input indices to the actual indices
mst_get_indices <- function(x, indices){
  if(x$method == 'topdown'){
    if(is.null(indices)) indices <- x$route[, 1:x$num_stage] else indices <- subset(x$route, x$route$index %in% indices)[, 1:x$num_stage]
  } else if(x$method == 'bottomup') {
    if(is.null(indices)) indices <- data.frame(module=1:x$num_module) else indices <- data.frame(module=indices)
  }
  indices
}

#' @rdname mst
#' @description \code{mst_obj} adds objective functions to the MST
#' @param theta a theta point or interval over which the TIF is optimized
#' @param indices the indices of the route (topdown) or the module (bottomup) where objectives are added
#' @param target the target values of the TIF objectives. \code{NULL} for maximization
#' @param flatten the parameter for getting a flat TIF
#' @export
mst_obj <- function(x, theta, indices=NULL, target=NULL, flatten=NULL) {
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst_get_indices(x, indices)
  theta <- round(theta, 2)
  
  for(i in 1:x$num_panel) {
    for(j in 1:nrow(indices)) {
      f <- unlist(indices[j, ]) + (i - 1) * x$num_module
      if(is.null(target) || is.na(target)) {
        x$ata <- ata_obj_relative(x$ata, theta, mode="max", flatten=flatten, forms=f, collapse=TRUE)
      } else {
        x$ata <- ata_obj_absolute(x$ata, theta, target=target, forms=f, collapse=TRUE)
      }      
    }
  }
  
  x
}

#' @rdname mst
#' @description \code{mst_constraint} adds constraints to the MST
#' @param coef the coefficients of the constraint
#' @param level the constrained level, \code{NA} for quantitative variable
#' @param min the lower bound of the constraint
#' @param max the upper bound of the constraint
#' @export
mst_constraint <- function(x, coef, min=NA, max=NA, level=NULL, indices=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  indices <- mst_get_indices(x, indices)
  
  for(i in 1:x$num_panel){
    for(j in 1:nrow(indices)){
      f <- unlist(indices[j,] + (i - 1) * x$num_module)
      x$ata <- ata_constraint(x$ata, coef, min, max, level, forms=f, collapse=TRUE)
    }
  }
  
  x
}


#' @rdname mst
#' @description \code{mst_stage_length} sets length limits on stages
#' @param stages the stage indices
#' @export
mst_stage_length <- function(x, stages, min=NA, max=NA){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(min) == 1) min <- rep(min, length(stages))
  if(length(max) == 1) max <- rep(max, length(stages))
  if(length(stages) != length(min) || length(stages) != length(max))
    stop("different lengths in stage, min and max")
  
  for(i in 1:length(stages)){
    if(!stages[i] %in% 1:x$num_stage) stop("invalid stage input")
    f <- subset(x$module, x$module$stage == stages[i])$index
    f <- as.vector(outer(f, (1:x$num_panel - 1) * x$num_module, "+"))
    x$ata <- ata_constraint(x$ata, 1, min[i], max[i], forms=f, collapse=FALSE)
  }
  
  x
}


#' @rdname mst
#' @description \code{mst_rdp} anchors the routing decision point (rdp) between adjacent modules
#' @param tol tolerance parameter (numeric)
#' @importFrom stats aggregate
#' @export
mst_rdp <- function(x, theta, indices, tol) {
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(theta) != 1) stop("rdp is not a single theta point")
  if(length(indices) != 2 || abs(indices[1] - indices[2]) != 1) stop("modules are not adjacent") 
  
  info <- round(aggregate(model_3pl_info(theta, x$pool$a, x$pool$b, x$pool$c, D=x$opts$D)[1, ], by=list(group=x$ata$group), sum)[, 2], 2)
  coef <- c(info, -1 * info)
  for(i in 1:x$num_panel)
    x$ata <- ata_constraint(x$ata, coef, -tol, tol, forms=indices + (i - 1) * x$num_module, collapse=TRUE)
  
  x
}


#' @rdname mst
#' @description \code{mst_module_mininfo} sets the minimum information for modules
#' @param mininfo the minimum information threshold
#' @importFrom stats aggregate
#' @export
mst_module_mininfo <- function(x, theta, mininfo, indices) {
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(any(indices < 1 | indices > x$num_module)) stop("invalid module index")
  if(length(theta) != 1) stop("set min info at one theta point at each time")

  coef <- round(aggregate(model_3pl_info(theta, x$pool$a, x$pool$b, x$pool$c, D=x$opts$D)[1,], by=list(gorup=x$ata$group), sum)[, 2], 2)
  for(i in 1:x$num_panel)
    x$ata <- ata_constraint(x$ata, coef, min=mininfo, forms=indices + (i - 1) * x$num_module)

  x
}


#' @rdname mst
#' @description \code{mst_assemble} assembles the mst
#' @export
mst_assemble <- function(x, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  x$ata <- ata_solve(x$ata, as.list=FALSE, ...)
  
  if(!is.null(x$ata$items)) {
    items <- x$ata$items
    items$module <- (items$form - 1) %% x$num_module + 1
    items$panel <- ceiling(items$form / x$num_module)
    items$stage <- x$module$stage[match(items$module, x$module$index)]
    items$form <- NULL
    x$items <- items
  }
  x
}


#' @rdname mst
#' @param ... further arguments
#' @export
print.mst <- function(x, ...){
  cat("The MST design has", x$num_stage, "stages,", x$num_module, "modules, and", x$num_route, "routes:\n")
  cat("route map:\n")
  print(x$route)
  cat("\nAssembled forms:\n")
  items <- x$items
  if(!is.data.frame(x$items)) items <- Reduce(rbind, items, NULL)
  if(nrow(items) > 10){
    print(items[1:5, ])
    cat("...\n")
    print(items[-4:0 + nrow(items),])
  } else {
    print(items)
  }
  cat("See more results in 'items'.")
  invisible(x)
}


#' @rdname mst
#' @details 
#' \code{plot.mst} draws module information functions when \code{byroute=FALSE}
#' and route information functions when \code{byroute=TRUE}
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
    for(i in 1:x$num_route){
      r <- subset(x$route, x$route$index == i)[1:x$num_stage]
      for(j in 1:x$num_panel){
        items <- subset(x$items, (x$items$module %in% r) & (x$items$panel == j))
        info <- rowSums(model_3pl_info(opts$theta, items$a, items$b, items$c, D=x$opts$D))
        data <- rbind(data, data.frame(t=opts$theta, info=info, panel=j, route=i))
      }
    }
    data$panel <- factor(paste("Panel", data$panel))
    data$route <- factor(data$route, levels=1:x$num_route, labels=apply(x$route[, 1:x$num_stage], 1, paste, collapse="-"))
    g <- ggplot(data, aes_string(x="t", y="info", color="route")) + 
      geom_line() + xlab(expression(theta)) + ylab("Information") + 
      theme_bw() + theme(legend.key=element_blank()) +
      guides(color=guide_legend("Routes")) +
      facet_grid(. ~ panel)
  } else {
    for(i in 1:x$num_panel){
      for(j in 1:x$num_module){
        items <- subset(x$items, x$items$panel == i & x$items$module == j)
        info <- rowSums(model_3pl_info(opts$theta, items$a, items$b, items$c, D=x$opts$D))
        data <- rbind(data, data.frame(t=opts$theta, info=info, panel=items$panel[1], stage=items$stage[1], module=items$module[1]))
      }
    }
    data$panel <- factor(paste("Panel", data$panel))
    data$stage <- factor(paste("Stage", data$stage))
    data$module <- factor(paste("Module", data$module))
    g <- ggplot(data, aes_string(x="t", y="info", color="module")) + 
      geom_line() + xlab(expression(theta)) + ylab("Information") + 
      theme_bw() + theme(legend.key=element_blank()) +
      guides(color=guide_legend("Modules")) +
      facet_grid(panel ~ stage)
  }
  
  g
}


#' @rdname mst
#' @description \code{mst_get_items} extracts items from the assembly results
#' @param panel the panel indices (numeric vector)
#' @param stage the stage indices (numeric vector)
#' @param module the module indices (numeric vector)
#' @param route_index the route indices (integer)
#' @export
mst_get_items <- function(x, panel=NULL, stage=NULL, module=NULL, route=NULL, route_index=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$items)) stop('the mst has not been assembled yet.')
  
  input <- list(p=panel, s=stage, m=module, r=route, r_ix=route_index)
  if(!is.null(panel))
    items <- subset(x$items, x$items$panel %in% input$p)
  if(!is.null(stage))
    items <- subset(items, items$stage %in% input$s)
  if(!is.null(module))
    items <- subset(items, items$module %in% input$m)
  if(!is.null(route))
    items <- subset(items, items$module %in% input$r)
  if(!is.null(route_index))
    items <- subset(items, items$module %in% unlist(x$route[x$route$index == 1, 1:x$num_stage]))
  
  items
}
