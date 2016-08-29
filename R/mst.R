#' Multistage Testing
#' @description \code{mst} creates a multistage (MST) object
#' @param pool an item pool (data.frame)
#' @param design a vector of number of modules in stages
#' @param npanel the number of panels
#' @details
#' The \code{mst} object contains \code{pool} (item pool), \code{route} (permissilbe routes), 
#' \code{lp} (test assembly object), \code{items} (assembled items after the lp is solve),
#' \code{nstage} (number of stage), \code{nmodule} (number of modules), \code{npanel} (number of panels),
#' \code{nroute} (number of routes), \code{module} (module information).
#' @return \code{mst} returns a \code{mst} object
#' @examples
#' # generate an item pool
#' pool <- gen.irt(1, 200)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' # 1-2-3 design with 2 parallel panels
#' x <- mst(pool, c(1, 2, 3), 2)
#' # remove route with radical changes from the 1-3-3 design
#' x <- mst.route(x, c(1, 2, 6), "-", print=TRUE)
#' x <- mst.route(x, c(1, 3, 4), "-", print=TRUE)
#' # maximize information at -1.0/0/1.0 for low-/mid-/high-ability routes
#' x <- mst.objective(x, -1.0, Inf, routes=1)
#' x <- mst.objective(x,  0.0, Inf, routes=2:3)
#' x <- mst.objective(x,  1.0, Inf, routes=4)
#' # all routes to have 10 items
#' x <- mst.constraint(x, "len", NA, 10, 10)
#' x <- mst.constraint(x, "content", 1, 3, 3)
#' # all least 2 items each stage
#' x <- mst.stagelength(x, 1, 2, 10)
#' x <- mst.stagelength(x, 2, 2, 10)
#' x <- mst.stagelength(x, 3, 2, 10)
#' # assemble
#' x <- mst.assemble(x)
#' # check content
#' for(p in 1:x$npanel){
#'  for(r in 1:x$nroute){
#'    cat("Panel #", p, " Route #", r, ", Content Area 1 has ",
#'    freq(mst.get(x, panel=p, route=r)$content, 1:3)$n[1], " items.\n", sep="")
#'  }
#' }
#' # check TIFs
#' plot(x, by.route=TRUE)
#' # run simulation
#' mst.sim(x,  1.5)
#' mst.sim(x,  0.5)
#' mst.sim(x, -0.5)
#' mst.sim(x, -1.5)
#' # Solve a big problem in sequence
#' rm("x")
#' pool <- gen.irt(1, 1000)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' x <- mst(pool, c(1, 2, 3), 3)
#' x <- mst.route(x, c(1, 2, 6), "-")
#' x <- mst.route(x, c(1, 3, 4), "-")
#' x <- mst.objective(x, -1.0, Inf, routes=1)
#' x <- mst.objective(x,  0.0, Inf, routes=2:3)
#' x <- mst.objective(x,  1.0, Inf, routes=4)
#' x <- mst.constraint(x, "len", NA, 10, 10)
#' x <- mst.constraint(x, "content", 1, 3, 3)
#' x <- mst.stagelength(x, 1, 2, 10)
#' x <- mst.stagelength(x, 2, 2, 10)
#' x <- mst.stagelength(x, 3, 2, 10)
#' # unlikely to be solved simulatanouesly within 1 minute
#' # invisible(mst.assemble(x, timeout=60))
#' # may be solved in sequence within 1 minute
#' x <- mst.assemble.sequence(x, timeout=60)
#' plot(x, by.route=TRUE)
#' @family mst
#' @export
#' @import lpSolveAPI
mst <- function(pool, design, npanel){
  # validate
  pool <- as.data.frame(pool)
  if(any(!c("a","b","c") %in% colnames(pool))) stop("cannot find a/b/c parameters in the pool.")
  if(any(design < 1)) stop("invalid design.")
  
  # common constants
  nstage <- length(design)
  nmodule <- sum(design)
  module <- list(n=design, lower=cumsum(c(0, design)[1:nstage])+1, upper=cumsum(c(design)))
  
  # routes
  route <- list()
  for(i in 1:nstage)
    route[[i]] <- module$lower[i]:module$upper[i]
  route <- expand.grid(route)
  colnames(route) <- paste("stage", 1:nstage, sep="")
  nroute <- nrow(route)

  # lp: default maxselection = 1
  lp <- ata(pool, npanel * nmodule)
  ata.maxselect(lp, 1)
  lp <- lp$lp
  
  x <- list(calls=list(), pool=pool, nitem=nrow(pool), npanel=npanel, nstage=nstage, nmodule=nmodule, nroute=nroute, module=module, route=route, lp=lp)
  class(x) <- "mst"
  return(x)
}

#' @rdname mst
#' @export
print.mst <- function(x, ...){
  cat("The", paste(x$module$n, collapse="-"), "MST design has", x$nstage, 
      "stages,", x$nmodule, "modules, and", x$nroute, "routes:\n")
  print(x$route)
  cat("\n")
  cat("Assemble", x$npanel, "panels from a pool of", x$nitem, "items.",
      "The LpSolve object is:\n")
  print(x$lp)
  cat("\n")
  if(is.null(x$items)) {
    cat("No assembly results is available now.")
  } else {
    cat("Assembled items are available in 'x$items'.")
    cat("The first and last 5 items in the results are:\n")
    print(x$items[1:5,])
    cat("...")
    print(x$items[1:5+nrow(x$items)-5,])
  }
}

#' @rdname mst
#' @description \code{mst.route} adds/removes a route to/from the mst object
#' @param x the mst object
#' @param route a vector of module index, and check \code{route} table in a mst object for routes
#' @param op "+" for adding a route and "-" for removing a route
#' @param print TRUE to print intermediate results
#' @family mst
#' @export
mst.route <- function(x, route, op, print=FALSE){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(length(route) != x$nstage || any(route > x$nmodule)) stop("invalid route.")
  index <- apply(x$route, 1, function(x){all(x == route)})
  if(op == "+"){
    if(any(index == TRUE)) stop("the route is already included.")
    x$route <- rbind(x$route, route)
  }else if(op == "-"){
    if(all(index == FALSE)) stop("the route is not included.")
    x$route <- x$route[!index,]
  }else{
    stop("cannot recognise the operation.")
  }
  x$nroute <- nrow(x$route)
  if(print) print(x$route)
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.info.obj} adds information objective functions
#' @param thetas a vector of theta points where information targets are set
#' @param target a single or vector of numeric values for targets, \code{Inf} for maximizing inforation
#' @param routes a vector of route index to add objective functions, or \code{NULL} for all routes
#' @family mst
#' @export
#' @import lpSolveAPI
mst.objective <- function(x, thetas, target, routes=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(routes)) routes <- 1:x$nroute else if(any(routes > x$nroute)) stop("invalid routes.")
  
  n <- length(thetas)
  if(length(target) == 1){
    if(target == Inf)
      lp.control(x$lp, sense="max")
    target <- rep(target, n)
  } else if(length(target) != n){
    stop("different lengths in thetas and targets")
  }
  
  nlp <- dim(x$lp)[2]
  set.objfn(x$lp, 1, nlp)
  information <- info(with(x$pool, irt(thetas, a, b, c)))
  information <- round(information, 3)
  
  for(p in 1:x$npanel){
    for(r in routes){
      index <- outer(1:x$nitem, (as.integer(x$route[r,])-1)*x$nitem, "+")
      index <- as.vector(index) + (p - 1) * x$nitem * x$nmodule
      for(i in 1:n){
        value <- rep(information[i,], x$nstage)
        if(target[i] == Inf){ # maximize information: X >= y
          add.constraint(x$lp, c(value, -1), ">=", 0, c(index, nlp))
        } else { # close on target: X + y >= target and X - y <= target
          add.constraint(x$lp, c(value,  1), ">=", target[i], c(index, nlp))
          add.constraint(x$lp, c(value, -1), "<=", target[i], c(index, nlp))
        }
      }
    }
  }
  
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.constraint} sets constraints
#' @param var the constrained variable. \code{"len"} for test length
#' @param level the constrained level for a categorical variable. \code{NA} or \code{NULL} for continuous variable
#' @param min the minimum value
#' @param max the maximum value
#' @family mst
#' @export
#' @import lpSolveAPI
mst.constraint <- function(x, var, level, min, max, routes=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(routes)) routes <- 1:x$nroute else if(any(routes > x$nroute)) stop("invalid routes.")
  if(var != "len" && !var %in% colnames(x$pool)) stop("cannot find constraint variable in the pool: ", var)
  if(min > max) stop("invalid min and max: ", min, " vs. ", max)
  
  # value
  if(var == "len")
    value <- rep(1.0, x$nitem)
  else if(is.null(level) || is.na(level))
    value <- round(x$pool[, var], 3)
  else
    value <- (x$pool[, var] == level) * 1.0
  
  value <- rep(value, x$nstage)
  nlp <- dim(x$lp)[2]

  for(p in 1:x$npanel){
    for(r in routes){
      index <- outer(1:x$nitem, (as.integer(x$route[r,])-1)*x$nitem, "+")
      index <- as.vector(index) + (p - 1) * x$nitem * x$nmodule
      if(min == max){
        add.constraint(x$lp, value, "=", max, index)
      } else {
        add.constraint(x$lp, value, ">=", min, index)
        add.constraint(x$lp, value, "<=", max, index)
      }
    }
  }
  
  x$calls <- c(x$calls, match.call())
  return(x)  
}

#' @rdname mst
#' @description \code{mst.stagelength} sets the min/max length for a stage
#' @param stage the stage being constrained
#' @family mst
#' @export
#' @import lpSolveAPI
mst.stagelength <- function(x, stage, min, max){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(stage < 1 || stage > x$nstage) stop("stage is out of bounds: ", stage)
  if(min > max) stop("invalid min and max: ", min, " vs. ", max)
  
  modules <- x$module$lower[stage]:x$module$upper[stage]
  value <- rep(1, x$nitem)
  for(p in 1:x$npanel){
    for(m in modules){
      index <- 1:x$nitem + (m - 1) * x$nitem + (p - 1) * x$nitem * x$nmodule
      if(min == max){
        add.constraint(x$lp, value, "=", max, index)
      } else {
        add.constraint(x$lp, value, ">=", min, index)
        add.constraint(x$lp, value, "<=", max, index)
      }
    }
  }
  
  x$calls <- c(x$calls, match.call())
  return(x)
}

#' @rdname mst
#' @description \code{mst.assemble} solves the LP object and assembles items
#' @param ... further arugments passed to lp.control or choose if summarize results \code{by.route}
#' @family mst
#' @export
#' @import lpSolveAPI
mst.assemble <- function(x, ...){
  # validate
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(is.null(x$lp) || nrow(x$lp) == 0) stop("invalid lp object.")
  
  nlp <- dim(x$lp)[2]
  nform <- x$npanel * x$nmodule
  lp.control(x$lp, mip.gap=c(.10, .10), epsint=.10, presolve="lindep", timeout=3*60)
  if(length(list(...)) != 0) lp.control(x$lp, ...)
  if(solve(x$lp) != 0){
    cat("No solution.")
  } else {
    index <- matrix(get.variables(x$lp)[-nlp], ncol=nform)
    rs <- NULL
    for(i in 1:nform){
      items <- x$pool[index[,i]==1,]
      if(nrow(items) == 0) next
      items$panel <- (i - 1) %/% x$nmodule + 1
      items$module <- (i - 1) %% x$nmodule + 1
      rs <- rbind(rs, items)
    }
    # # alternative way to extract results
    # rs <- get.variables(x$lp)
    # rs <- data.frame(name=dimnames(x$lp)[[2]], value=as.integer(rs))
    # rs <- rs[rs$name != "y",]
    # rs$form <- as.integer(gsub("^f([0-9]*)[.]x([0-9]*)$", "\\1", rs$name))
    # rs$item <- as.integer(gsub("^f([0-9]*)[.]x([0-9]*)$", "\\2", rs$name))
    # rs$panel <- (rs$form - 1) %/% x$nmodule + 1
    # rs$module <- (rs$form - 1) %% x$nmodule + 1
    # rs <- rs[rs$value == 1,]
    x$items <- rs
  }
  
  return(x)  
}

#' @rdname mst
#' @description \code{mst.summary} summarized an assembled mst object
#' @details 
#' In \code{mst.summary}, passing \code{by.route=TRUE} to summarize results by routes;
#' otherwise, by modules. A list, consisting of data and plot, is return.
#' @family mst
#' @export
#' @import ggplot2
mst.summary <- function(x, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  opts <- list(...)
  by.route <- ifelse(is.null(opts$by.route), FALSE, opts$by.route)
  if(is.null(x$items)) stop("items haven't been assembled yet.")
  
  t <- round(seq(-3, 3, .1), 1)
  nt <- length(t)
  nform <- ifelse(by.route, x$nroute, x$nmodule)
  rs <- NULL
  for(p in 1:x$npanel)
    for(f in 1:nform){
      if(by.route) items <- mst.get(x, panel=p, route.index=f)
      else items <- mst.get(x, panel=p, module.index=f)
      information <- with(items, info(irt(t, a, b, c), summary=1, fun=sum))
      value <- data.frame(theta=t, panel=p, form=ifelse(by.route, paste(x$route[f,],sep="",collapse="-"), f), info=information, stringsAsFactors=F)
      rs <- rbind(rs, value)
    }
  
    rs$form <- factor(rs$form)
    rs$panel <- factor(rs$panel)
    g <- ggplot(data=rs, aes_string(x="theta", y="info", color="form")) + 
      geom_line() + facet_wrap(~panel) +
      xlab(expression(theta)) + ylab("Information") + 
      scale_color_discrete(name=ifelse(by.route, "Routes", "Modules")) +
      coord_cartesian(xlim=c(-3,3)) + theme_bw() + theme(legend.key = element_blank())
  return(list(data=rs, plot=g))
}

#' @rdname mst
#' @details  
#' In \code{plot.mst}, use \code{by.route=TRUE} to plot TIFs for routes;
#' otherwise, TIFs for modules.
#' @export
#' @import ggplot2
plot.mst <- function(x, ...){
  mst.summary(x, ...)$plot
}

#' @rdname mst
#' @description \code{mst.get} retreives items from an assembled mst object
#' @param panel the panel index
#' @param module.index the module index
#' @param route.index the route index
#' @family mst
#' @export
mst.get <- function(x, panel, module.index=NULL, route.index=NULL){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  if(!is.null(module.index))
    index <- x$items$panel == panel & x$items$module == module.index
  if(!is.null(route.index))
    index <- x$items$panel == panel & x$items$module %in% x$route[route.index,]
  return(x$items[index,])
}

#' @rdname mst
#' @description \code{mst.sim} conducts a simulation for MST
#' @param theta the value of true theta
#' @param routing a list of routing cut scores or \code{NULL} for auto routing
#' @param estimator an ability estimation method.
#' @details 
#' In \code{mst.sim}, auto routing method routes a test taker to one of the permissible modules 
#' that has the largest information. Users can use any theta estimator from the Estimation module. 
#' See \code{estimate.theta.mle}, \code{estimate.theta.map}, and \code{estimate.theta.eap}
#' for example. When writing new estimator, make sure it takes arguments as \code{foo(u, a, b, c)}.
#' @family mst
#' @export
#' @importFrom stats rnorm
mst.sim <- function(x, theta, routing=NULL, estimator=estimate.theta.mle){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  
  rs <- list(panel=sample(1:x$npanel, 1), modules=rep(NA, x$nstage), true=theta, est=rnorm(1,0,.1), admins=NULL)
  panel <- x$items[x$items$panel == rs$panel, ]
  
  for(s in 1:x$nstage){
    if(s == 1) modules <- unique(x$route[,s]) 
    else modules <- x$route[x$route[,s-1]==rs$modules[s-1],s]
    
    # null for auto routing; list for manual routing
    if(is.null(routing)){
      tifs <- rep(NA, length(modules))
      for(m in 1:length(modules))
        tifs[m] <- with(panel[panel$module == modules[m],], info(irt(rs$est, a, b, c), summary=1, fun=sum))
      m <- modules[which.max(tifs)]
    }else{
      if(!is.list(routing) || length(routing) != x$nstage) stop("routing should be a list of n.stage long.")
      if(length(routing[[s]]) != x$module$n[s] - 1) stop("invalid number of routing cut score.")
      m <- sum(rs$est >= routing[[s]]) + 1
    }
    rs$modules[s] <- m      

    items <- panel[panel$module == m,]
    rsp <- with(items, gen.rsp(irt(rs$true, a, b, c)))$rsp[1,]
    items$rsp <- rsp
    rs$admins <- rbind(rs$admins, items)
    rs$est <- with(rs$admins, estimator(matrix(rsp,nrow=1), a, b, c)) 
  }

  return(rs)
}

#' @rdname mst
#' @description \code{mst.assemble.sequence} assembles panels in sequence rather than simultaneously.
#' @details 
#' Use \code{mst.assemble.sequence} when the LP problem is too large to be solved in \code{mst.assemble}.
#' The results will ievitably favor the panels assembled earlier than later. Try to run another round
#' of \code{mst.assemble} using those assembled items (perhapse, plus a bit more randomly selected items)
#' to reduce the unparallelism between panels.
#' @family mst  
#' @export
mst.assemble.sequence <- function(x, ...){
  if(class(x) != "mst") stop("not a 'mst' object: ", class(x))
  x0 <- x
  pool <- x0$pool
  pool$id <- 1:x0$nitem
  items <- rep(0, x0$nitem)
  rs <- NULL
  
  for(k in 1:x0$npanel){
    cat("Assembling panel #", k, "\n", sep="")
    x <- mst(pool[items < 1,], x0$module$n, 1)
    for(c in x0$calls)
      x <- eval(c)
    x <- mst.assemble(x, ...)
    if(!is.null(x$items)){
      x$items$panel <- k
      rs <- rbind(rs, x$items)
      items[x$items$id] <- items[x$items$id] + 1
    }
  }
  
  x0$items <- rs
  return(x0)
}

