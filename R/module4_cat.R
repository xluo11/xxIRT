#' Computerized Adaptive Testing (CAT) Simulation
#' @description \code{cat_sim} simulates CAT sessions with user-defined algorithms
#' @param theta.true the true theta parameter
#' @param pool a data frame of items used as the item pool
#' @param opts a list of option parameters (\code{min} and \code{max} are required)
#' @param debug \code{TRUE} to turn on the debugging mode
#' @param cat.select the item selection rule
#' @param cat.estimate the theta estimation rule
#' @param cat.stop the stopping rule
#' @return \code{cat_sim} returns a \code{cat} object (see details section)
#' @details 
#' All data (input and output) are combined into a list named \code{cat.data}, which includes
#' options (\code{opts}), true theta (\code{true}), estiamted theta (\code{est}), 
#' item pool (\code{pool}), administered items (\code{items}), statistics (\code{stats}),
#' administration history (\code{admin}), test length (\code{len}), and debugging switch (\code{debug}).\cr
#' 
#' To write a new \code{cat.select} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the selected \code{item}, updated \code{pool}, and \code{output} for additional output (optional). 
#' Retrieve the additional output using \code{output.select} from the returnig \code{cat} object. \cr
#' 
#' To write a new \code{cat.estimate} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the estimated \code{theta}, and \code{output} for additional output (optional). 
#' Retrieve the additional output using \code{output.estimate} from the returnig \code{cat} object. \cr
#' 
#' To write a new \code{cat.stop} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the boolean \code{stop} decision, and \code{output} for additional output (optional). 
#' Retrieve the additional output using \code{output.stop} from the returnig \code{cat} object. \cr
#' @examples
#' ### generate a 200-item pool
#' pool <- irt_model("3pl")$gendata(1,200)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
#' 
#' ### ex. 1: 10-30 items
#' ### maximum information selection rule
#' ### standard error stopping rule (se=.3)
#' opts <- list(min=10, max=30, stop.se=.3)
#' x <- cat_sim(0.1, pool, opts)
#' x$admin
#' plot(x)
#' 
#' ### ex. 2: 10-30 items
#' ### maximum information selection rule
#' ### minimum information stopping rule (mi=.3)
#' opts <- list(min=10, max=30, stop.mi=.8)
#' x <- cat_sim(0.1, pool, opts)
#' x$admin
#' plot(x)
#' 
#' ### ex. 3: 10-30 items
#' ### maximum information selection rule
#' ### confidence interval stopping rule (cut=0)
#' opts <- list(min=10, max=30, stop.cut=0)
#' x <- cat_sim(0.1, pool, opts)
#' x$admin
#' plot(x)
#' 
#' ### ex. 4: 10-30 items
#' ### maximum information selection rules, randomesque = 5
#' ### standard error stopping rule (se=.3) 
#' opts <- list(min=10, max=30, stop.se=.3, randomesque=5)
#' x <- cat_sim(0.1, pool, opts)
#' x$admin
#' plot(x)
#' 
#' ### ex. 5: 10-30 items
#' ### c-cat selection rule, first 10 areas are random
#' ### confidence interval stopping rule
#' opts <- list(min=30, max=60, stop.cut=0, 
#'     ccat.target=c(.50,.25,.25), ccat.random=10)
#' x <- cat_sim(0.1, pool, opts, cat.select=cat_select_ccat)
#' x$admin
#' plot(x)
#' freq(x$admin$content, 1:3)
#' 
#' \dontrun{
#' ### ex. 6: 15 items
#' ### shadow test selection rule
#' ### content: [5, 5, 5] items in area 1--3
#' ### response time: avg. 55--65 seconds
#' cons <- data.frame(name="content", level=c(1,2,3), min=c(5, 5, 5), 
#'                    max=c(5, 5, 5), stringsAsFactors=FALSE)
#' cons <- rbind(cons, c("time", NA, 55*15, 65*15))
#' opts <- list(min=15, max=15, stop.se=.03, shadow.constraints=cons)
#' x <- cat_sim(0.1, pool, opts, cat.select=cat_select_shadow)
#' x$admin
#' plot(x)
#' freq(x$admin$content, 1:3)
#' mean(x$items$time)
#' 
#' ### ex. 7: 20-30 items
#' ### shadow selection rule
#' ### projection-based stopping rule
#' cons <- data.frame(name="content", level=c(1,2,3), min=c(10, 10, 10), 
#'                    max=c(10, 10, 10), stringsAsFactors=FALSE)
#' cons <- rbind(cons, c("time", NA, 55*30, 65*30))
#' opts <- list(min=20, max=30, projection.cut=0, projection.constraints=cons, 
#' projection.method="difficulty", shadow.constraints=cons)
#' x <- cat_sim(0.1, pool, opts, cat.select=cat_select_shadow, 
#' cat.stop=cat_stop_projection)
#' x$admin
#' plot(x)
#' }
#' @importFrom stats runif
#' @export
cat_sim <- function(theta.true, pool, opts, cat.select=cat_select_default, cat.estimate=cat_estimate_default, cat.stop=cat_stop_default, debug=FALSE){
  # Validate: min and max test length in opts
  if(is.null(opts$min)) stop("minimum length is not found in options")
  if(is.null(opts$max)) stop("maximum length is not found in options")
  if(opts$min < 0 || opts$min > opts$max) stop("invalid min/max length values: ", opts$min, " -- ", opts$max)
  # Validate: item pool
  if(is.null(pool)) stop("item pool is NULL")
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a", "b", "c") %in% colnames(pool))) stop("cannot find a-, b-, or c-parameters in item pool")
  if(nrow(pool) < opts$max) stop("insufficient items in item pool: ", nrow(pool))
  
  # Entry point
  theta.est <- ifelse(is.null(opts$entry), 0, opts$entry)
  
  # Data
  cat.data <- list(debug=debug, pool=pool, opts=opts, len=0, true=theta.true, est=theta.est, items=data.frame(), 
                   stats=matrix(nrow=opts$max, ncol=3, dimnames=list(NULL, c("u", "t", "se"))))
  
  # debugging
  if(cat.data$debug){
    cat("Starting a cat simulation sessoin:\n")
    cat("The item pool has", nrow(cat.data$pool), "items\n")
    cat("Minimum length is", cat.data$opts$min, "and maximum length is", cat.data$opts$max, "\n")
    cat("The true ability is", round(cat.data$true, 2), "\n")
    cat("The entry point (initial ability estimate) is", round(cat.data$est, 2), "\n")
  }
  
  # Simulation
  for(i in 1:opts$max){
    # length
    cat.data$len <- i
    
    # select
    selection <- cat.select(cat.data)
    cat.data$pool <- selection$pool
    cat.data$items <- rbind(cat.data$items, selection$item)
    
    # administer
    p <- irt_stats(irt_model("3pl", theta=cat.data$true, items=selection$item), "prob")[1,1]
    rsp <- (p >= runif(1)) * 1
    cat.data$stats[i, 1] <- rsp
    
    # estimate
    estimation <- cat.estimate(cat.data)
    cat.data$est <- cat.data$stats[i, "t"] <- estimation$theta
    info <- irt_stats(irt_model("3pl", theta=cat.data$est, items=cat.data$items[1:i,]), "info")
    cat.data$stats[i, "se"] <- 1 / sqrt(sum(info))
    
    # stopping
    termination <- cat.stop(cat.data)
    
    # optional output
    if(!is.null(selection$output)) cat.data$output.select <- selection$output
    if(!is.null(estimation$output)) cat.data$output.estimate <- estimation$output
    if(!is.null(termination$output)) cat.data$output.stop <- termination$output
    
    # debug
    if(cat.data$debug) {
      cat("\nposition", i, "\n")
      cat("selected item:", paste(c("a","b","c"), "=", round(selection$item[c("a","b","c")],2), collapse=", "), "\n")
      cat("the item pool has", nrow(selection$pool), "items\n")
      cat("probability for the selected item is", round(p, 2), "and the simulated response is", rsp, "\n")
      cat("estimated theta is", round(estimation$theta, 2), "and se is", round(cat.data$stats[i,"se"], 2), "\n")
      cat("termination decision is", termination$stop, "\n")
    }
    
    if(termination$stop) break
  }
  
  # Clean up
  cat.data$stats <- cat.data$stats[1:i, ]
  cat.data$admin <- cbind(pos=1:i, cat.data$stats, cat.data$items)
  class(cat.data) <- "cat"
  return(cat.data)
}


#' @rdname cat_sim
#' @param x a \code{cat} object
#' @param ... further arguments
#' @export
print.cat <- function(x, ...){
  if(class(x) != "cat") stop("Not a 'cat' object.")
  
  cat("true=", round(x$true, 2), ", est.=", round(x$est, 2), ", se=", round(x$admin$se[x$len],2), 
      ", p=", round(mean(x$admin$u),2), ", used ", x$len, " items (", sum(x$admin$u)," correct).\n\n", sep="")
  if(x$len <= 10) {
    cat("Belows is a history of the CAT\n")
    print(round(x$admin, 2))
  } else {
    cat("Belows is a history of the first and last 5 items of the CAT\n")
    print(round(x$admin[1:5,], 2))
    cat("...\n")
    print(round(x$admin[1:5+x$len-5,], 2))
  }
}


#' @rdname cat_sim
#' @import ggplot2
#' @export
plot.cat <- function(x, ...){
  x$admin$lb <- x$admin$t - 1.96 * x$admin$se
  x$admin$ub <- x$admin$t + 1.96 * x$admin$se
  x$admin$Response <- factor(x$admin$u, levels=c(0, 1), labels=c("Wrong", "Right"))
  ggplot(data=x$admin, aes_string(x="pos",y="t",color="Response")) + 
    geom_point(aes_string(size="se"), alpha=.6) + 
    geom_hline(yintercept=x$true, linetype=2, color="gray70") +
    geom_linerange(aes_string(ymin="lb",ymax="ub"), linetype=3) +
    coord_cartesian(ylim=c(-3,3), xlim=c(0, x$opts$max)) + scale_size(range=c(1, 5)) +
    xlab("Position") + ylab(expression(paste("Est. ", theta))) + 
    guides(size=F, alpha=F) + theme_bw() + theme(legend.key = element_blank())
}


#' @rdname cat_sim
#' @description \code{cat_select_default} selects the most informative item
#' @param cat.data a list of CAT data 
#' @details 
#' \code{cat_select_default} selects the most informative item for current theta.
#' When \code{randomesque} is set, it randomly selects an item from the k most 
#' informative ones to control item exposure rate. 
#' @export
cat_select_default <- function(cat.data){
  pool <- cat.data$pool
  index <- cat_select_randomesque(cat.data$est, pool, cat.data$opts$randomesque)
  return(list(item=pool[index, ], pool=pool[-index, ]))
}


#' @rdname cat_sim
#' @description \code{cat_select_ccat} selects items under content-balancing constraint (see Kingsbury & Zara, 1989, 1991)
#' @details 
#' To use \code{cat_select_ccat}, set target (percentage)  using \code{ccat_target} and initial randomness using \code{ccat_random} in options.
#' @export
cat_select_ccat <- function(cat.data){
  target <- cat.data$opts$ccat.target
  if(is.null(target)) stop("the targeted percentage of content distribution is not found in options")
  if(!"content" %in% colnames(cat.data$pool)) stop("content is not found in item pool")
  random <- ifelse(is.null(cat.data$opts$ccat.random), 0, cat.data$opts$ccat.random)
  
  n.content <- length(target)
  n.curr <- cat.data$len - 1
  if(n.curr < random){
    nextdomain <- sample(1:n.content, 1)
    if(cat.data$debug) cat("random selection of content.\n")
  } else {
    if(nrow(cat.data$items) == 0){
      curr.content <- rep(0, n.content)
    } else {
      curr.content <- freq(cat.data$items$content, 1:n.content)$perc / 100
    }
    nextdomain <- which.max(target - curr.content)
    if(cat.data$debug) cat("current content:", paste(round(curr.content, 2), collapse=","), "\n")
  }
  
  if(cat.data$debug) cat("next domain is", nextdomain, "\n")
  
  pool <- cat.data$pool
  pool$temp.id <- 1:nrow(pool)
  pool <- subset(pool, pool$content == nextdomain)
  
  index <- cat_select_randomesque(cat.data$est, pool, cat.data$opts$randomesque)
  index <- pool$temp.id[index]
  return(list(item=cat.data$pool[index, ], pool=cat.data$pool[-index, ]))
}


#' @rdname cat_sim
#' @description \code{cat_select_shadow} implements the shadow test algorithm described in van der Linden (2010)
#' @details 
#' To use \code{cat_select_shadow}, pass in a data frame of constraints with 4 columns to 
#' \code{opts$shadow.constraints}: \code{name}, \code{level}, \code{min}, and \code{max}. 
#' @export
cat_select_shadow <- function(cat.data){
  cons <- cat.data$opts$shadow.constraints
  if(is.null(cons)) stop("The constraints of shadow-test selection algorithm is not found in options")
  if(!all(colnames(cons) %in% c("name", "level", "min", "max"))) stop("make sure the column names of the contraints data frame are 'name', 'level', 'min', 'max'")
  if(is.factor(cons$name)) cons$name <- levels(cons$name)[cons$name]
  if(is.factor(cons$level)) cons$level <- as.numeric(levels(cons$level)[cons$level])
  cons$min <- as.numeric(cons$min)
  cons$max <- as.numeric(cons$max)
  
  pool <- cat.data$pool
  pool$temp.id <- 1:nrow(pool)
  items <- cat.data$items
  n.curr <- cat.data$len - 1
  len <- c(cat.data$opts$min - n.curr, cat.data$opts$max - n.curr)
  
  if(cat.data$debug)
    cat("\nShadow test selection algorithm: select", paste(len,collapse="--"), 
        "items to maximize information at", round(cat.data$est, 2), "\n")
  
  # compute lower- and upper-bounds
  x <- ata(pool, nforms=1, len=len, maxselect=1)
  x <- ata_obj_relative(x, cat.data$est, "max")
  cons$curr <- apply(cons, 1, function(x) {
    if(nrow(cat.data$items) == 0)
      return(0)
    if(is.na(x["level"]) || x["level"] == "NA")
      return(sum(cat.data$items[, x["name"]]))
    return(sum(cat.data$items[, x["name"]] == x["level"]))
  })
  cons$min <- cons$min - cons$curr
  cons$min <- ifelse(cons$min < 0, 0, cons$min)
  cons$max <- cons$max - cons$curr
  cons$max <- ifelse(cons$max < 0, 0, cons$max)
  
  # add constraints
  for(i in 1:nrow(cons)) {
    x <- ata_constraint(x, coef=cons$name[i], min=cons$min[i], max=cons$max[i], level=cons$level[i])
    if(cat.data$debug) 
      cat("subject to: ", cons$name[i], ", level = ", cons$level[i], ", current = ", cons$curr[i], ", ~ [", cons$min[i], ", ", cons$max[i], "]\n", sep="")
  }
  x <- ata_solve(x, "lpsolve", verbose="none")
  
  # select an item from the shadow test
  if(!is.null(x$items)) {
    shadow <- x$items[[1]]
  } else {
    cat("No solution for shadow test at #", cat.data$len, "\n", sep="")
    shadow <- cat.data$pool
  }
  index <- cat_select_randomesque(cat.data$est, shadow, cat.data$opts$randomesque)
  index <- shadow$temp.id[index]
  
  output <- cat.data$output.select
  if(is.null(output)) output <- list()
  output[[cat.data$len]] <- shadow
  return(list(item=cat.data$pool[index, ], pool=cat.data$pool[-index, ], output.select=output))
}


#' @rdname cat_sim
#' @return \code{cat_estimate_default} estimates the ability using EAP (all correct/incorrect responses) or MLE (mixed responses)
#' @export
cat_estimate_default <- function(cat.data){
  n <- cat.data$len
  u <- matrix(cat.data$stats[1:n, "u"], nrow=1)
  score <- sum(u)
  method <- ifelse(score == 0 || score == n, "eap", "mle")
  t <- estimate_people(u, cat.data$items, model="3pl", method=method)
  t <- t$people[1,1]
  return(list(theta=t))
}


#' @rdname cat_sim
#' @return \code{cat_stop_default} evalutes whether to stop the CAT using the SE rule, MI rule, or CI rule
#' @details 
#' The \code{cat_stop_default} evaluates one of the three criteria after reaching minimum lenght:
#' (1) if \code{opts$stop.se} is set, evalute if the se reaches the threshold;
#' (2) if \code{opts$stop.mi} is set, evalute if all item reach the threshold;
#' (3) if \code{opts$stop.cut} is set, evalute if the 95% confidence interval contains the cut score
#' @export
cat_stop_default <- function(cat.data){
  n <- cat.data$len
  
  if(n < cat.data$opts$min){
    return(list(stop=FALSE))
  } else if(n >= cat.data$opts$max) {
    return(list(stop=TRUE))
  }
  
  se <- cat.data$stats[n, "se"]
  lb <- cat.data$est - 1.96 * se
  ub <- cat.data$est + 1.96 * se
  mi <- irt_stats(irt_model("3pl", theta=cat.data$est, items=cat.data$pool), "info")
  mi <- max(mi)
  if(!is.null(cat.data$opts$stop.se)){
    stop <- (se <= cat.data$opts$stop.se)
  } else if (!is.null(cat.data$opts$stop.mi)) {
    stop <- (mi <= cat.data$opts$stop.mi)
  } else if(!is.null(cat.data$opts$stop.cut)) {
    stop <- (lb > cat.data$opts$stop.cut || ub < cat.data$opts$stop.cut)
  } else {
    stop("no stopping rule parameters in options.")
  }
  
  if(cat.data$debug) cat("\ndefault termination: n = ", n, ", min/max length = (", 
                         cat.data$opts$min, ", ", cat.data$opts$max, "), se = ", 
                         round(se, 2), ", mi = ", round(mi, 2), ", lb = ", round(lb, 2), 
                         ", ub = ", round(ub, 2), "\n", sep="")
  return(list(stop=stop))
}


#' @rdname cat_sim
#' @description \code{cat_stop_projection} implements the projection-based stopping rule described in Luo et al (2016)
#' @details 
#' To use \code{cat_stop_projection}, pass in a data frame of constraints with 4 columns to
#' \code{opts$projection.constraints}: \code{name}, \code{level}, \code{min}, and \code{max}.
#' Also set a method in \code{opts$method} ('information' or 'difficulty') and a cut score in \code{opts$cut}.
#' @export
cat_stop_projection <- function(cat.data){
  method <- cat.data$opts$projection.method
  if(is.null(method)) stop("The projection method is not found in options")
  cutscore <- cat.data$opts$projection.cut
  if(is.null(cutscore)) stop("cut score (projection.cut) is not set in options")
  cons <- cat.data$opts$projection.constraints
  if(is.null(cons)) stop("The constraints of the projection-based stopping rule is not found in options")
  if(!all(colnames(cons) %in% c("name", "level", "min", "max"))) stop("make sure the column names of the contraints data frame are 'name', 'level', 'min', 'max'")
  if(is.factor(cons$name)) cons$name <- levels(cons$name)[cons$name]
  if(is.factor(cons$level)) cons$level <- as.numeric(levels(cons$level)[cons$level])
  cons$min <- as.numeric(cons$min)
  cons$max <- as.numeric(cons$max)
  
  if(cat.data$len < cat.data$opts$min){
    return(list(stop=FALSE))
  } else if(cat.data$len >= cat.data$opts$max) {
    return(list(stop=TRUE))
  }
  
  pool <- cat.data$pool
  n <- cat.data$len
  theta <- cat.data$est
  items <- cat.data$items
  se <- cat.data$stats[n, "se"]
  rsp <- cat.data$stats[1:n, "u"]
  len <- c(cat.data$opts$min - n, cat.data$opts$max - n)
  
  cons$curr <- apply(cons, 1, function(x) {
    if(nrow(items) == 0)
      return(0)
    if(is.na(x["level"]) || x["level"] == "NA")
      return(sum(items[, x["name"]]))
    return(sum(items[, x["name"]] == x["level"]))
  })
  cons$min <- cons$min - cons$curr
  cons$min <- ifelse(cons$min < 0, 0, cons$min)
  cons$max <- cons$max - cons$curr
  cons$max <- ifelse(cons$max < 0, 0, cons$max)
  
  if(cat.data$debug) {
    cat("\nProjection-based stopping rule: select", paste(len, collapse="--"), "items\n")
    apply(cons, 1, function(x) {
      cat("subject to: ", x["name"], ", level = ", x["level"], ", current = ", 
          x["curr"], ", ~ [", x["min"], ", ", x["max"], "]\n", sep="")
    })
  }
  
  if(method == "information"){
    x <- ata(pool, nforms=1, len=NULL, maxselect=1)
    x <- ata_constraint(x, 1, min=len[1], max=len[2])
    x <- ata_obj_relative(x, theta, "max")
    for(i in 1:nrow(cons)) 
      x <- ata_constraint(x, coef=cons$name[i], min=cons$min[i], max=cons$max[i], level=cons$level[i])
    x <- ata_solve(x, "lpsolve", verbose="none")
    if(is.null(x$items)) stop("no solutions.")
    items.ub <- items.lb <- x$items[[1]]
  } else if(method == "difficulty"){
    x <- ata(pool, nforms=1, len=NULL, maxselect=1)
    x <- ata_constraint(x, 1, min=len[1], max=len[2])
    x <- ata_obj_absolute(x, x$pool$b, theta + 2 * se)
    for(i in 1:nrow(cons)) 
      x <- ata_constraint(x, coef=cons$name[i], min=cons$min[i], max=cons$max[i], level=cons$level[i])
    x <- ata_solve(x, "lpsolve", verbose="none")
    if(is.null(x$items)) stop("no solutions.")
    items.ub <- x$items[[1]]

    x <- ata(pool, nforms=1, len=NULL, maxselect=1)
    x <- ata_constraint(x, 1, min=len[1], max=len[2])
    x <- ata_obj_absolute(x, x$pool$b, theta - 2 * se)
    for(i in 1:nrow(cons)) 
      x <- ata_constraint(x, coef=cons$name[i], min=cons$min[i], max=cons$max[i], level=cons$level[i])
    x <- ata_solve(x, "lpsolve", verbose="none")
    if(is.null(x$items)) stop("no solutions.")
    items.lb <- x$items[[1]]
  }  
  
  rsp.ub <- c(rsp, rep(1, nrow(items.ub)))
  items.ub <- rbind(items, items.ub[,colnames(items)])
  theta.ub <- estimate_people(rsp.ub, items.ub, method="mle")$people[1,1]
  rsp.lb <- c(rsp, rep(0, nrow(items.lb)))
  items.lb <- rbind(items, items.lb[,colnames(items)])
  theta.lb <- estimate_people(rsp.lb, items.lb, method="mle")$people[1,1]
  
  if(cat.data$debug) 
    cat("the projected theta range is: [", round(theta.lb, 2), 
        ", ", round(theta.ub, 2), "]\n", sep="")
  
  stop <- (theta.lb > cutscore || theta.ub < cutscore)
  return(list(stop=stop))
}
