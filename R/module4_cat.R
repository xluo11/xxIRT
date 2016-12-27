#' Computerized Adaptive Testing (CAT) Simulation
#' @description \code{cat.sim} simulates CAT sessions with user-defined algorithms
#' @param theta.true the true theta parameter
#' @param pool a data frame of items used as the item pool
#' @param opts a list of option parameters (\code{min} and \code{max} are required)
#' @param debug \code{TRUE} for turning on the debug mode
#' @param cat.select a function of the item selection rule
#' @param cat.estimate a function of the ability estimation rule
#' @param cat.stop the a function of the stopping rule
#' @return \code{cat.sim} returns a \code{cat} object (see details section)
#' @details 
#' All data (input and output) are combined into a list named \code{cat.data}, including
#' the options (\code{opts}), the true theta (\code{true}), the estiamted theta (\code{est}), 
#' the item pool (\code{pool}), the administered items (\code{items}), the statistics (\code{stats}),
#' the administration history (\code{admin}), the test length (\code{len}), the debuggin switch (\code{debug}).\cr
#' To write a new \code{cat.select} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the selected \code{item}, updated \code{pool}, and \code{output} for additional output. 
#' Retrieve the additional output using \code{output.select} from the returnig \code{cat} object. \cr
#' To write a new \code{cat.estimate} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the estimated \code{theta}, and \code{output} for additional output. 
#' Retrieve the additional output using \code{output.estimate} from the returnig \code{cat} object. \cr
#' To write a new \code{cat.stop} function, make sure it only takes \code{cat.data} as input and 
#' outputs a list with the boolean \code{stop} decision, and \code{output} for additional output. 
#' Retrieve the additional output using \code{output.stop} from the returnig \code{cat} object. \cr
#' @examples
#' \dontrun{
#' # generate an item pool
#' pool <- irt.model()$gen.data(1,200)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
#' # cat simulation: 10-30 items, default rules, stop criterion: se = .3
#' opts <- list(min=10, max=30, stop.se=.3)
#' x <- cat.sim(0.1, pool, opts)
#' x$admin
#' plot(x)
#' # cat simulation: 10-30 items, default rules, stop criterion: mi = .3
#' opts <- list(min=10, max=30, stop.mi=.8)
#' x <- cat.sim(0.1, pool, opts, debug=TRUE)
#' # cat simulation: 10-30 items, default rules, stop criterion: cut = -.5
#' opts <- list(min=10, max=30, stop.cut=-0.5)
#' x <- cat.sim(0.1, pool, opts, debug=TRUE)
#' # cat simulation: 10-30 items, default rules, stop criterion: se = .3, randomesque=10
#' opts <- list(min=10, max=30, stop.se=.3, randomesque=5)
#' x <- cat.sim(0.1, pool, opts)
#' x
#' plot(x)
#' # cat simulation: 10-30 items, c-cat selection rule, first 10 random
#' opts <- list(min=30, max=60, stop.cut=0, ccat.target=c(.5,.25,.25), ccat.random=10)
#' x <- cat.sim(0.1, pool, opts, cat.select=cat.select.ccat)
#' x
#' freq(x$admin$content, 1:3)
#' plot(x)
#' # cat simulation with shadow test
#' cons <- data.frame(name="content", level=1, min=10, max=10, stringsAsFactors=FALSE)
#' cons <- rbind(cons, c("content", 2, 10, 10))
#' cons <- rbind(cons, c("content", 3, 10, 10))
#' cons <- rbind(cons, c("time", NA, 55*30, 65*30))
#' opts <- list(min=30, max=30, stop.se=.03, shadow.constraints=cons)
#' x <- cat.sim(0.1, pool, opts, cat.select=cat.select.shadow, debug=TRUE)
#' x
#' freq(x$admin$content, 1:3)
#' sum(x$items$time)
#' plot(x)
#' # cat simulation using the projection-based stopping rule
#' cons <- data.frame(name="content", level=1, min=10, max=10, stringsAsFactors=FALSE)
#' cons <- rbind(cons, c("content", 2, 10, 10))
#' cons <- rbind(cons, c("content", 3, 10, 10))
#' opts <- list(min=10, max=30, projection.cut=0, projection.constraints=cons, 
#' projection.method="information")
#' x <- cat.sim(0.1, pool, opts, cat.stop=cat.stop.projection, debug=TRUE)
#' }
#' @importFrom stats runif
#' @export
cat.sim <- function(theta.true, pool, opts, cat.select=cat.select.default, cat.estimate=cat.estimate.default, cat.stop=cat.stop.default, debug=FALSE){
  # validate: min and max test length
  if(is.null(opts$min)) stop("minimal length is not found in options")
  if(is.null(opts$max)) stop("maximal length is not found in options")
  if(opts$min < 0 || opts$min > opts$max) stop("invalid min/max length values: ", opts$min, " -- ", opts$max)
  # validate: item pool
  if(is.null(pool)) stop("item pool is NULL")
  pool <- as.data.frame(pool, stringsAsFactors=TRUE)
  if(any(!c("a", "b", "c") %in% colnames(pool))) stop("cannot find a-, b-, or c-parameters in item pool.")
  if(nrow(pool) < opts$max) stop("insufficient items in item pool: ", nrow(pool))
  # entry point
  theta.est <- ifelse(is.null(opts$entry), 0, opts$entry)
  
  # simulation
  cat.data <- list(debug=debug, pool=pool, opts=opts, len=0, true=theta.true, est=theta.est, items=data.frame(), stats=matrix(nrow=opts$max, ncol=3, dimnames=list(NULL, c("u", "t", "se"))))
  
  # debugging
  if(cat.data$debug){
    cat("staring a cat simulation sessoin:\n")
    cat("the item pool has", nrow(cat.data$pool), "items\n")
    cat("minimal length is", cat.data$opts$min, "and maximal length is", cat.data$opts$max, "\n")
    cat("true ability is", round(cat.data$true, 2), "\n")
    cat("entry point (initial ability estimate) is", round(cat.data$est, 2), "\n")
  }
  
  for(i in 1:opts$max){
    # length
    cat.data$len <- i
    # select
    selection <- cat.select(cat.data)
    cat.data$pool <- selection$pool
    cat.data$items <- rbind(cat.data$items, selection$item)
    # administer
    p <- irt.stats(irt.model.3pl(data.frame(theta=cat.data$true), selection$item), "probability")[1,1]
    rsp <- (p >= runif(1)) * 1
    cat.data$stats[i, 1] <- rsp
    # estimate
    estimation <- cat.estimate(cat.data)
    cat.data$est <- cat.data$stats[i, 2] <- estimation$theta
    info <- irt.stats(irt.model.3pl(data.frame(theta=cat.data$est), cat.data$items[1:i,]), "information")
    cat.data$stats[i,3] <- 1 / sqrt(sum(info))
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
  
  # clean and report data
  cat.data$stats <- cat.data$stats[1:i,]
  cat.data$admin <- cbind(cat.data$stats, cat.data$items)
  class(cat.data) <- "cat"
  return(cat.data)
}

#' @rdname cat.sim
#' @param x a \code{cat} object
#' @param ... further arguments
#' @export
print.cat <- function(x, ...){
  if(class(x) != "cat") stop("put a cat object in the first argument.")
  cat("true=", round(x$true, 2), ", est.=", round(x$est, 2), ", se=", round(x$admin$se[x$len],2), ", p=", round(mean(x$admin$u),2), ", used ", x$len, " items (", sum(x$admin$u)," correct).\n\n", sep="")
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

#' @rdname cat.sim
#' @import ggplot2
#' @export
plot.cat <- function(x, ...){
  x$admin$lb <- x$admin$t - 1.96 * x$admin$se
  x$admin$ub <- x$admin$t + 1.96 * x$admin$se
  x$admin$pos <- 1:x$len
  x$admin$Response <- factor(x$admin$u, levels=c(0, 1), labels=c("Wrong", "Right"))
  ggplot(data=x$admin, aes_string(x="pos",y="t",color="Response")) + 
    geom_point(aes_string(size="se"), alpha=.5) + 
    geom_hline(yintercept=x$true, linetype=2, color="gray70") +
    geom_linerange(aes_string(ymin="lb",ymax="ub"), linetype=3) +
    coord_cartesian(ylim=c(-3,3), xlim=c(0, x$opts$max)) + scale_size(range=c(1, 3)) +
    xlab("Position") + ylab(expression(paste("Est. ", theta))) + 
    guides(size=F, alpha=F) + theme_bw() + theme(legend.key = element_blank())
}

#' @rdname cat.sim
#' @description \code{cat.select.randomesque} is a helper function for selecting an item from k most informative ones
#' @param theta the current theta estimate
#' @param randomesque the randomesque parameter
cat.select.randomesque <- function(theta, pool, randomesque){
  information <- irt.stats(irt.model.3pl(data.frame(theta=theta), pool), "information")[1,]
  randomesque <- ifelse(is.null(randomesque), 1, randomesque)
  randomesque <- min(randomesque, length(information))
  index <- order(information, decreasing=TRUE)[1:randomesque]
  if(length(index) > 1) index <- sample(index, 1)
  return(index)
}

#' @rdname cat.sim
#' @description \code{cat.select.default} selects the most informative item
#' @param cat.data a list of CAT data 
#' @details 
#' \code{cat.select.default} selects the most informative item for current theta.
#' When \code{randomesque} is set, it randomly select an item from the k most informative ones. 
#' @export
cat.select.default <- function(cat.data){
  pool <- cat.data$pool
  index <- cat.select.randomesque(cat.data$est, pool, cat.data$opts$randomesque)
  return(list(item=pool[index,], pool=pool[-index,]))
}

#' @rdname cat.sim
#' @description \code{cat.select.ccat} selects items under content-balancing constraint (see Kingsbury & Zara, 1989, 1991)
#' @details 
#' To use \code{cat.select.ccat}, set target (percentage)  using \code{ccat.target} and initial randomness using \code{ccat.random} in options.
#' @export
cat.select.ccat <- function(cat.data){
  target <- cat.data$opts$ccat.target
  if(is.null(target)) stop("the target of the c-cat selection algorithm is not found in options")
  if(!"content" %in% colnames(cat.data$pool)) stop("content is not found in item pool")
  random <- ifelse(is.null(cat.data$opts$ccat.random), 0, cat.data$opts$ccat.random)
  n.content <- length(target)
  n.curr <- cat.data$len - 1
  if(n.curr < random){
    nextdomain <- sample(1:n.content, 1)
  } else {
    if(nrow(cat.data$items) == 0){
      curr.content <- rep(0, n.content)
    } else {
      curr.content <- freq(cat.data$items$content, 1:n.content)$p
    }
    nextdomain <- which.max(target - curr.content)
  }
  pool <- cat.data$pool
  pool$temp.id <- 1:nrow(pool)
  pool <- subset(pool, pool$content == nextdomain)
  index <- cat.select.randomesque(cat.data$est, pool, cat.data$opts$randomesque)
  index <- pool$temp.id[index]
  return(list(item=cat.data$pool[index,], pool=cat.data$pool[-index,]))
}

#' @rdname cat.sim
#' @description \code{cat.select.shadow} implements the shadow test algorithm described in van der Linden (2010)
#' @details 
#' \code{cat.select.shadow}: pass all constraints as a data frame with columns as such \code{variable, level, min, max}
#' to \code{shadow.constraints} in \code{options}. 
#' @family cat
#' @export
cat.select.shadow <- function(cat.data){
  cons <- cat.data$opts$shadow.constraints
  if(is.null(cons)) stop("The constraints of shadow-test selection algorithm is not found in options")
  
  pool <- cat.data$pool
  pool$temp.id <- 1:nrow(pool)
  items <- cat.data$items
  n.curr <- cat.data$len - 1
  len <- c(cat.data$opts$min - n.curr, cat.data$opts$max - n.curr)
  
  if(cat.data$debug)
    cat("\nShadow test selection algorithm: select", paste(len,collapse="--"), 
        "items to maximize information at", round(cat.data$est, 2), "\n")
  
  x <- ata(pool, nform=1, len=len, maxselect=1)
  x <- ata.obj.relative(x, cat.data$est, "max")
  for(i in 1:nrow(cons)){
    con <- unlist(cons[i,])
    con <- list(name=con[1], min=as.numeric(con[3]), max=as.numeric(con[4]), 
                level=ifelse(con[2]=="NA", NA, as.numeric(con[2])))
    if(nrow(items) == 0){
      con$curr <- 0
    } else {
      if(is.na(con$level)) {
        con$curr <- sum(items[, con$name])
      } else {
        con$curr <- sum(items[, con$name] == con$level)
      }
    }
    con$min <- con$min - con$curr
    con$min <- ifelse(con$min < 0, 0 , con$min)
    con$max <- con$max - con$curr
    con$max <- ifelse(con$max < 0, 0 , con$max)
    x <- ata.constraint(x, con$name, con$min, con$max, level=con$level)

    if(cat.data$debug) 
      cat("subject to: ", con$name, ", level = ",con$level, ", current = ", 
          con$curr, ", ~ [", con$min, ", ", con$max, "]\n", sep="")
  }
  x <- ata.solve(x)
  
  if(is.null(x$result)) stop("no solutions.")
  shadow <- ata.get.items(x, TRUE)[[1]]
  index <- cat.select.randomesque(cat.data$est, shadow, cat.data$opts$randomesque)
  index <- shadow$temp.id[index]
  
  output <- cat.data$output.select
  if(is.null(output)) output <- list()
  output[[cat.data$len]] <- shadow
  return(list(item=cat.data$pool[index,], pool=cat.data$pool[-index,], output.select=output))
}

#' @rdname cat.sim
#' @return \code{cat.estimate.default} estimates the ability using EAP (all correct/incorrect responses) or MLE (mixed responses)
#' @details 
#' \code{cat.estimate.default} estimates theta using EAP for a response vector of all 1's or 0's and MLE otherwise
#' @export
cat.estimate.default <- function(cat.data){
  n <- cat.data$len
  u <- matrix(cat.data$stats[1:n, "u"], nrow=1)
  score <- sum(u)
  method <- ifelse(score == 0 || score == n, "eap", "mle")
  t <- estimate.people(u, cat.data$items, model="3pl", method=method)
  t <- t$people[1,1]
  return(list(theta=t))
}

#' @rdname cat.sim
#' @return \code{cat.stop.default} evalutes whether to stop the CAT using the SE rule, MI rule, or CI rule
#' @details 
#' The \code{cat.stop.default} evaluates one of the three criteria after reaching minimum lenght:
#' (1) if \code{opts$stop.se} is set, evalute if the se reaches the threshold;
#' (2) if \code{opts$stop.mi} is set, evalute if all item reach the threshold;
#' (3) if \code{opts$stop.cut} is set, evalute if the 95% confidence interval contains the cut score
#' @export
cat.stop.default <- function(cat.data){
  n <- cat.data$len
  if(n < cat.data$opts$min){
    return(list(stop=FALSE))
  } else if(n >= cat.data$opts$max) {
    return(list(stop=TRUE))
  }
  se <- cat.data$stats[n, "se"]
  lb <- cat.data$est - 1.96 * se
  ub <- cat.data$est + 1.96 * se
  mi <- max(irt.stats(irt.model.3pl(data.frame(theta=cat.data$est), cat.data$pool), "information"))
  if(!is.null(cat.data$opts$stop.se)){
    stop <- (se <= cat.data$opts$stop.se)
  } else if (!is.null(cat.data$opts$stop.mi)) {
    stop <- (mi <= cat.data$opts$stop.mi)
  } else if(!is.null(cat.data$opts$stop.cut)) {
    stop <- (lb > cat.data$opts$stop.cut || ub < cat.data$opts$stop.cut)
  } else {
    stop("no stopping rule parameters is set in options.")
  }
  if(cat.data$debug) {
    cat("\ndefault termination: n = ", n, ", min/max length = (", cat.data$opts$min, 
        ", ", cat.data$opts$max, "), se = ", round(se, 2), ", mi = ", round(mi, 2), 
        ", lb = ", round(lb, 2), ", ub = ", round(ub, 2), "\n", sep="")
    }
  return(list(stop=stop))
}

#' @rdname cat.sim
#' @description \code{cat.stop.projection} is the projection-based stopping rule
#' @export
cat.stop.projection <- function(cat.data){
  if(cat.data$len < cat.data$opts$min){
    return(list(stop=FALSE))
  } else if(cat.data$len >= cat.data$opts$max) {
    return(list(stop=TRUE))
  }
  
  method <- cat.data$opts$projection.method
  if(is.null(method)) stop("The projection method is not found in options")
  cons <- cat.data$opts$projection.constraints
  if(is.null(cons)) stop("The constraints of the projection-based stopping rule is not found in options")
  cutscore <- cat.data$opts$projection.cut
  if(is.null(cutscore)) stop("cut score (projection.cut) is not set in options")
  pool <- cat.data$pool
  items <- cat.data$items
  rsp <- cat.data$stats[1:cat.data$len, "u"]
  len <- c(cat.data$opts$min - cat.data$len, cat.data$opts$max - cat.data$len)
  
  if(cat.data$debug) cat("\nProjection-based stopping rule: select", paste(len,collapse="--"), "items\n")
  for(i in 1:nrow(cons)){
    con <- unlist(cons[i,])
    con <- list(name=con[1], min=as.numeric(con[3]), max=as.numeric(con[4]), 
                level=ifelse(con[2]=="NA", NA, as.numeric(con[2])))
    if(nrow(items) == 0){
      con$curr <- 0
    } else {
      if(is.na(con$level)) {
        con$curr <- sum(items[, con$name])
      } else {
        con$curr <- sum(items[, con$name] == con$level)
      }
    }
    con$min <- con$min - con$curr
    con$min <- ifelse(con$min < 0, 0 , con$min)
    con$max <- con$max - con$curr
    con$max <- ifelse(con$max < 0, 0 , con$max)
    
    if(cat.data$debug) 
      cat("subject to: ", con$name, ", level = ",con$level, ", current = ", 
          con$curr, ", ~ [", con$min, ", ", con$max, "]\n", sep="")
  }
  
  if(method == "information"){
    x <- ata(pool, nform=1, len=len, maxselect=1)
    x <- ata.obj.relative(x, cat.data$est, "max")
    for(i in 1:nrow(cons)) x <- ata.constraint(x, con$name, con$min, con$max, level=con$level)
    x <- ata.solve(x)
    if(is.null(x$result)) stop("no solutions.")
    x <- ata.get.items(x, TRUE)[[1]]
    proj.items <- rbind(items, x)
    proj.response <- c(rsp, rep(0, nrow(x)))
    proj.theta.lb <- estimate.people(proj.response, proj.items, model="3pl", method="mle")$people[1,1]
    proj.response <- c(rsp, rep(1, nrow(x)))
    proj.theta.ub <- estimate.people(proj.response, proj.items, model="3pl", method="mle")$people[1,1]
  } else if(method == "difficulty"){
    x <- ata(pool, nform=1, len=len, maxselect=1)
    x <- ata.obj.absolute(x, x$pool$b, cat.data$est + 2 * cat.data$stats[cat.data$len, "se"])
    for(i in 1:nrow(cons)) x <- ata.constraint(x, con$name, con$min, con$max, level=con$level)
    x <- ata.solve(x)
    if(is.null(x$result)) stop("no solutions.")
    x <- ata.get.items(x, TRUE)[[1]]
    proj.items <- rbind(items, x)
    proj.response <- c(rsp, rep(1, nrow(x)))
    proj.theta.ub <- estimate.people(proj.response, proj.items, model="3pl", method="mle")$people[1,1]
    
    x <- ata(pool, nform=1, len=len, maxselect=1)
    x <- ata.obj.absolute(x, x$pool$b, cat.data$est - 2 * cat.data$stats[cat.data$len, "se"])
    for(i in 1:nrow(cons)) x <- ata.constraint(x, con$name, con$min, con$max, level=con$level)
    x <- ata.solve(x)
    if(is.null(x$result)) stop("no solutions.")
    x <- ata.get.items(x, TRUE)[[1]]
    proj.items <- rbind(items, x)
    proj.response <- c(rsp, rep(0, nrow(x)))
    proj.theta.lb <- estimate.people(proj.response, proj.items, model="3pl", method="mle")$people[1,1]
  }
  
  if(cat.data$debug) cat("the projected theta range is: [", round(proj.theta.lb, 2), ", ", round(proj.theta.ub, 2), "]\n", sep="")
  stop <- proj.theta.lb > cutscore || proj.theta.ub < cutscore
  return(list(stop=stop))
}
