#' Computerized Adaptive Testing
#' @description \code{cat.sim} simulates CAT sessions with user-defined algorithms
#' @param theta the true value of theta parameter
#' @param pool an item pool (data.frame) 
#' @param opts a list of option parameters: min and max are required.
#' @param debug TRUE to print debugging information
#' @param cat.select the selection rule
#' @param cat.estimate the estimation rule
#' @param cat.stop the stopping rule
#' @return \code{cat.sim} returns a \code{cat} object (see details section)
#' @details 
#' All inputs are combined into a list called \code{cat.data} which is passed to 
#' \code{cat.select}, \code{cat.estimate}, and \code{cat.stop}. Besides, \code{cat.data} 
#' contains outpus too. In details, \code{cat.data} includes \code{pool} (the item pool),
#' \code{opts} (options passed from arguments), \code{len} (test length), 
#' \code{true} (true theta), \code{est} (estimated theta), \code{items} administered items), 
#' \code{stats} (history of response, theta, se), and \code{cat.data$admin} (combining items and stats) \cr
#' When writing new selection, estimation, termination functions, use \code{cat.data} as the only argument 
#' and follow the structure strictly. For example, the selection function should return a list of 
#' selected item (item) and updated pool (pool). e.g., \code{foo(cat.data){...; return(list(item=item, pool=pool))}}.
#' The estimation function should return a list containing an estimated theta (theta). e.g., \code{foo(cat.data){...; return(list(theta=theta))}}.
#' The termination function should return a list containg a boolean value (stop). e.g., \code{foo(cat.data){...; return(list(stop=FALSE))}}.
#' If a \code{output} element is included in the returning list, it'll be added to \code{cat.data} as
#' \code{cat.data$output.select}, \code{cat.data$output.estimate}, \code{cat.data$output.stop} respectively.
#' @examples
#' # generate an item pool
#' pool <- gen.irt(1, 100)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' # cat simulation: 10-30 items
#' opts <- list(min=10, max=30, stop.se=.3)
#' x <- cat.sim(0.1, pool, opts)
#' x
#' plot(x)
#' # cat simulation with randomesque
#' opts <- list(min=10, max=30, stop.se=.3, select.random=10)
#' x <- cat.sim(0.1, pool, opts)
#' plot(x)
#' # cat simulation with content balancing
#' opts <- list(min=10, max=30, stop.se=.3, ccat.target=c(.5,.3,.2), ccat.random=5)
#' x <- cat.sim(0.1, pool, opts, cat.select=cat.select.ccat)
#' freq(x$items$content, 1:3)
#' @family cat
#' @export
#' @importFrom stats runif
cat.sim <- function(theta, pool, opts, cat.select=cat.select.default, cat.estimate=cat.estimate.default, cat.stop=cat.stop.default, debug=FALSE){
  # validate min and max test length
  if(is.null(opts$min)) stop("min length is not set in options.")
  if(is.null(opts$max)) stop("max length is not set in options.")
  if(opts$min < 0 || opts$min > opts$max) stop("invalid min/max length values: ", opts$min, " -- ", opts$max)
  # validate item pool
  if(is.null(pool)) stop("item pool is null.")
  pool <- as.data.frame(pool, stringsAsFactors=TRUE)
  if(any(!c("a", "b", "c") %in% colnames(pool))) stop("cannot find a/b/c parameters in item pool.")
  if(nrow(pool) < opts$max) stop("insufficient items in item pool: ", nrow(pool))
  # entry point
  t <- ifelse(is.null(opts$entry), 0, opts$entry)
  
  # debugging
  if(debug){
    cat("staring a cat simulation sessoin:\n")
    cat("pool has", nrow(pool), "items\n")
    cat("min length is", opts$min, "and maximum length is", opts$max, "\n")
    cat("entry point is", t, "\n")
    cat("true ability is", round(theta, 2), "\n")
  }

  # combine data
  cat.data <- list(pool=pool, opts=opts, len=0, true=theta, est=t, items=data.frame(), stats=matrix(nrow=opts$max, ncol=3, dimnames=list(NULL, c("u", "t", "se"))))
  for(i in 1:opts$max){
    # length
    cat.data$len <- i
    if(debug) cat("position #", i, ":", sep="")
    # select
    selection <- cat.select(cat.data)
    cat.data$pool <- selection$pool
    cat.data$items <- rbind(cat.data$items, selection$item)
    if(!is.null(selection$output)) cat.data$output.select <- selection$output
    if(debug) cat("select item", paste(c("a","b","c"), "=", round(selection$item[c("a","b","c")],2), collapse=",", sep=""), ",")
    # administer
    p <- with(selection$item, prob(irt(cat.data$true, a, b, c))[1,1])
    rsp <- (p >= runif(1)) * 1
    cat.data$stats[i, 1] <- rsp
    if(debug) cat("p=", round(p,2),", response=", rsp, sep="")
    # estimate
    estimation <- cat.estimate(cat.data)
    cat.data$est <- cat.data$stats[i, 2] <- estimation$theta
    cat.data$stats[i,3] <- 1 / sqrt(sum(info(irt(cat.data$est, cat.data$items[1:i,"a"], cat.data$items[1:i,"b"], cat.data$items[1:i,"c"]))))
    if(!is.null(estimation$output)) cat.data$output.estimate <- estimation$output
    if(debug) cat("est.=", round(estimation$theta, 2), sep="", "\n")
    # stopping
    termination <- cat.stop(cat.data)
    if(!is.null(termination$output)) cat.data$output.stop <- termination$output
    if(termination$stop) break
  }
  
  # clean and report data
  cat.data$stats <- cat.data$stats[1:i,]
  cat.data$admin <- cbind(cat.data$stats, cat.data$items)
  class(cat.data) <- "cat"
  cat.data
}

#' @rdname cat.sim
#' @param cat.data a list of CAT inputs and outputs (see details) 
#' @return \code{cat.select.default} returns a list with \code{item} (selected item), \code{pool} (updated pool), \code{output} (optional output) elements.
#' @details 
#' \code{cat.select.default} randomly selects an item from the k (set in options using keyword random or 5 by default) most informative items. 
#' @family cat
#' @export
cat.select.default <- function(cat.data){
  pool <- cat.data$pool
  information <- info(irt(cat.data$est, pool$a, pool$b, pool$c))[1,]
  randomesque <- ifelse(is.null(cat.data$opts$select.random), 1, cat.data$opts$select.random)
  randomesque <- min(randomesque, length(information))
  index <- order(information, decreasing=T)[1:randomesque]
  if(length(index) > 1) index <- sample(index, 1)
  return(list(item=pool[index,], pool=pool[-index,]))
}

#' @rdname cat.sim
#' @return \code{cat.estimate.default} returns a list with \code{theta} (theta estimate) and \code{output} (optional output) elements.
#' @details 
#' \code{cat.estimate.default} estimates theta using EAP for a response vector of all 1's or 0's and MLE otherwise
#' @family cat
#' @export
cat.estimate.default <- function(cat.data){
  n <- cat.data$len
  u <- matrix(cat.data$stats[1:n, 1], nrow=1)
  score <- sum(u)
  if(score == 0 || score == n)
    t <- estimate.theta.eap(u, cat.data$items$a, cat.data$items$b, cat.data$items$c)
  else
    t <- estimate.theta.mle(u, cat.data$items$a, cat.data$items$b, cat.data$items$c)
  return(list(theta=t))
}

#' @rdname cat.sim
#' @return \code{cat.stop.default} returns a list with \code{stop} (TRUE to stop and FALSE to continue) and \code{output} (optional output) elements.
#' @details 
#' \code{cat.stop.default} evaluates one of the three criteria after reaching minimum lenght:
#' (1) if \code{opts$stop.se} is set, then evalute if the se reaches the se threshold;
#' (2) if \code{opts$stop.mi} is set, then evalute if all item fails to reach the mi threshold;
#' (3) if \code{opts$stop.cut} is set, then evalute if the 95% confidence interval contains the cut score
#' @family cat
#' @export
cat.stop.default <- function(cat.data){
  n <- cat.data$len
  if(n < cat.data$opts$min)
    return(list(stop=FALSE))
  else if(n >= cat.data$opts$max)
    return(list(stop=TRUE))
  se <- cat.data$stats[n, "se"]
  lb <- cat.data$est - 1.96 * se
  ub <- cat.data$est + 1.96 * se
  mi <- max(info(irt(cat.data$est, cat.data$pool$a, cat.data$pool$b, cat.data$pool$c)))
  if(!is.null(cat.data$opts$stop.se))
    stop <- (se <= cat.data$opts$stop.se)
  else if (!is.null(cat.data$opts$stop.mi))
    stop <- (mi <= cat.data$opts$stop.mi)
  else if(!is.null(cat.data$opts$stop.cut))
    stop <- (lb > cat.data$opts$stop.cut | ub < cat.data$opts$stop.cut)
  else
    stop("no stopping rule parameters is set in options.")
  return(list(stop=stop))
}

#' @rdname cat.sim
#' @param x a \code{cat} object
#' @param ... further arguments
#' @family cat
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
#' @family cat
#' @export
#' @import ggplot2
plot.cat <- function(x, ...){
  x$admin$lb <- x$admin$t - 1.96 * x$admin$se
  x$admin$ub <- x$admin$t + 1.96 * x$admin$se
  x$admin$pos <- 1:x$len
  x$admin$Response <- factor(x$admin$u, levels=c(0, 1), labels=c("Wrong", "Right"))
  
  ggplot(data=x$admin, aes_string(x="pos",y="t",color="Response")) + 
    geom_point(aes_string(size="se"), alpha=.5) + 
    #geom_hline(yintercept=x$true, linetype=3) + 
    annotate("text", x=4, y=3, label=paste("true ability =", round(x$true, 2))) +
    annotate("text", x=4, y=2.5, label=paste("est. ability =", round(x$est, 2))) +
    geom_linerange(aes_string(ymin="lb",ymax="ub"), linetype=3) +
    coord_cartesian(ylim=c(-3,3), xlim=c(0, x$opts$max)) + 
    #scale_size(range=c(1, 4)) + 
    xlab("Position") + ylab("Estimated Ability") + 
    guides(size=F, alpha=F) + theme_bw() + theme(legend.key = element_blank())
}

#' @rdname cat.sim
#' @description \code{cat.select.ccat} implements the constrained item slection algorithm described in Kingsbury & Zara (1989, 1991)
#' @details 
#' \code{cat.select.ccat}: set target content percentage using \code{ccat.target} in \code{options}. 
#' Use \code{ccat.random} in \code{options} to allow the first k items to randomly draw next content domain.
#' @family cat
#' @export
cat.select.ccat <- function(cat.data){
  target <- cat.data$opts$ccat.target
  if(is.null(target)) stop("ccat.target is not found in the options.")
  random <- cat.data$opts$ccat.random
  if(is.null(random)) random <- 0
  
  n.content <- length(target)
  n.min <- cat.data$opts$min
  n.max <- cat.data$opts$max
  n.curr <- cat.data$len - 1
  
  if(n.curr < random){
    nextdomain <- sample(1:n.content, 1)
  } else {
    if(nrow(cat.data$items) == 0)
      curr.content <- rep(0, n.content)
    else
      curr.content <- freq(cat.data$items$content, 1:n.content)$p
    nextdomain <- which.max(target - curr.content)
  }
  
  pool <- cat.data$pool
  pool$temp.id <- 1:nrow(pool)
  pool <- pool[pool$content == nextdomain,]
  information <- with(pool, info(irt(cat.data$est, a, b, c))[1,])
  randomesque <- ifelse(is.null(cat.data$opts$select.random), 5, cat.data$opts$select.random)
  randomesque <- min(randomesque, length(information))
  index <- order(information, decreasing=TRUE)
  index <- index[1:randomesque]
  if(length(index) > 1) index <- sample(index, 1)
  index <- pool$temp.id[index]
  
  return(list(item=cat.data$pool[index,], pool=cat.data$pool[-index,]))
}