#' Create IRT Models
#' @description Create a IRT model object via a common interface
#' @param model the name of IRT model
#' @param people a data frame of people parameters
#' @param items a data frame of item parameters
#' @param responses a data frame of responses
#' @param ... other arguments passed to the constructor
#' @examples
#' # create a 3PL model with given parameters
#' x <- model_3pl()$gendata(10, 5)
#' irt_model("3pl", x$people, x$items, x$responses)
#' # create a 3PL model with generated data
#' irt_model("3pl")$gendata(10, 5)
#' # generate data with Rasch items
#' irt_model("3pl")$gendata(10, 5, a.sd=0, c.alpha=0)
#' @export
irt_model <- function(model=c("3pl"), people=NULL, items=NULL, responses=NULL, ...){
  model <- match.arg(model)
  switch(tolower(model),
         "3pl" = model_3pl(people, items, responses, ...))
}


#' @rdname irt_model
#' @param x a \code{irt_model} object
#' @importFrom utils head
#' @export
print.irt.model <- function(x, ...){
  n.people <- ifelse(is.null(x$people), 0, nrow(x$people))
  n.items <- ifelse(is.null(x$items), 0, nrow(x$items))
  with.responses <- ifelse(is.null(x$responses), "without", "with")
  
  cat("a ", x$name, " model: ", n.people, " people, ", n.items, " items, ", with.responses, " responses.\n", sep="")
  cat("snapshot of people:\n")
  print(utils::head(x$people, n=10))
  cat("snapshot of items:\n")
  print(utils::head(x$items, n=6))
  cat("snapshot of responses:\n")
  print(x$responses[1:min(n.people, 10), 1:min(n.items, 6)])
  
  invisible(x)
}


#' Compute IRT Statistics
#' @description Compute probability, information, likelihood and loglikelihood via a common interface
#' @param x an IRT model object
#' @param stats the statistic to be computed
#' @param summary the summarization direction
#' @param fun the summarization function
#' @param ... other optional arguments
#' @details 
#' In \code{irt_stats}, use \code{stats="prob"} to compute probility, \code{stats="info"} to compute information,
#' \code{stats="lik"} to compute likelihood, and \code{stats="loglik"} to compute log-likelihood. 
#' Use \code{summary="people"} to summarize results over items and \code{summary="items"} to summarize results over people.\cr
#' @examples  
#' # Compute prob(ability), info(rmation), lik(elihood) and loglik(elihood)
#' x <- irt_model("3pl")$gendata(20, 5)
#' irt_stats(x, "prob")
#' irt_stats(x, "prob", "people", sum)
#' irt_stats(x, "info")
#' irt_stats(x, "info", "items", sum)
#' irt_stats(x, "lik")
#' irt_stats(x, "loglik")
#' @export
irt_stats <- function(x, stats=c("prob", "info", "lik", "loglik"), summary=NULL, fun=NULL, ...){
  if(!"irt.model" %in% class(x)) stop("data is not a irt.model object")
  stats <- match.arg(stats)
  value <- switch (stats, "prob" = x$P(x), "info" = x$I(x), "lik" = x$L(x), "loglik" = x$L(x, log=TRUE))
  if(!is.null(summary)){
    if(is.null(fun)) stop("No summary function")
    if(!summary %in% c("people", "items")) stop("invalid summary input. use 'people' or 'items'")
    value <- apply(value, ifelse(summary=="people", 1, 2), fun, ...)
  }
  return(value)
}


#' Subset IRT Model
#' @description Subset and sample from an IRT model object
#' @param x an IRT model object
#' @param people.index the indices of people to keep
#' @param items.index the indices of items to keep
#' @examples 
#' # subset
#' x <- irt_model("3pl")$gendata(10, 5)
#' irt_select(x, people.index=c(1,3,5))
#' irt_select(x, items.index=c(1,3,5))
#' @export
irt_select <- function(x, people.index=NULL, items.index=NULL){
  if(!"irt.model" %in% class(x)) stop("data is not a irt.model object")
  
  if(!is.null(people.index) && is.null(x$people)) stop("No people parameters to select")
  if(is.null(people.index)) people.index <- 1:nrow(x$people)
  people <- data.frame(theta=x$people$theta[people.index])
  
  if(!is.null(items.index) && is.null(x$items)) stop("No item parameters to select")
  if(is.null(items.index)) items.index <- 1:nrow(x$items)
  items <- x$items[items.index, ]
  
  if(!is.null(x$responses)) responses <- x$responses[people.index, items.index]
  
  return(irt_model(x$name, people, items, responses))
}


#' @rdname irt_select
#' @param n.people the nubmer of people to sample
#' @param n.items the number of items to sample
#' @examples
#' # sample wihtout replacement
#' irt_sample(x, n.people=3)
#' irt_sample(x, n.items=3)
#' # sample with replacement
#' irt_sample(x, n.people=30)
#' irt_sample(x, n.items=30)
#' @export
irt_sample <- function(x, n.people=NULL, n.items=NULL){
  if(!"irt.model" %in% class(x)) stop("data is not a irt.model object")
  
  if(!is.null(n.people) && is.null(x$people)) stop("No people parameters to sample")
  if(is.null(n.people)) n.people <- nrow(x$people)
  people.index <- sample(1:nrow(x$people), n.people, replace=(n.people > nrow(x$people)))
  
  if(!is.null(n.items) && is.null(x$items)) stop("No item parameters to sample")
  if(is.null(n.items)) n.items <- nrow(x$items)
  items.index <- sample(1:nrow(x$items), n.items, replace=(n.items > nrow(x$items)))

  return(irt_select(x, people.index, items.index))
}


#' Rescale 3PL parameters
#' @description Rescale parameters in a 3PL model
#' @param x an IRT model object
#' @param parameter the rescaling parameter
#' @param mean the mean of the new scale
#' @param sd the standard deviation of the new scale
#' @examples
#' # rescale theta
#' x <- irt_model("3pl")$gendata(20, 5)
#' c(mean(x$people$theta), sd(x$people$theta))
#' y <- irt_rescale_3pl(x, "theta", 0, 1)
#' c(mean(y$people$theta), sd(y$people$theta))
#' round(abs(irt_stats(x, "prob") - irt_stats(y, "prob")), 2)
#' # rescale b
#' x <- irt_model("3pl")$gendata(20, 5)
#' c(mean(x$items$b), sd(x$items$b))
#' y <- irt_rescale_3pl(x, "b", 0, 1)
#' c(mean(y$items$b), sd(y$items$b))
#' round(abs(irt_stats(x, "prob") - irt_stats(y, "prob")), 2)
#' @importFrom stats sd
#' @export
irt_rescale_3pl <- function(x, parameter=c("theta", "b"), mean=0, sd=1){
  if(!"model.3pl" %in% class(x)) stop("data is not a 3PL model")
  
  parameter <- match.arg(parameter)
  if(parameter == "theta"){
    mean0 <- mean(x$people$theta)
    sd0 <- stats::sd(x$people$theta)
  } else if(parameter == "b"){
    mean0 <- mean(x$items$b)
    sd0 <- stats::sd(x$items$b)
  } else {
    stop("invalid rescale parameter. use 'theta' or 'b'")
  }
  
  a <- sd / sd0
  b <- mean - a * mean0
  x$people$theta <- a * x$people$theta + b
  x$items$b <- a * x$items$b + b
  x$items$a <- x$items$a / a
  
  return(x)
}


#' @rdname irt_model
#' @details 
#' In \code{plot_irt_model}, use the \code{stats} argument to control what IRT statistics to draw.
#' Use \code{total} argument to control whether aggregate results over items/people or not.
#' @examples
#' # draw test/item characteristic curve
#' x <- irt_model("3pl")$gendata(20, 5)
#' plot(x, stats="prob")
#' plot(x, stats="prob", total=FALSE)
#' # draw test/iten information function
#' plot(x, stats="info")
#' plot(x, stats="info", total=FALSE)
#' # draw loglikelihood
#' plot(x, stats="loglik")
#' plot(x, stats="loglik", total=FALSE, theta=seq(-5, 5, .1))
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
plot.model.3pl <- function(x, ...){
  # Optional inputs: theta, total, stats
  opts <- list(...)
  if(is.null(opts$theta)) opts$theta <- round(seq(-3, 3, .1), 1)
  if(is.null(opts$total)) opts$total <- TRUE
  if(is.null(opts$stats)) stop("stats input is missing")
  
  # Compute statistics: prob and info
  if(opts$stats %in% c("prob", "info")) {
    y <- irt_model(model=x$name, theta=opts$theta, items=x$items)
    y <- irt_stats(y, stats=opts$stats)
  } else if(opts$stats %in% c("loglik")) {
    y <- sapply(opts$theta, function(t){
      y <- irt_model(model=x$name, theta=rep(t, nrow(x$people)), items=x$items, responses=x$responses)
      irt_stats(y, stats="loglik", summary="people", fun=sum)
    })
    y <- t(y)    
  } else {
    stop("invalid 'stats' argument. use 'prob', 'info', or 'loglik'")
  }
  
  # Total
  if(opts$total) y <- rowSums(y)
  
  # Drawing
  y <- data.frame(theta=opts$theta, y)
  y <- reshape2::melt(y, id.vars="theta")
  ggplot(y, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab(opts$stats) +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}
