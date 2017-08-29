#' IRT Utility Functions
#' @description \code{irt_stats} provides a friendly interface to compute statistics
#' @param x an IRT model object
#' @param stats the statistic to be computed
#' @param summary the summarization direction
#' @param fun the summarization function
#' @param ... other optional arguments
#' @details 
#' In \code{irt_stats}, use \code{stats="prob"} to compute probability, 
#' \code{stats="info"} to compute information, \code{stats="lik"} to compute 
#' likelihood, and \code{stats="loglik"} to compute log-likelihood. 
#' Use \code{summary="people"} to summarize results over items and 
#' \code{summary="items"} to summarize results over people.\cr
#' @examples  
#' # Compute probability, information, likelihood and log-likelihood
#' x <- model_3pl()$gendata(20, 5)
#' irt_stats(x, "prob")
#' irt_stats(x, "prob", "people", sum)
#' irt_stats(x, "info")
#' irt_stats(x, "info", "items", sum)
#' irt_stats(x, "lik")
#' irt_stats(x, "loglik")
#' @export
irt_stats <- function(x, stats=c("prob", "info", "lik", "loglik"), summary=NULL, fun=NULL, ...){
  stats <- match.arg(stats)
  value <- switch (stats, "prob" = x$P(x), "info" = x$I(x), "lik" = x$L(x), "loglik" = x$L(x, log=TRUE))
  if(!is.null(summary)){
    if(is.null(fun)) stop("No summary function")
    if(!summary %in% c("people", "items")) stop("invalid summary input. use 'people' or 'items'")
    value <- apply(value, ifelse(summary=="people", 1, 2), fun, ...)
  }
  return(value)
}

#' @rdname irt_stats
#' @description \code{irt_select} subsets data in an IRT model 
#' @param people_index the indices of people to keep
#' @param item_index the indices of items to keep
#' @examples 
#' # subset
#' x <- model_3pl()$gendata(10, 5)
#' irt_select(x, people_index=c(1, 3, 5))
#' irt_select(x, item_index=c(1, 3, 5))
#' @export
irt_select <- function(x, people_index=NULL, item_index=NULL){
  # people
  if(!is.null(people_index) && is.null(x$people)) stop("No people parameters to select")
  if(is.null(people_index)) people_index <- 1:nrow(x$people)
  people <- data.frame(theta=x$people$theta[people_index])
  # items
  if(!is.null(item_index) && is.null(x$items)) stop("No item parameters to select")
  if(is.null(item_index)) item_index <- 1:nrow(x$items)
  items <- x$items[item_index, ]
  # responses
  if(!is.null(x$responses)) responses <- x$responses[people_index, item_index]
  return(model_3pl(people=people, items=items, responses=responses))
}


#' @rdname irt_stats
#' @description \code{irt_sample} randomly samples data in an IRT model
#' @param n.people the number of people to sample
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
  # people
  if(!is.null(n.people) && is.null(x$people)) stop("No people parameters to sample")
  if(is.null(n.people)) n.people <- nrow(x$people)
  people_index <- sample(1:nrow(x$people), n.people, replace=(n.people > nrow(x$people)))
  # items
  if(!is.null(n.items) && is.null(x$items)) stop("No item parameters to sample")
  if(is.null(n.items)) n.items <- nrow(x$items)
  item_index <- sample(1:nrow(x$items), n.items, replace=(n.items > nrow(x$items)))
  return(irt_select(x, people_index, item_index))
}

#' @rdname irt_stats
#' @description \code{irt_rescale} rescales parameters in a 3PL model
#' @param parameter the rescaling parameter: 'theta' or 'b'
#' @param mean the mean of the new scale
#' @param sd the standard deviation of the new scale
#' @examples
#' # rescale theta
#' x <- model_3pl()$gendata(20, 5)
#' c(mean(x$people$theta), sd(x$people$theta))
#' y <- irt_rescale(x, "theta", 0, 1)
#' c(mean(y$people$theta), sd(y$people$theta))
#' round(abs(irt_stats(x, "prob") - irt_stats(y, "prob")), 2)
#' # rescale b
#' c(mean(x$items$b), sd(x$items$b))
#' y <- irt_rescale(x, "b", 0, 1)
#' c(mean(y$items$b), sd(y$items$b))
#' round(abs(irt_stats(x, "prob") - irt_stats(y, "prob")), 2)
#' @importFrom stats sd
#' @export
irt_rescale <- function(x, parameter=c("theta", "b"), mean=0, sd=1) {
  parameter <- match.arg(parameter)
  old_scale <- switch(parameter, 
                      "theta"=c(mean=mean(x$people$theta), sd=stats::sd(x$people$theta)),
                      "b"=c(mean=mean(x$items$b), sd=stats::sd(x$items$b)))
  a <- sd / old_scale["sd"]
  b <- mean - a * old_scale["mean"]
  x$people$theta <- a * x$people$theta + b
  x$items$b <- a * x$items$b + b
  x$items$a <- x$items$a / a
  return(x)
}



