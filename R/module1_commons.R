#' Common IRT Computations and Operations
#' @description \code{irt.model} is a function for creating \code{irt.model} objects
#' @param people a data frame of people parameters
#' @param items a data frame of item parameters
#' @param responses a data frame of responses
#' @param model the name of IRT model
#' @return \code{irt.model} returns a \code{irt.mode} object
#' @details 
#' A \code{irt.model} object contains people parameters, item parameters and responses.
#' The \code{irt.model} also contains functions to\code{probability}, \code{informaiton} and \code{likelihood}.
#' In addtional, it has a \code{gen.data} function to generate data. \cr
#' @examples
#' # create a 3PL model with manual inputs
#' people <- data.frame(theta=c(-1,0,1))
#' items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
#' responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), 
#' item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
#' x <- irt.model(people, items, responses, "3PL")
#' x
#' x$probability(x)
#' x$information(x)
#' x$likelihood(x)
#' # create a 3PL model with generated data
#' x <- irt.model(model="3pl")$gen.data(20, 5)
#' x
#' x$probability(x)
#' x$information(x)
#' x$likelihood(x)
#' # Generate Rasch items
#' irt.model(model="3pl")$gen.data(20, 5, a.mu=0, a.sig=0, c.alpha=0, c.beta=0)
#' # Generate an 3PL item pool
#' irt.model(model="3pl")$gen.data(1, 100)$items
#' # Generate responses using given people and item parameters
#' irt.model(model="3pl")$gen.data(people=people, items=items)$responses
#' @export
irt.model <- function(people=NULL, items=NULL, responses=NULL, model="3pl"){
  x <- switch(tolower(model),
              "3pl" = irt.model.3pl(people, items, responses))
  return(x)
}

#' @rdname irt.model
#' @description \code{irt.model.3pl.init} is a function for creating \code{irt.model.3pl} object using parameter vectors
#' @param theta a vector of theta parameters
#' @param a a vector of a parameters
#' @param b a vector of b parameters
#' @param c a vector of b parameters
#' @examples 
#' # create 3pl model using parameter vectors
#' irt.model.3pl.init(people$theta, items$a, items$b, items$c, responses)
#' irt.model.3pl.init(people$theta, .58, items$b, 0, NULL)
#' @export
irt.model.3pl.init <- function(theta, a, b, c, responses=NULL){
  n.people <- length(theta)
  n.items <- length(b)
  if(length(a) == 1) a <- rep(a, n.items)
  if(length(c) == 1) c <- rep(c, n.items)
  if(length(a) != n.items) stop("invalid a parameter length")
  if(length(c) != n.items) stop("invalid c parameter length")
  x <- irt.model.3pl(data.frame(theta=theta), data.frame(a=a, b=b, c=c), responses)
  return(x)
}

#' @rdname irt.model
#' @description \code{irt.stats} is a function for common IRT computations (e.g., probability, information, likelihood)
#' @param x a \code{irt.model} object
#' @param stats the statistic to be computed (e.g., \code{probability}, \code{information}, \code{likelihood})
#' @param summary the summarization dimension (e.g., \code{people}, \code{items})
#' @param fun the summarization function (e.g, \code{sum} or \code{prod})
#' @param ... other optional arguments
#' @return \code{irt.stats} returns a vector/matrix of resulting computation values
#' @examples  
#' # compute probability, information, likelihood and loglikelihood
#' x <- irt.model(model="3pl")$gen.data(20, 5)
#' irt.stats(x, "probability")
#' irt.stats(x, "probability", "people", sum)
#' irt.stats(x, "information")
#' irt.stats(x, "information", "items", sum)
#' irt.stats(x, "likelihood")
#' irt.stats(x, "likelihood", "people", prod)
#' log(irt.stats(x, "likelihood", "people", prod))
#' irt.stats(x, "loglikelihood", "people", sum)
#' @export
irt.stats <- function(x, stats="probability", summary=NULL, fun=NULL, ...){
  if(class(x) != "irt.model") stop("data is not a irt.model object")
  if(!stats %in% c("probability", "information", "likelihood", "loglikelihood")) stop("invalid stats input. use 'probability', 'information', or 'likelihood'")
  value <- switch (stats,
                   "probability" = x$probability(x),
                   "information" = x$information(x),
                   "likelihood" = x$likelihood(x),
                   "loglikelihood" = log(x$likelihoo(x)))
  if(!is.null(summary)){
    if(is.null(fun)) stop("no summary function")
    if(!summary %in% c("people", "items")) stop("invalid summary input. use 'people' or 'items'")
    value <- apply(value, ifelse(summary=="people", 1, 2), fun, ...)
  }
  return(value)
}

#' @rdname irt.model
#' @importFrom utils head
#' @export
print.irt.model <- function(x, ...){
  n.people <- ifelse(is.null(x$people), 0, nrow(x$people))
  n.items <- ifelse(is.null(x$items), 0, nrow(x$items))
  with.responses <- ifelse(is.null(x$responses), "without", "with")
  cat("a ", x$name, " irt.model object (", n.people, " people by ", n.items, " items) ", with.responses, " responses.\n", sep="")
  cat("snapshot of people:\n")
  print(head(x$people, n=10))
  cat("snapshot of items:\n")
  print(head(x$items, n=6))
  cat("snapshot of responses:\n")
  print(x$responses[1:min(n.people, 10), 1:min(n.items, 6)])
  invisible(x)
}

#' @rdname irt.model
#' @description \code{plot.irt.model} draws the item/test characteristics curve, informaiton function, likelihood curve for an IRT object
#' @return \code{plot.irt.model} returns a 'ggplot2' object
#' @details 
#' In \code{plot.irt.model}, use the \code{stats} argument to control what IRT statistics for drawing.
#' Use \code{total} argument to control draw one summed line for the whole test or multiple line for indiviual items.
#' When drawing \code{stats="loglikelihood"}, make sure the \code{irt.model} object has only one person
#' @examples
#' # plot probability, information and log-likelihood
#' x <- irt.model(model="3pl")$gen.data(20, 5)
#' plot(x, stats="probability")
#' plot(x, stats="probability", total=FALSE)
#' plot(x, stats="information")
#' plot(x, stats="information", total=FALSE)
#' plot(irt.sample(x, n.people=1), stats="loglikelihood")
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
plot.irt.model <- function(x, ...){
  # inputs
  opts <- list(...)
  if(is.null(opts$theta)) opts$theta <- round(seq(-3, 3, .1), 1)
  theta <- opts$theta
  if(is.null(opts$total)) opts$total <- TRUE
  total <- opts$total
  if(is.null(opts$stats)) stop("stats input is missing")
  stats <- opts$stats
  # compute statistics
  if(stats == "loglikelihood"){
    if(nrow(x$responses) != 1)
      stop("Only allow one person's responses when drawing loglikelihood")
    responses <- matrix(rep(unlist(x$responses), length(theta)), nrow=length(theta), byrow=TRUE)
    x <- irt.model(data.frame(theta=theta), x$items, responses)
  } else if(stats %in% c("probability", "information")){
    x <- irt.model(data.frame(theta=theta), x$items, NULL)
  } else {
    stop("invalid stats value. use 'probability', 'information', or 'loglikelihood'")
  }
  # total
  if(total){
    y <- irt.stats(x, stats, "people", sum)
  } else {
    y <- irt.stats(x, stats, NULL, NULL)
  }
  y <- data.frame(theta=theta, y)
  y <- melt(y, id.vars="theta")
  # drawing
  ggplot(y, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab(stats) +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}

#' @rdname irt.model
#' @description \code{irt.select} selects data from an IRT object
#' @param people.index the indices of people to select
#' @param items.index the indices of items to select
#' @details 
#' \code{irt.select} subsets responses when it is not \code{NULL}.
#' When \code{people.index} or \code{items.index} is \code{NULL}, keep all.
#' @examples 
#' # select from an IRT model
#' irt.select(x, people.index=c(1,3,5))
#' irt.select(x, items.index=c(1,3,5))
#' @export
irt.select <- function(x, people.index=NULL, items.index=NULL){
  if(class(x) != "irt.model") stop("data is not a irt.model object")
  if(!is.null(people.index) && is.null(x$people)) stop("No people parameters to select")
  if(is.null(people.index)) people.index <- 1:nrow(x$people)
  people <- data.frame(theta=x$people$theta[people.index])
  if(!is.null(items.index) && is.null(x$items)) stop("No item parameters to select")
  if(is.null(items.index)) items.index <- 1:nrow(x$items)
  items <- x$items[items.index, ]
  if(!is.null(x$responses))
    responses <- x$responses[people.index, items.index]
  return(irt.model(people, items, responses, x$name))
}

#' @rdname irt.model
#' @description \code{irt.sample} samples data from an IRT object
#' @param n.people the nubmer of people to sample
#' @param n.items the number of items to sample
#' @examples
#' # sample wihtout replacement from an IRT model
#' irt.sample(x, n.people=3)
#' irt.sample(x, n.items=3)
#' # sample with replacement from an IRT model
#' irt.sample(x, n.people=30)
#' irt.sample(x, n.items=30)
#' @export
irt.sample <- function(x, n.people=0, n.items=0){
  if(class(x) != "irt.model") stop("data is not a irt.model object")
  if(n.people > 0 && is.null(x$people)) stop("No people parameters to sample")
  if(n.people == 0) {
    people.index <- NULL
  } else {
    people.index <- sample(1:nrow(x$people), n.people, replace=ifelse(n.people > nrow(x$people), TRUE, FALSE))
  }
  if(n.items > 0 && is.null(x$items)) stop("No item parameters to sample")
  if(n.items == 0) {
    items.index <- NULL
  } else {
    items.index <- sample(1:nrow(x$items), n.items, replace=ifelse(n.items > nrow(x$items), TRUE, FALSE))
  }
  return(irt.select(x, people.index, items.index))
}

#' @rdname irt.model
#' @description \code{irt.rescale.3pl} rescale 3PL parameters
#' @param parameter the rescaling parameter
#' @param mu the mean of the new scale
#' @param sig the standard deviation of the new scale
#' @examples
#' # rescale theta
#' x <- irt.model(model="3pl")$gen.data(5,3)
#' c(mean(x$people$theta), sd(x$people$theta))
#' y <- irt.rescale.3pl(x, "theta", 0, 1)
#' c(mean(y$people$theta), sd(y$people$theta))
#' irt.stats(x, "probability")
#' irt.stats(y, "probability")
#' # rescale b
#' x <- irt.model(model="3pl")$gen.data(5,3)
#' c(mean(x$items$b), sd(x$items$b))
#' y <- irt.rescale.3pl(x, "b", 0, 1)
#' c(mean(y$items$b), sd(y$items$b))
#' irt.stats(x, "probability")
#' irt.stats(y, "probability")
#' @importFrom stats sd
#' @export
irt.rescale.3pl <- function(x, parameter="theta", mu=0, sig=1){
  if(class(x) != "irt.model" || x$name != "3pl") 
    stop("data is not a 3PL irt.model object")
  if(parameter == "theta"){
    mu0 <- mean(x$people$theta)
    sig0 <- sd(x$people$theta)
  } else if(parameter == "b"){
    mu0 <- mean(x$items$b)
    sig0 <- sd(x$items$b)
  } else {
    stop("invalid rescale parameter. use 'theta' or 'b'")
  }
  a <- sig / sig0
  b <- mu - a * mu0
  x$people$theta <- a * x$people$theta + b
  x$items$b <- a * x$items$b + b
  x$items$a <- x$items$a / a
  return(x)
}
