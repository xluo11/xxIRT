#' 3-Parameter-Logistic Model
#' @description Create a 3-parameter-logistic (3PL) model object
#' @param people people parameters (data.frame)
#' @param items item parameters (data.frame)
#' @param responses dichotomous responses (data.frame or matrix)
#' @param theta the ability parameters (vector)
#' @param a the discrimination parameters (vector)
#' @param b the difficulty parameters (vector)
#' @param c the pseudo-guessing parameters (vector)
#' @details
#' A 3pl model contains \code{people} (people parameters), \code{items} 
#' (item parameters), \code{responses} (responses data), and functions to 
#' compute \code{P} (probability), \code{I} (information), and \code{L} 
#' (likelihood). \cr
#' Arguments are allowed to be \code{NULL}. The \code{people} argument needs 
#' to have a column named \code{theta}. The \code{items} argument needs to 
#' have columns named \code{a}, \code{b}, and \code{c}. The \code{responses} 
#' argument needs to be a data frame or matrix whose dimensionality matches 
#' with \code{people} and \code{items}.
#' @examples
#' # create a 3pl model using given parameters
#' theta <- c(-1, 0, 1)
#' a <- c(.5882, 1)
#' b <- c(-1, 1)
#' c <- c(0, .2)
#' u <- matrix(c(1, 0, 1, 0, 1, 0), nrow=3)
#' people <- data.frame(theta=theta)
#' items <- data.frame(a=a, b=b, c=c)
#' model_3pl(people=people, items=items, responses=u) 
#' model_3pl(people=people, items=items) 
#' model_3pl(theta=theta, a=a, b=b, c=c) 
#' model_3pl(people=people, a=a, b=b, c=c) 
#' model_3pl(theta=theta, items=items) 
#' # compute P(robability), I(nformation), L(ikelihood)
#' x <- model_3pl(people=people, items=items, responses=u)
#' x$P()
#' x$I()
#' x$L()
#' model_3pl()$P(x)
#' model_3pl()$I(x)
#' model_3pl()$L(x)
#' # create a 3pl model using generated data
#' x <- model_3pl()$gendata(10, 5)
#' x$P(x)
#' x$I(x)
#' x$L(x)
#' @importFrom stats rnorm runif rbeta
#' @export
model_3pl <- function(people=NULL, items=NULL, responses=NULL, theta=NULL, a=NULL, b=NULL, c=NULL){
  # Validate people
  if(!is.null(people)) {
    people <- as.data.frame(people, stringsAsFactors=FALSE)
    if(!"theta" %in% colnames(people)) stop("theta is not found in people")
  } else if(!is.null(theta)) {
    people <- data.frame(theta=unlist(theta), stringsAsFactors=FALSE)
  }
  if(!is.null(people)){
    if(max(people$theta) >  5) warning("there are extremely large thetas")
    if(min(people$theta) < -5) warning("there are extremely small thetas")
  }
  
  # Validate items
  if(!is.null(items)){
    items <- as.data.frame(items, stringsAsFactors=FALSE)
    if(!all(c("a", "b", "c") %in% colnames(items))) stop("a, b, or c is not found in items")
  } else if(!is.null(a) && !is.null(b) && !is.null(c)) {
    if(length(a) == 1) a <- rep(a, length(b))
    if(length(c) == 1) c <- rep(c, length(b))
    if(length(b) != length(a) || length(b) != length(c)) stop("a, b, c have different lengths")
    items <- data.frame(a=a, b=b, c=c, stringsAsFactors=FALSE)
  }
  if(!is.null(items)){
    if(min(items$a) <= 0) stop("there are negative a parameters")
    if(max(items$a) >  3) warning("there are extremely large a parameters")
    if(min(items$b) < -5) warning("there are extremely small b parameters")
    if(max(items$a) >  5) warning("there are extremely large b parameters")
    if(min(items$c) <  0) warning("there are invalid c parameters")
    if(max(items$c) >= 1) stop("there are invalid c parameters")
  }
  
  # Validate responses
  if(!is.null(responses)){
    responses <- as.matrix(responses)
    if(nrow(responses) != nrow(people)) stop("response rows don't match with people")
    if(ncol(responses) != nrow(items)) stop("response columns don't match with items")
    if(any(responses != 0 & responses != 1 & !is.na(responses))) stop("responses are not dichotomous")
  }
  
  # Constructor
  x <- list(name="3pl", people=people, items=items, responses=responses)
  class(x) <- "model.3pl"
  
  # Function to compute probability
  x$P <- function(data=NULL){
    if(is.null(data)) data <- x
    if(is.null(data$people)) stop("No people parameters")
    if(is.null(data$items)) stop("No items parameters")
    p <- 1.7 * outer(data$items$b, data$people$theta, "-")
    p <- data$items$c + (1 - data$items$c) / (1 + exp(data$items$a * p))
    p <- t(p)
    dimnames(p) <- list(NULL, paste("item", 1:nrow(data$items), sep="."))
    return(p)
  }
  
  # Function to compute information
  x$I <- function(data=NULL){
    if(is.null(data)) data <- x
    p <- model_3pl()$P(data)
    p <- t(p)
    i <- (1.7 * data$items$a * (p - data$items$c) / (1 - data$items$c))^2 * (1 - p) / p
    i <- t(i)
    return(i)
  }
  
  # Function to compute likelihood
  x$L <- function(data=NULL, log=FALSE){
    if(is.null(data)) data <- x
    if(is.null(data$responses)) stop("No responses data")
    u <- as.matrix(data$responses)
    p <- model_3pl()$P(data)
    l <- p^u * (1-p)^(1-u)
    if(log) l <- log(l)
    return(l)
  }
  
  # Function to generate data
  x$gendata <- function(n.people=NULL, n.items=NULL, people=NULL, items=NULL, t.mean=0, t.sd=1, a.mean=0, a.sd=0.2, b.mean=0, b.sd=1, c.alpha=5, c.beta=46){
    if(is.null(n.people) && is.null(people)) stop("n.people and people arguments are both NULL")
    if(is.null(n.items) && is.null(items)) stop("n.items and items arguments are both NULL")
    if(is.null(people)) t <- rnorm(n.people, t.mean, t.sd)
    if(is.null(items)){
      a <- exp(rnorm(n.items, a.mean, a.sd))
      b <- rnorm(n.items, b.mean, b.sd)
      c <- rbeta(n.items, c.alpha, c.beta)
    }
    y <- model_3pl(theta=t, a=a, b=b, c=c)
    n.people <- nrow(y$people)
    n.items <- nrow(y$items)
    p <- model_3pl()$P(y)
    r <- matrix(runif(n.people * n.items), nrow=n.people, ncol=n.items)
    u <- (p >= r) * 1
    y$responses <- u
    return(y)
  }
  
  # Change class name and return
  return(x)
}


#' @rdname model_3pl
#' @param x a \code{model.3pl} object
#' @param ... additional arguments
#' @importFrom utils head
#' @export
print.model.3pl <- function(x, ...){
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


#' @rdname model_3pl
#' @details 
#' In \code{plot.model.3pl}, use the \code{stats} argument to control what IRT statistics to draw.
#' Use \code{total} argument to control whether to aggregate results over items or people.
#' @examples
#' # draw test/item characteristic curve
#' x <- model_3pl()$gendata(20, 5)
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
  if(is.null(opts$stats)) stop("stats is missing: 'prob', 'info', 'lik', or 'loglik'")
  # Compute statistics: prob and info
  if(opts$stats %in% c("prob", "info")) {
    y <- irt_stats(model_3pl(theta=opts$theta, items=x$items), stats=opts$stats)
  } else if(opts$stats %in% c("log", "loglik")) {
    y <- t(sapply(opts$theta, function(t){
      irt_stats(model_3pl(theta=rep(t, nrow(x$responses)), items=x$items, responses=x$responses), opts$stats, summary="people", fun=sum)
    }))
  } else {
    stop("invalid 'stats' argument. use 'prob', 'info', 'log', or 'loglik'")
  }
  # Total
  if(opts$total) y <- rowSums(y)
  # Drawing
  y <- data.frame(theta=opts$theta, y)
  y <- reshape2::melt(y, id.vars="theta")
  ggplot(y, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + 
    ylab(switch(opts$stats, 'prob'='Probability', 'info'='Information', 'lik'='Likelihood', 'loglik'='Log-likelihood')) +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}
