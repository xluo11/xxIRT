#' 3-parameter-logistic Model
#' @description \code{irt.model.3pl} creates a 3PL \code{irt.model} object
#' @param people a data frame of \code{theta} parameters
#' @param items a data frame of \code{a}, \code{b}, \code{c} parameters 
#' @param responses a data frame of dichotomous responses  
#' @return a \code{irt.mode} object
#' @details
#' All arguments in \code{irt.model.3pl} are allowed to be \code{NULL}.
#' If not \code{NULL}, (a) the \code{people} argument is expected to be a data frame with a column named \code{theta}, 
#' the \code{items} argument is expected to be a data frame with three columns named \code{a}, \code{b}, and \code{c}, and  
#' the \code{responses} argument is expected to be a data frame with dimesions equal to the numbers of people (in rows) and items (in columns).\cr
#' @examples
#' # create a 3pl model with manual inputs
#' people <- data.frame(theta=c(-1,0,1))
#' items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
#' responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), 
#' item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
#' x <- irt.model.3pl(people, items, responses) 
#' x$probability(x)
#' x$information(x)
#' x$likelihood(x)
#' # create a 3pl model with generated data
#' x <- irt.model.3pl()$gen.data(10, 5)
#' x$probability(x)
#' x$information(x)
#' x$likelihood(x)
#' @importFrom stats rnorm runif rbeta
#' @export
irt.model.3pl <- function(people=NULL, items=NULL, responses=NULL){
  # validation: 1. column names; 2. value range
  if(!is.null(people)){
    people <- as.data.frame(people, stringsAsFactors=FALSE)
    if(ncol(people) == 1) colnames(people) <- "theta"
    if(!"theta" %in% colnames(people)) stop("theta is not found in people")
    people <- data.frame(theta=people$theta)
    if(max(people$theta) > 5) warning("there are some extremely large thetas in people")
    if(min(people$theta) < -5) warning("there are some extremely small thetas in people")
  }
  if(!is.null(items)){
    items <- as.data.frame(items, stringsAsFactors=FALSE)
    if(ncol(items) == 3) colnames(items) <- c("a", "b", "c")
    if(any(!c("a", "b", "c") %in% colnames(items))) stop("a, b, c are not found in items")
    items <- data.frame(a=items$a, b=items$b, c=items$c)
    if(min(items$a) <= 0) warning("there are some negative a parameters in items")
    if(max(items$a) > 3) warning("there are some extremely large a parameters in items")
    if(min(items$b) < -5) warning("there are some extremely small b parameters in items")
    if(max(items$a) > 5) warning("there are some extremely large b parameters in items")
    if(min(items$c) < 0) warning("there are some negative c parameters in items")
    if(max(items$c) >= 1) warning("there are some extremely large c parameters in items")
  }
  if(!is.null(responses)){
    responses <- as.data.frame(responses, stringsAsFactors=FALSE)
    if(nrow(responses) != nrow(people)) stop("response rows don't match people")
    if(ncol(responses) != nrow(items)) stop("response columns don't match items")
    if(any(responses != 0 & responses != 1 & !is.na(responses))) stop("responses are not dichotomous")
  }
  
  # constructor
  x <- list(name="3pl", people=people, items=items, responses=responses)
  x$probability <- function(x){
    if(class(x) != "irt.model") stop("data is not a irt.model object")
    if(is.null(x$people)) stop("No people parameters")
    if(is.null(x$items)) stop("No items parameters")
    people <- x$people
    items <- x$items
    # calculation
    p <- 1.7 * outer(items$b, people$theta, "-")
    p <- items$c + (1 - items$c) / (1 + exp(items$a * p))
    p <- t(p)
    dimnames(p) <- list(paste("people", 1:nrow(people), sep="."), paste("item", 1:nrow(items), sep="."))
    return(p)
  }
  x$information <- function(x){
    if(class(x) != "irt.model") stop("data is not a irt.model object")
    items <- x$items
    p <- t(x$probability(x))
    i <- (1.7 * items$a * (p - items$c) / (1 - items$c))^2 * (1 - p) / p
    i <- t(i)
    return(i)
  }
  x$likelihood <- function(x){
    if(class(x) != "irt.model") stop("data is not a irt.model object")
    if(is.null(x$responses)) stop("No responses.")
    u <- as.matrix(x$responses)
    p <- x$probability(x)
    l <- p^u * (1-p)^(1-u)
    return(l)
  }
  x$gen.data <- function(n.people=NULL, n.items=NULL, people=NULL, items=NULL, theta.mu=0, theta.sig=1, a.mu=0, a.sig=0.2, b.mu=0, b.sig=1, c.alpha=0, c.beta=.2){
    if(is.null(n.people) && is.null(people)) stop("Pass in either the number of people or the actual people parameters")
    if(is.null(n.items) && is.null(items)) stop("Pass in either the number of items or the actual item pamaraters")
    if(is.null(people))
      people <- data.frame(theta=rnorm(n.people, theta.mu, theta.sig))
    if(is.null(items))
      items <- data.frame(a=exp(rnorm(n.items, a.mu, a.sig)), b=rnorm(n.items, b.mu, b.sig), c=rbeta(n.items, c.alpha, c.beta))
    n.people <- nrow(people)
    n.items <- nrow(items)
    y <- irt.model.3pl(people, items, NULL)
    p <- y$probability(y)
    r <- matrix(runif(n.people * n.items), nrow=n.people, ncol=n.items)
    responses <- (p >= r) * 1
    y$responses <- data.frame(responses)
    return(y)
  }
  class(x) <- "irt.model"
  return(x)
}