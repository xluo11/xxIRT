#' 3PL Model
#' @description Create a 3PL model object
#' @param people a data frame of \code{theta} parameters
#' @param items a data frame of \code{a}, \code{b}, \code{c} parameters
#' @param responses a data frame or a matrix of dichotomous responses
#' @param theta a vector of \code{theta} parameters
#' @param a a vector of \code{a} parmaeters
#' @param b a vector of \code{b} parmaeters
#' @param c a vector of \code{c} parmaeters
#' @details
#' A 3pl model contains \code{people} (peoeple parameters), \code{items} (item parameters),
#' \code{responses} (responses data), and functions to compute \code{P} (probability),
#' \code{I} (information), and \code{L} (likelihood). \cr
#' Inputs are allowed to be \code{NULL}. When necessary parameters are null, a function returns an error. 
#' All arguments in \code{irt.model.3pl} are allowed to be \code{NULL}. The \code{people} argument needs to be
#' a one-column data frame or a data frame with a column named theta. The \code{items} argument needs to be
#' a three-column data frame or a data frame with columns named a, b, and c. The \code{responses} argument needs
#' to be a data frame or matrix whose dimensions match the dimension of \code{people} and \code{items}.
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
#' x$P(x)
#' x$I(x)
#' x$L(x)
#' model_3pl()$P(x)
#' model_3pl()$I(x)
#' model_3pl()$L(x)
#' # create a 3pl model using generated data
#' x <- model_3pl()$gendata(10, 5)
#' x
#' x$P(x)
#' x$I(x)
#' x$L(x)
#' @importFrom stats rnorm runif rbeta
#' @export
model_3pl <- function(people=NULL, items=NULL, responses=NULL, theta=NULL, a=NULL, b=NULL, c=NULL){
  # Validate input: people
  if(!is.null(people)) {
    people <- as.data.frame(people, stringsAsFactors=FALSE)
    if(!"theta" %in% colnames(people)) stop("theta is not found in people")
  } else if(!is.null(theta)) {
    people <- data.frame(theta=unlist(theta), stringsAsFactors=FALSE)
  }
  if(!is.null(people)){
    if(max(people$theta) > 5) warning("there are extremely large thetas in people")
    if(min(people$theta) < -5) warning("there are extremely small thetas in people")
  }
  
  # Validate input: items
  if(!is.null(items)){
    items <- as.data.frame(items, stringsAsFactors=FALSE)
    if(!all(c("a", "b", "c") %in% colnames(items))) stop("a, b, c are not found in items")
  } else if(!is.null(a) && !is.null(b) && !is.null(c)) {
    if(length(a) == 1) a <- rep(a, length(b))
    if(length(c) == 1) c <- rep(c, length(b))
    if(length(b) != length(a) || length(b) != length(c)) stop("a, b, c have different lengths")
    items <- data.frame(a=a, b=b, c=c, stringsAsFactors=FALSE)
  }
  if(!is.null(items)){
    if(min(items$a) <= 0) warning("there are negative a parameters in items")
    if(max(items$a) > 3) warning("there are extremely large a parameters in items")
    if(min(items$b) < -5) warning("there are extremely small b parameters in items")
    if(max(items$a) > 5) warning("there are extremely large b parameters in items")
    if(min(items$c) < 0) warning("there are negative c parameters in items")
    if(max(items$c) >= 1) warning("there are extremely large c parameters in items")
  }
  
  # Validate input: responses
  if(!is.null(responses)){
    responses <- as.matrix(responses)
    if(nrow(responses) != nrow(people)) stop("response rows don't match people")
    if(ncol(responses) != nrow(items)) stop("response columns don't match items")
    if(any(responses != 0 & responses != 1 & !is.na(responses))) stop("responses are not dichotomous")
  }
  
  # Constructor
  x <- list(name="3pl", people=people, items=items, responses=responses)
  
  # Function to compute probability
  x$P <- function(x){
    if(!"model.3pl" %in% class(x)) stop("input is not a 3pl object")
    if(is.null(x$people)) stop("No people parameters")
    if(is.null(x$items)) stop("No items parameters")
    people <- x$people
    items <- x$items
    p <- 1.7 * outer(items$b, people$theta, "-")
    p <- items$c + (1 - items$c) / (1 + exp(items$a * p))
    p <- t(p)
    dimnames(p) <- list(paste("people", 1:nrow(people), sep="."), paste("item", 1:nrow(items), sep="."))
    return(p)
  }
  
  # Function to compute information
  x$I <- function(x){
    if(!"model.3pl" %in% class(x)) stop("input is not a 3pl object")
    items <- x$items
    p <- t(x$P(x))
    i <- (1.7 * items$a * (p - items$c) / (1 - items$c))^2 * (1 - p) / p
    i <- t(i)
    return(i)
  }
  
  # Function to compute likelihood
  x$L <- function(x, log=FALSE){
    if(!"model.3pl" %in% class(x)) stop("input is not a 3pl object")
    if(is.null(x$responses)) stop("No responses data")
    u <- as.matrix(x$responses)
    p <- x$P(x)
    l <- p^u * (1-p)^(1-u)
    if(log) l <- log(l)
    return(l)
  }
  
  # Function to generate data
  x$gendata <- function(n.people=NULL, n.items=NULL, people=NULL, items=NULL, theta.mean=0, theta.sd=1, a.mean=0, a.sd=0.2, b.mean=0, b.sd=1, c.alpha=5, c.beta=42){
    if(is.null(n.people) && is.null(people)) stop("Both n.people and people arguments are NULL")
    if(is.null(n.items) && is.null(items)) stop("Both n.items and items arguments are NULL")
    if(is.null(people)) t <- rnorm(n.people, theta.mean, theta.sd)
    if(is.null(items)){
      a <- exp(rnorm(n.items, a.mean, a.sd))
      b <- rnorm(n.items, b.mean, b.sd)
      c <- rbeta(n.items, c.alpha, c.beta)
    }
    y <- model_3pl(theta=t, a=a, b=b, c=c)
    n.people <- nrow(y$people)
    n.items <- nrow(y$items)
    p <- y$P(y)
    r <- matrix(runif(n.people * n.items), nrow=n.people, ncol=n.items)
    u <- (p >= r) * 1
    y$responses <- u
    return(y)
  }
  
  # Change class name and return
  class(x) <- c("model.3pl", "irt.model")
  return(x)
}