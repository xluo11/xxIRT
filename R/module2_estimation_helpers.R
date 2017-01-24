#' Estimation Helper Functions
#' @name estimationHelper
NULL


#' @rdname estimationHelper
#' @param responses the responses data
#' @param items the item parameters
estimate_people_3pl_check_input <- function(responses, items){
  # convert vector to matrix
  if(is.vector(responses) && length(responses) == nrow(items))
    responses <- matrix(responses, nrow=1)
  responses <- as.matrix(responses)
  # validate dimension
  if(ncol(responses) != nrow(items))
    stop("response columns don't match item rows")
  # validate dichotomy
  if(any(responses != 0  & responses != 1 & !is.na(responses)))
    stop("response is not dichotomous")
  return(list(responses=responses, items=items))
}


#' @rdname estimationHelper
#' @details 
#' \code{qudrature} was originally intended to call \code{gaussquad::hermite.h.quadrature.rules(12)[[12]]}
quadrature <- function () {
  return(data.frame(x=c( 3.889725e+00, 3.020637e+00, 2.279507081, 1.59768264, 0.9477884, 0.3142404, -0.3142404, -0.9477884, -1.59768264, -2.279507081, -3.020637e+00, -3.889725e+00),
                    w=c(2.658552e-07, 8.573687e-05, 0.003905391, 0.05160799, 0.2604923, 0.5701352, 0.5701352, 0.2604923, 0.05160799, 0.003905391, 8.573687e-05, 2.658552e-07)))
}


#' @rdname estimationHelper
#' @param people the people parameters
estimate_items_3pl_check_input <- function(responses, people){
  # convert vector to data frame
  if(is.vector(responses) && length(responses) == nrow(people))
    responses <- matrix(responses, ncol=1)
  responses <- as.matrix(responses)
  # validate dimensions
  if(!is.null(people) && nrow(responses) != nrow(people))
    stop("response rows don't match people rows")
  # validate dichotomy
  if(any(responses != 0  & responses != 1 & !is.na(responses)))
    stop("response is not dichotomous")
  return(list(responses=responses, people=people))
}


#' @rdname estimationHelper
#' @param n.items the number of items
#' @param fix a list of fixed parameter values
#' @param init the initial values of parameters
estimate_items_3pl_init_par <- function(fix, init, n.items){
  if(is.null(fix$a)){
    a.fixed <- rep(FALSE, n.items)
  } else if(length(fix$a) == 1) {
    a.fixed <- rep(TRUE, n.items)
  } else if(length(fix$a) == n.items) {
    a.fixed <- !is.na(fix$a)
  } else {
    stop("invalid fixed a parameters")
  }
  a <- ifelse(a.fixed, fix$a, init$a)
  if(is.null(fix$b)){
    b.fixed <- rep(FALSE, n.items)
  } else if(length(fix$b) == 1) {
    b.fixed <- rep(TRUE, n.items)
  } else if(length(fix$b) == n.items) {
    b.fixed <- !is.na(fix$b)
  } else {
    stop("invalid fixed b parameters")
  }
  b <- ifelse(b.fixed, fix$b, init$b)
  if(is.null(fix$c)){
    c.fixed <- rep(FALSE, n.items)
  } else if(length(fix$c) == 1) {
    c.fixed <- rep(TRUE, n.items)
  } else if(length(fix$c) == n.items) {
    c.fixed <- !is.na(fix$c)
  } else {
    stop("invalid fixed c parameters")
  }
  c <- ifelse(c.fixed, fix$c, init$c)
  return(list(a=a, b=b, c=c, a.fixed=a.fixed, b.fixed=b.fixed, c.fixed=c.fixed))
}
