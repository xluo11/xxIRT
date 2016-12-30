#' Estimation of People Parameters
#' @description \code{estimate.people} is a wrapper function for estimating people parameters
#' @param responses a data frame of responses
#' @param items a data frame of item parameters 
#' @param model the IRT model
#' @param method the estimation method
#' @param ... further arguments
#' @return a data frame of estimated people parameters
#' @examples
#' \dontrun{
#' # data generation
#' data <- irt.model(model="3pl")$gen.data(500, 50)
#' # MLE
#' x <- estimate.people(data$responses, data$items, "3pl", "mle", debug=TRUE)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' # MAP
#' x <- estimate.people(data$responses, data$items, "3pl", "map", debug=TRUE)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' # EAP
#' x <- estimate.people(data$responses, data$items, "3pl", "eap")
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' }
#' @export
estimate.people <- function(responses, items, model="3pl", method="mle", ...){
  switch(tolower(model),
         "3pl" = switch(tolower(method),
                        "mle" = estimate.people.3pl.mle(responses, items, ...),
                        "map" = estimate.people.3pl.map(responses, items, ...),
                        "eap" = estimate.people.3pl.eap(responses, items, ...)
                        )
         )
}

#' @rdname quadrature
#' @description \code{estimate.people.3pl.check.input} is a helper function for validating inputs
#' @param responses the responses data
#' @param items the item parameters
#' @return a list of validated \code{responses} and \code{items}
estimate.people.3pl.check.input <- function(responses, items){
  # convert vector to data.frame
  if(is.vector(responses) && length(responses) == nrow(items))
    responses <- as.data.frame(matrix(unlist(responses), nrow=1))
  # validate dimension
  if(ncol(responses) != nrow(items))
    stop("response columns don't match item rows")
  # validate dichotomy
  if(any(responses != 0  & responses != 1 & !is.na(responses)))
    stop("response is not dichotomous")
  return(list(responses=responses, items=items))
}

#' @rdname estimate.people
#' @description \code{estimate.people.3pl.mle} estimates people parameters with known item parameters using maximum likelihood estimation.
#' @param init initial values of people parameters, a number or a vector
#' @param iter the maximum of iterations 
#' @param conv convergence criterion 
#' @param bound the bounds of resulting people parameters
#' @param debug \code{TRUE} to turn on debug mode
#' @details
#' The debug mode draws a convergence curve. \cr
#' For the maximum likelihood estimation, refer to Baker and Kim (2004), pp. 66-69.
#' @examples
#' \dontrun{
#' # data generation
#' data <- irt.model(model="3pl")$gen.data(500, 50)
#' # MLE
#' x <- estimate.people.3pl.mle(data$responses, data$items, debug=TRUE)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' # MAP
#' x <- estimate.people.3pl.map(data$responses, data$items, debug=TRUE)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' # EAP
#' x <- estimate.people.3pl.eap(data$responses, data$items)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
#' }
#' @importFrom graphics plot
#' @importFrom grDevices rgb
#' @export
estimate.people.3pl.mle <- function(responses, items, init=0, iter=30, conv=0.01, bound=3.5, debug=FALSE){
  # validation
  inputs <- estimate.people.3pl.check.input(responses, items)
  responses <- as.matrix(inputs$responses)
  items <- inputs$items
  # inits
  diagnosis <- list(h=rep(NA, iter))
  n.items <- nrow(items)
  n.people <- nrow(responses)
  if(length(init) == 1) init <- rep(init, n.people)
  if(length(init) != n.people) stop("incorrect init length")
  t <- init
  u <- responses
  a.matrix <- matrix(rep(items$a, each=n.people), nrow=n.people)
  h <- rep(0, n.people)
  # newton raphson
  if(debug) cat("Estimating:")
  for(i in 1:iter){
    if(debug) cat(".")
    p <- irt.stats(irt.model(data.frame(theta=t), items, responses, model="3pl"), "probability")
    p.star <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=items$a, b=items$b, c=0), responses, model="3pl"), "probability")
    L1 <- 1.7 * a.matrix * (u - p) * p.star / p
    L2 <- (-1) * (1.7 * a.matrix * p.star / p) ^ 2 * p * (1 - p)
    h0 <- h
    h <- rowSums(L1, na.rm=TRUE) / rowSums(L2, na.rm=TRUE)
    h <- ifelse(abs(h) > abs(h0), h * .5, h)
    t <- ifelse(abs(t - h) > bound, t, t - h)
    diagnosis$h[i] <- mean(abs(h))
    if(mean(abs(h)) <= conv) break
  }
  output <- list(people=data.frame(theta=t))
  diagnosis$h <- diagnosis$h[1:i]
  if(debug) {
    cat("Done.\n")
    output$diagnosis <- diagnosis
    plot(diagnosis$h, type="l", xlim=c(0, iter), xlab="Iteration", ylab="Convergence", col=rgb(.8,.2,.2,.7))
  }
  return(output)
}

#' @rdname estimate.people
#' @description \code{estimate.people.3pl.map} estimates people parameters with known item parameters using maximum a posterior.
#' @param prior.mu the mean of the prior distribuiton
#' @param prior.sig the standard deviation of the prior distribution
#' @details
#' For the maximum a posteriori estimation, refer to Baker and Kim (2004), pp. 192.
#' @importFrom graphics plot
#' @importFrom grDevices rgb
#' @export
estimate.people.3pl.map <- function(responses, items, prior.mu=0, prior.sig=1, init=0, iter=15, conv=0.01, bound=3.5, debug=FALSE){
  # validation
  inputs <- estimate.people.3pl.check.input(responses, items)
  responses <- as.matrix(inputs$responses)
  items <- inputs$items
  # inits
  diagnosis <- list(h=rep(NA, iter))
  n.items <- nrow(items)
  n.people <- nrow(responses)
  if(length(init) == 1) init <- rep(init, n.people)
  if(length(init) != n.people) stop("incorrect init length")
  t <- init
  u <- responses
  a.matrix <- matrix(rep(items$a, each=n.people), nrow=n.people)
  c.matrix <- matrix(rep(items$c, each=n.people), nrow=n.people)
  h <- rep(0, n.people)
  # newton raphson iteration
  if(debug) cat("Estimating:")
  for(i in 1:iter){
    if(debug) cat(".")
    p <- irt.stats(irt.model(data.frame(theta=t), items, responses, model="3pl"), "probability")
    L1 <- (1.7 * a.matrix) * (p - c.matrix) / (p * (1 - c.matrix)) * (u - p)
    L1 <- rowSums(L1, na.rm=TRUE) - (t - prior.mu) / prior.sig
    L2 <- (-1) * (1.7 * a.matrix * (p - c.matrix) / (1 - c.matrix)) ^ 2 * (1 - p) / p
    L2 <- rowSums(L2, na.rm=TRUE) - 1 / prior.sig^2
    h0 <- h
    h <- L1 / L2
    h <- ifelse(abs(h) > abs(h0), h * .5, h)
    t <- ifelse(abs(t - h) > bound, t, t - h)
    diagnosis$h[i] <- mean(abs(h))
    if(mean(abs(h)) < conv) break
  }
  output <- list(people=data.frame(theta=t))
  diagnosis$h <- diagnosis$h[1:i]
  if(debug){
    cat("Done.\n")
    output$diagnosis <- diagnosis
    plot(diagnosis$h, type="l", xlim=c(0, iter), xlab="Iteration", ylab="Convergence", col=rgb(.8,.2,.2,.7))
  }
  return(output)
}

#' Helper Functions for Parameter Estimation
#' @description \code{quadrature} is a helper fucntion for getting hermite gauss quadrature points
#' @details 
#' \code{qudrature} was originally intended to call \code{gaussquad::hermite.h.quadrature.rules(12)[[12]]}
quadrature <- function () {
return(data.frame(x=c( 3.889725e+00, 3.020637e+00, 2.279507081, 1.59768264, 0.9477884, 0.3142404, -0.3142404, -0.9477884, -1.59768264, -2.279507081, -3.020637e+00, -3.889725e+00),
                  w=c(2.658552e-07, 8.573687e-05, 0.003905391, 0.05160799, 0.2604923, 0.5701352, 0.5701352, 0.2604923, 0.05160799, 0.003905391, 8.573687e-05, 2.658552e-07)))
}

#' @rdname estimate.people
#' @description \code{estimate.3pl.theta.eap} estimates people parameters with known item parameters using expected a posterior.
#' @details
#' For the expected a posteriori, refer to Baker and Kim (2004), pp. 193.
#' @export
estimate.people.3pl.eap <- function(responses, items){
  # validation
  inputs <- estimate.people.3pl.check.input(responses, items)
  responses <- as.matrix(inputs$responses)
  items <- inputs$items
  # inits
  n.items <- nrow(items)
  n.people <- nrow(responses)
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  # eap
  L <- sapply(X, function(x){
    irt <- irt.model(data.frame(theta=rep(x, n.people)), items, responses)
    irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
  })
  L.up <- L * matrix(rep(A * X, n.people), nrow=n.people, byrow=TRUE)
  L.up <- rowSums(L.up, na.rm=TRUE)
  L.down <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
  L.down <- rowSums(L.down, na.rm = TRUE)
  t <- L.up / L.down
  output <- list(people=data.frame(theta=t))
  return(output)
}

#' Estimation of Item Parameters
#' @description \code{estimate.items} is a wrapper function for estimating item parameters
#' @param responses a data frame of responses
#' @param people a data frame of people parameters 
#' @param model the IRT model
#' @param method the estimation method
#' @param ... further arguments
#' @return a data frame of estimated item parameters
#' @examples
#' \dontrun{
#' # data generation
#' data <- irt.model(model="3pl")$gen.data(2000, 50)
#' # JMLE
#' x <- estimate.items(data$responses, model="3pl", method="jmle", people=data$people, debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' # MMLE
#' x <- estimate.items(data$responses, model="3pl", method="mmle", debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' # BME
#' x <- estimate.items(data$responses, model="3pl", method="bme", debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' }
#' @export
estimate.items <- function(responses, model="3pl", method="jmle", ...){
  switch(tolower(model),
         "3pl" = switch(tolower(method),
                        "jmle" = estimate.items.3pl.jmle(responses, ...),
                        "mmle" = estimate.items.3pl.mmle(responses, ...),
                        "bme"  = estimate.items.3pl.bme(responses, ...)
         )
  )
}

#' @rdname quadrature
#' @description \code{estimate.items.3pl.check.input} is a helper function for validating inputs
#' @param people the people parameters
#' @return a list of validated \code{responses} and \code{people}
estimate.items.3pl.check.input <- function(responses, people){
  # convert vector to data frame
  if(is.vector(responses) && length(responses) == nrow(people))
    responses <- matrix(unlist(responses), ncol=1)
  # validate dimensions
  if(!is.null(people) && nrow(responses) != nrow(people))
      stop("response rows don't match people rows")
  # validate dichotomy
  if(any(responses != 0  & responses != 1 & !is.na(responses)))
    stop("response is not dichotomous")
  return(list(responses=responses, people=people))
}

#' @rdname quadrature
#' @description \code{estimate.items.3pl.init.par} is a helper function for initiating item parameters
#' @param n.items the number of items
#' @param fix the fixed values of parameters
#' @param init the initial values of parameters
#' @return a list of three initial parameters and three boolean vectors (whether fixed or not)
estimate.items.3pl.init.par <- function(fix, init, n.items){
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

#' @rdname estimate.items
#' @description \code{estimate.items.3pl.jmle} calibrates item parameters with knwon thetas using joint maximum likelihood.
#' @param fix a list of fixed item parameters
#' @param iter the maximum iterations
#' @param conv the convergence criterion
#' @param bound the bounds of item parameters
#' @param init a list of initial item parameters
#' @param debug \code{TRUE} to turn on debug mode
#' @return estimated item parameters and diagnosis data if debug mode is on
#' @details 
#' For the joint maximum likelihood estimation, refer to Baker and Kim (2004), pp. 46-54. \cr
#' To fixing item parameters, pass in a number or a vector (the same length with the items) to the \code{fix} argument.\cr
#' The debug mode prints convergence process and add \code{diagnosis} data to output
#' @examples
#' \dontrun{
#' # data generation
#' data <- irt.model(model="3pl")$gen.data(2000, 50)
#' # JMLE
#' x <- estimate.items.3pl.jmle(data$responses, data$people, debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' # MMLE
#' x <- estimate.items.3pl.mmle(data$responses, debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' # BME
#' x <- estimate.items.3pl.bme(data$responses, debug=TRUE)
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
#' }
#' @importFrom stats aggregate
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate.items.3pl.jmle <- function(responses, people, fix=list(), iter=30, conv=0.01, bound=list(a=2.0, b=3.5, c=0.25), init=list(a=1, b=0, c=0), debug=FALSE){
  # validation
  inputs <- estimate.items.3pl.check.input(responses, people)
  responses <- as.matrix(inputs$responses)
  people <- inputs$people
  # preps
  n.people <- nrow(responses)
  n.items <- ncol(responses)
  # group, and compute f and r
  theta <- factor(round(people$theta, 3))
  n <- length(levels(theta))
  f <- as.vector(table(theta))
  r <- aggregate(responses, list(theta), sum, na.rm=TRUE)[,-1]
  t <- as.numeric(levels(theta))
  # initial item parameters
  init.par <- estimate.items.3pl.init.par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  # initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  # iteration
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    p <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=c)), "probability")
    p.star <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=0)), "probability")
    a.matrix <- matrix(rep(a, n), nrow=n, byrow=TRUE)
    L2 <- -1.7 * a.matrix * (r - p * f) * (p.star / p)
    L22 <- -(1.7 * a.matrix)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L2, na.rm=TRUE) / colSums(L22, na.rm=TRUE)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * 0.5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    p <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=c)), "probability")
    p.star <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=0)), "probability")
    L1 <- (r - p * f) * outer(t, b, "-") * p.star / p
    L11 <- (-1) * f * outer(t, b, "-")^2 * p * (1 - p) * (p.star / p)^2
    h0 <- h.a
    h.a <- colSums(L1, na.rm=TRUE) / colSums(L11, na.rm=TRUE)
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    p <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=c)), "probability")
    p.star <- irt.stats(irt.model(data.frame(theta=t), data.frame(a=a, b=b, c=0)), "probability")
    c.matrix <- matrix(rep(c, n), nrow=n, byrow=TRUE)
    L3 <- (r - p * f) / (p - c.matrix) * (p.star / p)
    L33 <- (-1) * f * (1 - p) / (1 - c.matrix) / (p - c.matrix) * (p.star / p)
    h0 <- h.c
    h.c <- colSums(L3, na.rm=TRUE) / colSums(L33, na.rm=TRUE)
    h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c)
    c.new <- c - h.c
    c <- ifelse(c.fixed | c.new <= 0 | c.new > bound$c, c, c.new)
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      
    # convergence
    if(mean(abs(h.b)) <= conv) break;
  }
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  # debug graph
  if(debug){
    plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", xlim=c(0,iter), ylim=range(diagnosis$h), col=rgb(.8,.2,.2,.7))
    lines(diagnosis$h[,"a"], col=rgb(.2,.8,.2,.7))
    lines(diagnosis$h[,"c"], col=rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output) 
}

#' @rdname estimate.items
#' @description \code{estimate.items.3pl.mmle} calibrates item parameters without knowing people parameters using marginal maximum likelihood.
#' @details
#' For the marginal maximum likelihood estimation, refer to Baker and Kim (2004), pp.166-174.
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate.items.3pl.mmle <- function(responses, fix=list(), iter=30, conv=0.01, bound=list(a=2, b=3.5, c=.25), init=list(a=1, b=0, c=0), debug=FALSE){
  # validation
  inputs <- estimate.items.3pl.check.input(responses, NULL)
  responses <- as.matrix(inputs$responses)
  # preps
  n.people <- nrow(responses)
  n.items <- ncol(responses)
  # initial item parameters
  init.par <- estimate.items.3pl.init.par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  # initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  # gauss-hermite quadrature
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  # estimation
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    p.star <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=0), NULL), "probability")
    a.matrix <- matrix(rep(a, length(X)), nrow=length(X), byrow=TRUE)
    L2 <- -1.7 * a.matrix * (r - p * f) * (p.star / p)
    L22 <- -(1.7 * a.matrix)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L2, na.rm=TRUE) / colSums(L22, na.rm=TRUE)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * 0.5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=T)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    p.star <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=0), NULL), "probability")
    L1 <- (r - p * f) * outer(X, b, "-") * p.star / p
    L11 <- (-1) * f * outer(X, b, "-")^2 * p * (1 - p) * (p.star / p)^2
    h0 <- h.a
    h.a <- colSums(L1, na.rm=TRUE) / colSums(L11, na.rm=TRUE)
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a * 1.1)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=T)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    p.star <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=0), NULL), "probability")
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L3 <- (r - p * f) / (p - c.matrix) * (p.star / p)
    L33 <- (-1) * f * (1 - p) / (1 - c.matrix) / (p - c.matrix) * (p.star / p)
    h0 <- h.c
    h.c <- colSums(L3, na.rm=TRUE) / colSums(L33, na.rm=TRUE)
    h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c)
    c.new <- c - h.c
    c <- ifelse(c.fixed | c.new <= 0 | c.new > bound$c, c, c.new)
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      
    # convergence
    if(mean(abs(h.b)) <= conv) break;
  }
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  # debug graph
  if(debug){
    plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", xlim=c(0,iter), ylim=range(diagnosis$h), col=rgb(.8,.2,.2,.7))
    lines(diagnosis$h[,"a"], col=rgb(.2,.8,.2,.7))
    lines(diagnosis$h[,"c"], col=rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output)
}

#' @rdname estimate.items
#' @description \code{estimate.items.3pl.bme} calibrates item parameters without knowing people parameters using bayesian maximum likelihood.
#' @param prior a list of parameters for a-, b-, c-parameter prior distributions 
#' @details
#' For the Bayesian maximum likelihood estimation, refer to Baker and Kim (2004), pp.183-191.
#' In \code{estimate.items.3pl.bme}, a parameters is assumed to have a lognormal prior distribution, 
#' b parameters a normal prior distribution, and c parameters a beta prior distribution.
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate.items.3pl.bme <- function(responses, fix=list(), iter=30, conv=0.01, prior=list(a.mu=0, a.sig=0.2, b.mu=0, b.sig=1, c.alpha=5, c.beta=43), bound=list(a=2, b=3.5, c=0.25), init=list(a=1, b=0, c=0.1), debug=FALSE){
  # validation
  inputs <- estimate.items.3pl.check.input(responses, NULL)
  responses <- as.matrix(inputs$responses)
  # preps
  n.people <- nrow(responses)
  n.items <- ncol(responses)
  # initial item parameters
  init.par <- estimate.items.3pl.init.par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  # initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  # gauss-hermite quadrature
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  # estimation
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    p.star <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=0), NULL), "probability")
    w <- p.star * (1 - p.star) / p / (1 -p)
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L2 <- (-1) * 1.7 * a * (1 - c)  * colSums((r - f * p) * w) - (b - prior$b.mu) / prior$b.sig^2
    L22 <- (-1) * (1.7 * a)^2 * colSums(f * (p - c.matrix)^2 / (1 - c.matrix)^2 * (1 - p) / p) - 1 / prior$b.sig^2
    h0 <- h.b
    h.b <- L2 / L22
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * .5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    p.star <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=0), NULL), "probability")
    w <- p.star * (1 - p.star) / p / (1 -p)
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L1 <-  1.7 * a * (1 - c) * colSums((r - f * p) * outer(X, b, "-") * w) - (log(a) - prior$a.mu) / prior$a.sig^2
    L11 <- (-1) * (1.7 * a)^2 * colSums(f * outer(X, b, "-")^2 * (p - c.matrix)^2 / (1 - c.matrix)^2 * (1 - p) / p) - 1 / prior$a.sig^2
    h0 <- h.a
    h.a <- L1 / L11
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    L <- sapply(X, function(x){
      irt <- irt.model(data.frame(theta=rep(x, n.people)), data.frame(a=a, b=b, c=c), responses)
      irt.stats(irt, "likelihood", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt.stats(irt.model(data.frame(thetat=X), data.frame(a=a, b=b, c=c), NULL), "probability")
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L3 <- 1 / (1 - c) * colSums((r - f * p) / p) + ((prior$c.alpha - 2) / c - (prior$c.beta - 2) / (1 - c))
    L33 <- (-1) * colSums(f / (1 - c.matrix)^2 * (1 - p) / p) - (prior$c.alpha - 2) / c^2 - (prior$c.beta - 2) / (1 - c)^2
    h0 <- h.c
    h.c <- L3 / L33
    h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c)
    c.new <- c - h.c
    c <- ifelse(c.fixed | c.new <= 0 | c.new > bound$c, c, c.new)
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      
    # convergence
    if(mean(abs(h.b)) <= conv) break;
  }
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  # debug graph
  if(debug){
    plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", xlim=c(0,iter), ylim=range(diagnosis$h), col=rgb(.8,.2,.2,.7))
    lines(diagnosis$h[,"a"], col=rgb(.2,.8,.2,.7))
    lines(diagnosis$h[,"c"], col=rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output)
}

#' Estimation of People and Item Parameters
#' @description \code{estimate.3pl} estimates both people and item responses for given responses
#' @param responses a data frame of responses
#' @param ... other optional arguments
#' @return a 3PL \code{irt.model} object with estimated people and item parameters
#' @examples
#' \dontrun{
#' data <- irt.model(model="3pl")$gen.data(2000, 50)
#' x <- estimate.3pl(data$responses, debug=TRUE)
#' cor(data$people$theta, people$theta)
#' plot(data$people$theta, people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' abline(a=0, b=1, lty=2)
#' cor(data$items, items)
#' plot(data$items$a, items$a, xlim=c(0,2), ylim=c(0,2), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' abline(a=0, b=1, lty=2)
#' plot(data$items$b, items$b, xlim=c(-3,3), ylim=c(-3,3), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' abline(a=0, b=1, lty=2)
#' plot(data$items$c, items$c, xlim=c(0,.4), ylim=c(0,.4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' abline(a=0, b=1, lty=2)
#' }
#' @export
estimate.3pl <- function(responses, ...){
  items <- estimate.items(responses, model="3pl", method="mmle", ...)$items
  people <- estimate.people(responses, items, model="3pl", method="mle", ...)$people
  items <- estimate.items(responses, model="3pl", method="jmle", people=people, ...)$items
  people <- estimate.people(responses, items, model="3pl", method="mle", ...)$people
  x <- irt.model(people, items, responses, model="3pl")
  return(x)
}

