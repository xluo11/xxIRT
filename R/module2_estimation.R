#' Estimate People Parameters
#' @description Estimate people parameters when item parameters are known
#' @param responses a data frame or matrix of responses
#' @param items a data frame of item parameters 
#' @param model the IRT model
#' @param method the estimation method
#' @param ... further arguments
#' @return a data frame of estimated people parameters
#' @examples
#' \dontrun{
#' # data generation
#' library(ggplot2)
#' library(reshape2)
#' library(dplyr)
#' data <- irt_model("3pl")$gendata(500, 50)
#' 
#' # Estimate people parameters: MLE
#' x.mle <- estimate_people(data$responses, data$items, "3pl", "mle")
#' # Estimate people parameters: MAP
#' x.map <- estimate_people(data$responses, data$items, "3pl", "map")
#' # Estimate people parameters: EAP
#' x.eap <- estimate_people(data$responses, data$items, "3pl", "eap")
#' # Comparison with true parameters
#' x <- data.frame(true=data$people$theta, mle=x.mle$people$theta, 
#'   map=x.map$people$theta, eap=x.eap$people$theta)
#' round(t(apply(x, 2, function(v) c(R=cor(v, x$true), RMSE=rmse(v, x$true)))), 2)
#' melt(x, id.vars="true") %>% 
#'   ggplot(aes(x=true, y=value, color=variable)) + geom_point(pch=1) +
#'   facet_wrap(~variable, nrow=2) + xlab("True") + ylab("Est.") +
#'   theme_bw() + theme(legend.key=element_blank())
#' }
#' @export
estimate_people <- function(responses, items, model=c("3pl"), method=c("mle", "eap", "map"), ...){
  model <- match.arg(model)
  method <- match.arg(method)
  fun <- switch(model,
                "3pl" = switch(method, "mle" = estimate_people_3pl_mle, "map" = estimate_people_3pl_map, "eap" = estimate_people_3pl_eap)
  )
  fun(responses, items, ...)
}


#' @rdname estimate_people
#' @description \code{estimate_people_3pl_mle} is the maximum likelihood (ML) estimator of the people parameter.
#' @param init initial values of people parameters, a number or a vector
#' @param iter the maximum of Newton-Raphson iterations 
#' @param conv convergence criterion 
#' @param bound the bounds of estimated people parameters
#' @param debug \code{TRUE} to turn on debugging mode
#' @details
#' The debug mode draws a convergence curve. \cr
#' For the maximum likelihood (ML) estimation, refer to Baker and Kim (2004), pp. 66-69.
#' @importFrom graphics plot
#' @importFrom grDevices rgb
#' @export
estimate_people_3pl_mle <- function(responses, items, init=0, iter=30, conv=0.01, bound=3.5, debug=FALSE){
  # Validate inputs
  inputs <- estimate_people_3pl_check_input(responses, items)
  u <- inputs$responses
  items <- inputs$items
  
  # Initial values
  diagnosis <- list(h=rep(NA, iter))
  n.items <- nrow(items)
  n.people <- nrow(u)
  if(length(init) == 1) init <- rep(init, n.people)
  if(length(init) != n.people) stop("incorrect init length")
  t <- init
  a.mat <- matrix(rep(items$a, each=n.people), nrow=n.people)
  h <- rep(0, n.people)
  
  # Newton-Raphson
  if(debug) cat("Estimating:")
  for(i in 1:iter){
    if(debug) cat(".")
    p <- irt_stats(irt_model("3pl", theta=t, a=items$a, b=items$b, c=items$c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=t, a=items$a, b=items$b, c=0), "prob")
    L1 <- 1.7 * a.mat * (u - p) * p.star / p
    L2 <- (-1) * (1.7 * a.mat * p.star / p) ^ 2 * p * (1 - p)
    h0 <- h
    h <- rowSums(L1, na.rm=TRUE) / rowSums(L2, na.rm=TRUE)
    h <- ifelse(abs(h) > abs(h0), h * .5, h)
    t <- ifelse(abs(t - h) > bound, t, t - h)
    diagnosis$h[i] <- mean(abs(h))
    if(mean(abs(h)) <= conv) break
  }
  
  # Output
  output <- list(people=data.frame(theta=t))
  diagnosis$h <- diagnosis$h[1:i]
  if(debug) {
    cat("Done.\n")
    output$diagnosis <- diagnosis
    graphics::plot(diagnosis$h, type="l", xlim=c(0, iter), 
                   xlab="Iteration", ylab="Convergence", 
                   col=grDevices::rgb(.8,.2,.2,.7))
  }
  return(output)
}


#' @rdname estimate_people
#' @description \code{estimate_people_3pl_map} is the maximum a posteriori (MAP) estimator of people parameters
#' @param prior.mean the mean of the prior distribuiton
#' @param prior.sd the standard deviation of the prior distribution
#' @details
#' For the maximum a posteriori (MAP) estimation, refer to Baker and Kim (2004), pp. 192.
#' @importFrom graphics plot
#' @importFrom grDevices rgb
#' @export
estimate_people_3pl_map <- function(responses, items, prior.mean=0, prior.sd=1, init=0, iter=30, conv=0.01, bound=3.5, debug=FALSE){
  # Validate input
  inputs <- estimate_people_3pl_check_input(responses, items)
  u <- inputs$responses
  items <- inputs$items
  
  # Initial values
  diagnosis <- list(h=rep(NA, iter))
  n.items <- nrow(items)
  n.people <- nrow(u)
  if(length(init) == 1) init <- rep(init, n.people)
  if(length(init) != n.people) stop("incorrect init length")
  t <- init
  a.mat <- matrix(rep(items$a, each=n.people), nrow=n.people)
  c.mat <- matrix(rep(items$c, each=n.people), nrow=n.people)
  h <- rep(0, n.people)
  
  # Newton-Raphson
  if(debug) cat("Estimating:")
  for(i in 1:iter){
    if(debug) cat(".")
    p <- irt_stats(irt_model("3pl", theta=t, items=items), "prob")
    L1 <- (1.7 * a.mat) * (p - c.mat) / (p * (1 - c.mat)) * (u - p)
    L1 <- rowSums(L1, na.rm=TRUE) - (t - prior.mean) / prior.sd
    L2 <- (-1) * (1.7 * a.mat * (p - c.mat) / (1 - c.mat)) ^ 2 * (1 - p) / p
    L2 <- rowSums(L2, na.rm=TRUE) - 1 / prior.sd^2
    h0 <- h
    h <- L1 / L2
    h <- ifelse(abs(h) > abs(h0), h * .5, h)
    t <- ifelse(abs(t - h) > bound, t, t - h)
    diagnosis$h[i] <- mean(abs(h))
    if(mean(abs(h)) < conv) break
  }
  
  # Output
  output <- list(people=data.frame(theta=t))
  diagnosis$h <- diagnosis$h[1:i]
  if(debug){
    cat("Done.\n")
    output$diagnosis <- diagnosis
    graphics::plot(diagnosis$h, type="l", xlim=c(0, iter), 
                   xlab="Iteration", ylab="Convergence", 
                   col=grDevices::rgb(.8,.2,.2,.7))
  }
  return(output)
}


#' @rdname estimate_people
#' @description \code{estimate_people_3pl_eap} is the expected a priori (EAP) estimator of people parameters
#' @details
#' For the expected a priori (EAP), refer to Baker and Kim (2004), pp. 193.
#' @export
estimate_people_3pl_eap <- function(responses, items){
  # Validate inputs
  inputs <- estimate_people_3pl_check_input(responses, items)
  u <- inputs$responses
  items <- inputs$items
  
  # Initial values
  n.items <- nrow(items)
  n.people <- nrow(u)
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  
  # EAP
  L <- sapply(X, function(x){
    irt <- irt_model("3pl", theta=rep(x, n.people), items=items, responses=responses)
    irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
  })
  L.up <- L * matrix(rep(A * X, n.people), nrow=n.people, byrow=TRUE)
  L.up <- rowSums(L.up, na.rm=TRUE)
  L.down <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
  L.down <- rowSums(L.down, na.rm = TRUE)
  theta <- L.up / L.down
  output <- list(people=data.frame(theta=theta))
  return(output)
}


#' Estimate Item Parameters
#' @description Estimate item parameters
#' @param responses a data frame or matrix of responses
#' @param people a data frame of people parameters 
#' @param model the IRT model
#' @param method the estimation method
#' @param ... further arguments
#' @return a data frame of estimated item parameters
#' @examples
#' \dontrun{
#' # data generation
#' library(ggplot2)
#' library(reshape2)
#' library(dplyr)
#' data <- irt_model("3pl")$gendata(2000, 50)
#' 
#' # Estimate item parameters: JMLE
#' x.jmle <- estimate_items(data$responses, "3pl", "jmle", people=data$people)
#' # Estimate item parameters: MMLE
#' x.mmle <- estimate_items(data$responses, "3pl", "mmle")
#' # Estimate item parameters: BME
#' x.bme <- estimate_items(data$responses, "3pl", "bme")
#' # Comparison with true parameters
#' sapply(list(jmle=x.jmle, mmle=x.mmle, bme=x.bme), function(x) diag(cor(x$items, data$items)))
#' sapply(list(jmle=x.jmle, mmle=x.mmle, bme=x.bme), function(x) rmse(x$items, data$items))
#' x <- rbind(data.frame(method="jmle", melt(x.jmle$items), true=melt(data$items)$value),
#'            data.frame(method="mmle", melt(x.mmle$items), true=melt(data$items)$value),
#'            data.frame(method="bme", melt(x.bme$items), true=melt(data$items)$value))
#' ggplot(data=x, aes(x=true, y=value, color=method)) + geom_point(pch=1) +
#'   facet_grid(variable ~ method, scales="free") + xlab("True") + ylab("Est.") +
#'   theme_bw() + theme(legend.key=element_blank())
#' }
#' @export
estimate_items <- function(responses, model=c("3pl"), method=c("jmle", "mmle", "bme"), ...){
  model <- match.arg(model)
  method <- match.arg(method)
  fun <- switch(model,
                "3pl" = switch(method, "jmle" = estimate_items_3pl_jmle, "mmle" = estimate_items_3pl_mmle, "bme"  = estimate_items_3pl_bme)
  )
  fun(responses, ...)
}


#' @rdname estimate_items
#' @description \code{estimate_items_3pl_jmle} is the joint maximum likelihood estimator (JML) of item parameters
#' @param fix a list of fixed item parameters
#' @param iter the maximum iterations
#' @param conv the convergence criterion
#' @param bound the bounds of item parameters
#' @param init a list of initial item parameters
#' @param debug \code{TRUE} to turn on debugging mode
#' @return estimated item parameters and diagnosis data if debug mode is on
#' @details 
#' For the joint maximum likelihood estimation (JMLE), refer to Baker and Kim (2004), pp. 46-54. \cr
#' To fixing item parameters, pass in a number or a vector (the same length with the items) to the \code{fix} argument.\cr
#' The debug mode prints convergence process and add \code{diagnosis} data to output
#' @importFrom stats aggregate
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate_items_3pl_jmle <- function(responses, people, fix=list(), iter=30, conv=0.01, bound=list(a=2.0, b=3.5, c=0.25), init=list(a=1, b=0, c=0), debug=FALSE){
  # Validate input
  inputs <- estimate_items_3pl_check_input(responses, people)
  u <- inputs$responses
  people <- inputs$people
  
  # Constants
  n.people <- nrow(u)
  n.items <- ncol(u)
  
  # Group, and compute f and r
  t <- factor(round(people$t, 3))
  n <- length(levels(t))
  f <- as.vector(table(t))
  r <- stats::aggregate(u, list(t), sum, na.rm=TRUE)[,-1]
  t <- as.numeric(levels(t))
  
  # Initial item parameters
  init.par <- estimate_items_3pl_init_par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  
  # Initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  
  # Diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  
  # Newton-Raphson iteration
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    p <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=0), "prob")
    a.mat <- matrix(rep(a, n), nrow=n, byrow=TRUE)
    L2 <- -1.7 * a.mat * (r - p * f) * (p.star / p)
    L22 <- -(1.7 * a.mat)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L2, na.rm=TRUE) / colSums(L22, na.rm=TRUE)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * 0.5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    p <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=0), "prob")
    L1 <- (r - p * f) * outer(t, b, "-") * p.star / p
    L11 <- (-1) * f * outer(t, b, "-")^2 * p * (1 - p) * (p.star / p)^2
    h0 <- h.a
    h.a <- colSums(L1, na.rm=TRUE) / colSums(L11, na.rm=TRUE)
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    p <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=t, a=a, b=b, c=0), "prob")
    c.mat <- matrix(rep(c, n), nrow=n, byrow=TRUE)
    L3 <- (r - p * f) / (p - c.mat) * (p.star / p)
    L33 <- (-1) * f * (1 - p) / (1 - c.mat) / (p - c.mat) * (p.star / p)
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
  
  # Output
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  if(debug){
    graphics::plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", 
                   xlim=c(0,iter), ylim=range(diagnosis$h), col=grDevices::rgb(.8,.2,.2,.7))
    graphics::lines(diagnosis$h[,"a"], col=grDevices::rgb(.2,.8,.2,.7))
    graphics::lines(diagnosis$h[,"c"], col=grDevices::rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output) 
}


#' @rdname estimate_items
#' @description \code{estimate_items_3pl_mmle} is the marginal maximum likelihood estimator (MMLE) of item parameters
#' @details
#' For the marginal maximum likelihood estimation (MMLE), refer to Baker and Kim (2004), pp.166-174.
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate_items_3pl_mmle <- function(responses, fix=list(), iter=30, conv=0.01, bound=list(a=2, b=3.5, c=.25), 
                                    init=list(a=1, b=0, c=0), debug=FALSE){
  # Validate input
  inputs <- estimate_items_3pl_check_input(responses, NULL)
  u <- inputs$responses
  
  # Constants
  n.people <- nrow(u)
  n.items <- ncol(u)
  
  # Initial item parameters
  init.par <- estimate_items_3pl_init_par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  
  # Initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  
  # Gauss-hermite quadrature
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  
  # Diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  
  # Estimation
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(u), 0, u)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=0), "prob")
    a.mat <- matrix(rep(a, length(X)), nrow=length(X), byrow=TRUE)
    L2 <- -1.7 * a.mat * (r - p * f) * (p.star / p)
    L22 <- -(1.7 * a.mat)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L2, na.rm=TRUE) / colSums(L22, na.rm=TRUE)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * 0.5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(u), 0, u)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=0), "prob")
    L1 <- (r - p * f) * outer(X, b, "-") * p.star / p
    L11 <- (-1) * f * outer(X, b, "-")^2 * p * (1 - p) * (p.star / p)^2
    h0 <- h.a
    h.a <- colSums(L1, na.rm=TRUE) / colSums(L11, na.rm=TRUE)
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a * 1.1)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(u), 0, u)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=0), "prob")
    c.mat <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L3 <- (r - p * f) / (p - c.mat) * (p.star / p)
    L33 <- (-1) * f * (1 - p) / (1 - c.mat) / (p - c.mat) * (p.star / p)
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
  
  # Output
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  if(debug){
    graphics::plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", 
                   xlim=c(0,iter), ylim=range(diagnosis$h), col=grDevices::rgb(.8,.2,.2,.7))
    graphics::lines(diagnosis$h[,"a"], col=grDevices::rgb(.2,.8,.2,.7))
    graphics::lines(diagnosis$h[,"c"], col=grDevices::rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output)
}


#' @rdname estimate_items
#' @description \code{estimate_items_3pl_bme} is the bayesian estimator of item parameters
#' @param prior a list of prior distributions parameters
#' @details
#' For the Bayesian estimation, refer to Baker and Kim (2004), pp.183-191.
#' In \code{estimate_items_3pl_bme}, a parameters is assumed to have a lognormal prior distribution, 
#' b parameters a normal prior distribution, and c parameters a beta prior distribution.
#' @importFrom graphics plot lines
#' @importFrom grDevices rgb
#' @export
estimate_items_3pl_bme <- function(responses, fix=list(), iter=30, conv=0.01, 
                                   prior=list(a.mean=0, a.sd=0.2, b.mean=0, b.sd=1, c.alpha=5, c.beta=43), 
                                   bound=list(a=2, b=3.5, c=0.25), init=list(a=1, b=0, c=0.1), debug=FALSE){
  # Validate inputs
  inputs <- estimate_items_3pl_check_input(responses, NULL)
  u <- inputs$responses
  
  # Constants
  n.people <- nrow(u)
  n.items <- ncol(u)
  
  # Initial item parameters
  init.par <- estimate_items_3pl_init_par(fix, init, n.items)
  a <- init.par$a
  b <- init.par$b
  c <- init.par$c
  a.fixed <- init.par$a.fixed
  b.fixed <- init.par$b.fixed
  c.fixed <- init.par$c.fixed
  
  # Initial learning rates
  h.a <- h.b <- h.c <- rep(0, n.items)
  
  # Gauss-hermite quadrature
  gauss.hermite <- quadrature()
  X <- gauss.hermite$x
  A <- gauss.hermite$w
  
  # Diagnosis information
  diagnosis <- list(h=matrix(nrow=iter, ncol=3, dimnames=list(paste("iter",1:iter,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iter, n.items), dimnames=list(c("a","b","c"), paste("iter",1:iter,sep=""), paste("item",1:n.items,sep=""))))
  
  # estimation
  if(debug) cat("Calibrating:")
  for(i in 1:iter){
    if(debug) cat(".")
    # b parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=0), "prob")
    w <- p.star * (1 - p.star) / p / (1 -p)
    c.mat <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L2 <- (-1) * 1.7 * a * (1 - c)  * colSums((r - f * p) * w) - (b - prior$b.mean) / prior$b.sd^2
    L22 <- (-1) * (1.7 * a)^2 * colSums(f * (p - c.mat)^2 / (1 - c.mat)^2 * (1 - p) / p) - 1 / prior$b.sd^2
    h0 <- h.b
    h.b <- L2 / L22
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * .5, h.b)
    b.new <- b - h.b
    b <- ifelse(b.fixed | abs(b.new) > bound$b, b, b.new)
    # a parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    p.star <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=0), "prob")
    w <- p.star * (1 - p.star) / p / (1 -p)
    c.mat <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L1 <-  1.7 * a * (1 - c) * colSums((r - f * p) * outer(X, b, "-") * w) - (log(a) - prior$a.mean) / prior$a.sd^2
    L11 <- (-1) * (1.7 * a)^2 * colSums(f * outer(X, b, "-")^2 * (p - c.mat)^2 / (1 - c.mat)^2 * (1 - p) / p) - 1 / prior$a.sd^2
    h0 <- h.a
    h.a <- L1 / L11
    h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a)
    a.new <- a - h.a
    a <- ifelse(a.fixed | a.new <= 0 | a.new > bound$a, a, a.new)
    # c parameters
    L <- sapply(X, function(x){
      irt <- irt_model("3pl", theta=rep(x, n.people), a=a, b=b, c=c, responses=u)
      irt_stats(irt, "lik", summary="people", fun=prod, na.rm=TRUE)
    })
    P <- L * matrix(rep(A, n.people), nrow=n.people, byrow=TRUE)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% ifelse(is.na(responses), 0, responses)
    p <- irt_stats(irt_model("3pl", theta=X, a=a, b=b, c=c), "prob")
    c.mat <- matrix(rep(c, length(X)), nrow=length(X), byrow=TRUE)
    L3 <- 1 / (1 - c) * colSums((r - f * p) / p) + ((prior$c.alpha - 2) / c - (prior$c.beta - 2) / (1 - c))
    L33 <- (-1) * colSums(f / (1 - c.mat)^2 * (1 - p) / p) - (prior$c.alpha - 2) / c^2 - (prior$c.beta - 2) / (1 - c)^2
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
  
  # Output
  if(debug) cat("Done.\n")
  output <- list(items=data.frame(a=a, b=b, c=c))
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  if(debug){
    graphics::plot(diagnosis$h[,"b"], type="l", xlab="Iteration", ylab="Convergence", 
                   xlim=c(0, iter), ylim=range(diagnosis$h), col=grDevices::rgb(.8,.2,.2,.7))
    graphics::lines(diagnosis$h[,"a"], col=grDevices::rgb(.2,.8,.2,.7))
    graphics::lines(diagnosis$h[,"c"], col=grDevices::rgb(.2,.2,.8,.7))
    output$diagnosis <- diagnosis
  }
  return(output)
}


#' Estimate Both People and Item Parameters
#' @description Estimates both people and item responses using MMLE and JMLE
#' @param responses a data frame of responses
#' @param ... other optional arguments
#' @return a 3PL model with estimated people and item parameters
#' @examples
#' \dontrun{
#' data <- irt_model("3pl")$gendata(2000, 50)
#' x <- estimate_3pl(data$responses, debug=TRUE)
#' cor(data$people$theta, x$people$theta)
#' plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' cor(data$items, x$items)
#' plot(data$items$a, x$items$a, xlim=c(0,2), ylim=c(0,2), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' plot(data$items$b, x$items$b, xlim=c(-3,3), ylim=c(-3,3), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' plot(data$items$c, x$items$c, xlim=c(0,.4), ylim=c(0,.4), 
#' xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.7))
#' }
#' @export
estimate_3pl <- function(responses, ...){
  items <- estimate_items(responses, model="3pl", method="mmle", ...)$items
  people <- estimate_people(responses, items, model="3pl", method="mle", ...)$people
  items <- estimate_items(responses, model="3pl", method="jmle", people=people, ...)$items
  people <- estimate_people(responses, items, model="3pl", method="mle", ...)$people
  x <- irt_model("3pl", people=people, items=items, responses=responses)
  return(x)
}

