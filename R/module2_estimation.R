#' Estimation of 3PL Model
#' @name estimation
NULL

#' @rdname estimation
#' @description \code{estimate_mle} estimates parameters using joint or maximum likelihood estimation method
#' @param u a matrix of response data
#' @param t theta parameters
#' @param a a parameters
#' @param b b parameters
#' @param c c parameters
#' @param iter the number of maximum iterations
#' @param conv the convergence criterion
#' @param method the estimation method of item parameters
#' @param bound_t the bound of theta parameters
#' @param bound_a the bound of a parameters
#' @param bound_b the bound of b parameters
#' @param bound_c the bound of c parameters
#' @param mmle_mu the mean of the marginal distribution in MMLE
#' @param mmle_sig the SD of the marginal parameters in MMLE
#' @param scale the scaling parameter
#' @param scale_mean the mean of the scale
#' @param scale_sd the SD of the scale
#' @param debug \code{TRUE} to print and report debugging information
#' @return a list of \code{t, a, b, c} parameters
#' @details 
#' When the \code{t, a, b, c} parameters are \code{NULL}, they are free 
#' to be estimated; otherwise, they are fixed at the provided values. 
#' When setting values for a parameter, use numeric values to fix parameters
#' and \code{NA} to free parameters. For instance, an argument of \code{t=c(-1, NA, 1)}
#' means to fix the 1st and 3rd theta parameters to -1 and 1 and estimate the 2nd theta
#' parameters. The same is true for the a-, b-, and c-parameters. \cr
#' The \code{method} argument in \code{estimate_mle} controls whether to use joint (\code{jmle}) 
#' or maximum (\code{mmle}) likelihood method to estimate item parameters. The \code{scale} 
#' argument controls where to set the scale: \code{b} or \code{theta} parameters. \cr
#' When \code{debug} mode is on, print and report additional information regarding the convergence
#' over the iterations. \cr
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' set.seed(10001)
#' ### generate data
#' data <- model_3pl()$gendata(1000, 40)
#' ### MLE
#' x <- estimate_mle(data$responses, debug=TRUE)
#' y <- rbind(data.frame(param='t', true=data$people$theta, est=x$t),
#'      data.frame(param='a', true=data$items$a, est=x$a),
#'      data.frame(param='b', true=data$items$b, est=x$b),
#'      data.frame(param='c', true=data$items$c, est=x$c))
#' group_by(y, param) %>% 
#' summarise(corr=cor(true, est), rmse=rmse(true, est))
#' ggplot(y, aes(x=true, y=est, color=param)) + 
#'   geom_point(alpha=.5) + facet_wrap(~param, scales="free") + 
#'   xlab("True Parameters") + ylab("Estimated Parameters") +
#'   theme_bw()
#' group_by(y, param) %>% summarise(corr=cor(true, est), 
#'     rmse=rmse(true, est), mean=mean(est), sd=sd(est))
#' ### Bayesian
#' x <- estimate_bayesian(data$responses, debug=TRUE)
#' y <- rbind(data.frame(param='t', true=data$people$theta, est=x$t),
#'      data.frame(param='a', true=data$items$a, est=x$a),
#'      data.frame(param='b', true=data$items$b, est=x$b),
#'      data.frame(param='c', true=data$items$c, est=x$c))
#' group_by(y, param) %>% summarise(corr=cor(true, est), rmse=rmse(true, est))
#' ggplot(y, aes(x=true, y=est, color=param)) + 
#'   geom_point(alpha=.5) + facet_wrap(~param, scales="free") + 
#'   xlab("True Parameters") + ylab("Estimated Parameters") +
#'   theme_bw()
#' group_by(y, param) %>% summarise(corr=cor(true, est), 
#'     rmse=rmse(true, est), mean=mean(est), sd=sd(est))
#' }
#' @import ggplot2
#' @importFrom stats sd dnorm
#' @importFrom reshape2 melt
#' @export
estimate_mle <- function(u, t=NULL, a=NULL, b=NULL, c=NULL, iter=20, conv=0.005, method=c("jmle", "mmle"), bound_t=3.5, bound_a=2, bound_b=3.5, bound_c=0.25, mmle_mu=0, mmle_sig=1, scale=c("none", "theta", "b"), scale_mean=0, scale_sd=1, debug=FALSE){
  method <- match.arg(method)
  scale <- match.arg(scale)
  u <- as.matrix(u)
  n.people <- dim(u)[1]
  n.items <- dim(u)[2]
  
  inputs <- estimate_check_input(u, t, a, b, c)
  t <- inputs$t
  a <- inputs$a
  b <- inputs$b
  c <- inputs$c
  t_fixed <- inputs$t_fixed
  a_fixed <- inputs$a_fixed
  b_fixed <- inputs$b_fixed
  c_fixed <- inputs$c_fixed
  
  if(method == "mmle") {
    X <- seq(-3, 3, .5)
    A <- dnorm(X, mean=mmle_mu, sd=mmle_sig)
  }
  h_t <- ifelse(t_fixed, 0, 1)
  h_a <- ifelse(a_fixed, 0, 1)
  h_b <- ifelse(b_fixed, 0, 1)
  h_c <- ifelse(c_fixed, 0, 1)
  debugger <-matrix(nrow=iter, ncol=4, dimnames=list(1:iter, c('t', 'a', 'b', 'c')))
  
  if(debug) cat(method, "- estimating:")
  for(k in seq(iter)){
    if(debug) cat('.')
    # update b
    if(any(!b_fixed)){
      if(method == "jmle"){
        p <- irt_stats(model_3pl(theta=t, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=t, a=a, b=b, c=0), "prob")
        f1 <- -1 * ((u - p) * p_alt / p)
        f1 <- ifelse(is.na(f1), 0, f1) %*% diag(1.7 * a, nrow=n.items)
        f2 <- -1 * (p * (1 - p) * p_alt / p) %*% diag(1.7 * a, nrow=n.items)^2
      } else if(method == "mmle") {
        L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
        P <- L %*% diag(A)
        P <- P / rowSums(P)
        f <- colSums(P)
        r <- t(P) %*% ifelse(is.na(u), 0, u)
        p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
        f1 <- -1 * ((r - p * f) * p_alt / p) %*% diag(1.7 * a, nrow=n.items)
        f2 <- -1 * (f * p * (1 - p) * p_alt / p) %*% diag(1.7 * a, nrow=n.items)^2
      } else {
        stop("invalid estimation method for items")
      }
      h <- h_b
      h_b <- colSums(f1, na.rm=TRUE) / colSums(f2, na.rm=TRUE)
      h_b <- ifelse(abs(h_b) > h, 0.5, 1.0) * h_b
      h_b[b_fixed] <- 0
      b <- b - h_b
      b <- ifelse(abs(b) > bound_b, sign(b) * bound_b, b)
      b[is.na(b)] <- 0
      if(scale == "b") b[!b_fixed] <- (b[!b_fixed] - mean(b[!b_fixed])) / sd(b[!b_fixed]) * scale_sd + scale_mean
    }
    # update a
    if(any(!a_fixed)){
      if(method == "jmle") {
        p <- irt_stats(model_3pl(theta=t, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=t, a=a, b=b, c=0), "prob")
        f1 <- (u - p) * outer(t, b, '-') * p_alt / p
        f2 <- -1 * outer(t, b, '-')^2 * p * (1 - p) * (p_alt / p)^2        
      } else if(method == "mmle") {
        L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
        P <- L %*% diag(A)
        P <- P / rowSums(P)
        f <- colSums(P)
        r <- t(P) %*% ifelse(is.na(u), 0, u)
        p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
        f1 <- (r - p * f) * outer(X, b, '-') * p_alt / p
        f2 <- -1 * f * outer(X, b, '-')^2 * p * (1 - p) * (p_alt / p)^2
      } else {
        stop("invalid estimation method for items")
      }
      h <- h_a
      h_a <- colSums(f1, na.rm=TRUE) / colSums(f2, na.rm=TRUE)
      h_a <- ifelse(abs(h_a) > h, 0.5, 1.0) * h_a
      h_a[a_fixed] <- 0
      a <- a - h_a
      a <- ifelse(a <= 0, 0.01, a)
      a <- ifelse(a > bound_a, bound_a, a)
      a[is.na(a)] <- 1
    }
    # update c
    if(any(!c_fixed)){
      if(method == "jmle") {
        p <- irt_stats(model_3pl(theta=t, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=t, a=a, b=b, c=0), "prob")
        c_mat <- matrix(1, nrow=n.people) %*% matrix(c, nrow=1)
        f1 <- (u - p) / (p - c_mat) * p_alt / p
        f2 <- -1 * (1 - p) / (1 - c_mat) / (p - c_mat) * p_alt / p
      } else if(method == "mmle") {
        L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
        P <- L %*% diag(A)
        P <- P / rowSums(P)
        f <- colSums(P)
        r <- t(P) %*% ifelse(is.na(u), 0, u)
        p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
        p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
        c_mat <- matrix(1, nrow=length(X)) %*% matrix(c, nrow=1)
        f1 <- (r - p * f) / (p - c_mat) * p_alt / p
        f2 <- -1 * f * (1 - p) / (1 - c_mat) / (p - c_mat) * p_alt / p
      } else {
        stop("invalid estimation method for items")
      }
      h <- h_c
      h_c <- colSums(f1, na.rm=TRUE) / colSums(f2, na.rm=TRUE)
      h_c <- ifelse(abs(h_c) > h, 0.5, 1.0) * h_c
      h_c[c_fixed] <- 0
      c <- c - h_c
      c <- ifelse(c < 0, 0, c)
      c <- ifelse(c > bound_c, bound_c, c)
      c[is.na(c)] <- 0
    }
    # update t
    if(any(!t_fixed)){
      p <- irt_stats(model_3pl(theta=t, a=a, b=b, c=c), "prob")
      p_alt <- irt_stats(model_3pl(theta=t, a=a, b=b, c=0), "prob")
      L1 <- ((u - p) * p_alt / p) 
      L1 <- ifelse(is.na(L1), 0, L1) %*% diag(1.7 * a, nrow=n.items)
      L2 <- -1 * ((p_alt / p) %*% diag(1.7 * a, nrow=n.items))^2 * p * (1 - p)
      h <- h_t
      h_t <- rowSums(L1) / rowSums(L2)
      h_t <- ifelse(abs(h_t) > h, 0.5, 1.0) * h_t
      h_t[t_fixed] <- 0
      t <- t - h_t
      t <- ifelse(abs(t) > bound_t, sign(t) * bound_t, t)
      t[is.na(t)] <- 0
      if(scale == "theta") t[!t_fixed] <- (t[!t_fixed] - mean(t[!t_fixed])) / sd(t[!t_fixed]) * scale_sd + scale_mean
    }
    # debug
    h <- c(mean(abs(h_t[!t_fixed])), mean(abs(h_a[!a_fixed])), mean(abs(h_b[!b_fixed])), mean(abs(h_c[!c_fixed])))
    h[is.nan(h) | is.na(h)] <- 0
    debugger[k, ] <- h
    # to break?
    if(all(debugger[k, c("t", "b")] <= conv)) break
  }
  output <- list(t=t, a=a, b=b, c=c)
  if(debug){
    cat('\n')
    x <- cbind(debugger, k=1:iter)[1:k, ]
    x <- melt(as.data.frame(x), id.vars="k", variable.name="param")
    g <- ggplot(x, aes_string(x="k", y="value", color="param")) +
      geom_line() + xlab("iterations") + ylab("convergence") + 
      coord_cartesian(xlim=c(1, iter)) + theme_bw()
    print(g)
    output <- c(output, list(conv=debugger[1:k,], graph=g))
  }
  return(output)
}


#' @rdname estimation
#' @description \code{estimate_bayesian} estimates parameters using Bayesian estimation method
#' @param t_mu the mean of the prior distribution of theta parameters
#' @param t_sig the SD of the prior distribution of theta parameters
#' @param a_mu the mean of the prior distribution of a parameters
#' @param a_sig the SD of the prior distribution of a parameters
#' @param b_mu the mean of the prior distribution of b parameters
#' @param b_sig the SD of the prior distribution of b parameters
#' @param c_alpha the alpha of the prior distribution of c parameters
#' @param c_beta the beta of the prior distribution of c parameters
#' @param report_sd \code{TRUE} to report the posterior variance of thetas in EAP
#' @details 
#' The \code{method} argument in \code{estimate_bayesian} controls whether to use 
#' maximum (\code{map}) or expected (\code{eap}) a posteriori to estimate theta parameters. \cr 
#' @import ggplot2
#' @importFrom stats sd dnorm
#' @importFrom reshape2 melt
#' @export
estimate_bayesian <- function(u, t=NULL, a=NULL, b=NULL, c=NULL, method=c("map", "eap"), iter=20, conv=0.005, bound_t=3.5, bound_a=2, bound_b=3.5, bound_c=0.25, scale=c("none", "theta", "b"), scale_mean=0, scale_sd=1, t_mu=0, t_sig=1, a_mu=0, a_sig=0.2, b_mu=0, b_sig=1, c_alpha=5, c_beta=46, report_sd=FALSE, debug=FALSE) {
  method <- match.arg(method)
  scale <- match.arg(scale)
  u <- as.matrix(u)
  n.people <- dim(u)[1]
  n.items <- dim(u)[2]
  
  inputs <- estimate_check_input(u, t, a, b, c)
  t <- inputs$t
  a <- inputs$a
  b <- inputs$b
  c <- inputs$c
  t_fixed <- inputs$t_fixed
  a_fixed <- inputs$a_fixed
  b_fixed <- inputs$b_fixed
  c_fixed <- inputs$c_fixed
  c[!c_fixed] <- 0.01
  
  X <- seq(-3, 3, .5)
  A <- dnorm(X, mean=t_mu, sd=t_sig)
  h_t <- ifelse(t_fixed, 0, 1)
  h_a <- ifelse(a_fixed, 0, 1)
  h_b <- ifelse(b_fixed, 0, 1)
  h_c <- ifelse(c_fixed, 0, 1)
  debugger <-matrix(nrow=iter, ncol=4, dimnames=list(1:iter, c('t', 'a', 'b', 'c')))
  
  if(debug) cat(method, "- estimating:")
  for(k in seq(iter)){
    if(debug) cat('.')
    # update b
    if(any(!b_fixed)){
      L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
      P <- L %*% diag(A)
      P <- P / rowSums(P)
      f <- colSums(P)
      r <- t(P) %*% ifelse(is.na(u), 0, u)
      p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
      p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
      w <- p_alt * (1 - p_alt) / p / (1 -p)
      c_mat <- matrix(1, nrow=length(X)) %*% c
      f1 <- -1 * 1.7 * a * (1 - c) * colSums((r - p * f) * w) - (b - b_mu) / b_sig^2
      f2 <- -1 * (1.7 * a)^2 * colSums(f * (p - c_mat)^2 / (1 - c_mat)^2 * (1 - p) / p) - 1 / b_sig^2
      h <- h_b
      h_b <- f1 / f2
      h_b <- ifelse(abs(h_b) > h, 0.5, 1.0) * h_b
      h_b[b_fixed] <- 0
      b <- b - h_b
      b <- ifelse(abs(b) > bound_b, sign(b) * bound_b, b)
      b[is.na(b)] <- 0
      if(scale == "b") b[!b_fixed] <- (b[!b_fixed] - mean(b[!b_fixed])) / sd(b[!b_fixed]) * scale_sd + scale_mean
    }
    # update a
    if(any(!a_fixed)){
      L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
      P <- L %*% diag(A)
      P <- P / rowSums(P)
      f <- colSums(P)
      r <- t(P) %*% ifelse(is.na(u), 0, u)
      p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
      p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
      w <- p_alt * (1 - p_alt) / p / (1 - p)
      c_mat <- matrix(1, nrow=length(X)) %*% c
      f1 <- 1.7 * a * (1 - c) * colSums((r - p * f) * outer(X, b, '-') * w) - (log(a) - a_mu) / a_sig^2
      f2 <- -1 * (1.7 * a)^2 * colSums(f * outer(X, b, '-')^2 * (p - c_mat)^2 / (1 - c_mat)^2 * (1 - p) / p) - 1 / a_sig^2
      h <- h_a
      h_a <- f1 / f2
      h_a <- ifelse(abs(h_a) > h, 0.5, 1.0) * h_a
      h_a[a_fixed] <- 0
      a <- a - h_a
      a <- ifelse(a <= 0, 0.01, a)
      a <- ifelse(a > bound_a, bound_a, a)
      a[is.na(a)] <- 1
    }
    # update c
    if(any(!c_fixed)){
      L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
      P <- L %*% diag(A)
      P <- P / rowSums(P)
      f <- colSums(P)
      r <- t(P) %*% ifelse(is.na(u), 0, u)
      p <- irt_stats(model_3pl(theta=X, a=a, b=b, c=c), "prob")
      p_alt <- irt_stats(model_3pl(theta=X, a=a, b=b, c=0), "prob")
      c_mat <- matrix(1, nrow=length(X)) %*% c
      f1 <- 1 / (1 - c) * colSums((r - p * f) / p) + (c_alpha - 2) / c - (c_beta - 2) / (1 - c)
      f2 <- -1 * colSums(f / (1 - c_mat)^2 * (1 - p) / p) - (c_alpha - 2) / c^2 - (c_beta - 2) / (1 - c)^2
      h <- h_c
      h_c <- f1 / f2
      h_c <- ifelse(abs(h_c) > h, 0.5, 1.0) * h_c
      h_c[c_fixed] <- 0
      c <- c - h_c
      c <- ifelse(c < 0, 0, c)
      c <- ifelse(c > bound_c, bound_c, c)
      c[!c_fixed & (is.na(c) | c == 0)] <- 0.01
    }
    # update t
    if(any(!t_fixed)){
      if(method == "map"){
        p <- irt_stats(model_3pl(theta=t, a=a, b=b, c=c), "prob")
        f1 <- (sweep(p, MARGIN=2, c, FUN='-') * (u - p) / (p %*% diag(1 - c, nrow=n.items))) 
        f1 <- ifelse(is.na(f1), 0, f1) %*% diag(1.7 * a, nrow=n.items)
        f1 <- rowSums(f1) - (t - t_mu) / t_sig 
        f2 <- -1 * (sweep(p, MARGIN=2, c, FUN='-') %*% diag(1.7 * a / (1 - c), nrow=n.items))^2 * (1 - p) / p
        f2 <- rowSums(f2) - 1 / t_sig^2
        h <- h_t
        h_t <- f1 / f2
        h_t <- ifelse(abs(h_t) > h, 0.5, 1.0) * h_t
        h_t[t_fixed] <- 0
        t <- t - h_t
      } else if(method == "eap"){
        L <- sapply(X, function(x){irt_stats(model_3pl(theta=rep(x, n.people), a=a, b=b, c=c, responses=u), "lik", summary="people", fun=prod, na.rm=TRUE)})
        p <- L %*% diag(A) 
        p <- p / rowSums(p)
        theta <- as.vector(p %*% matrix(X))
        h_t <- ifelse(t_fixed, 0, t - theta)
        t[!t_fixed] <- theta[!t_fixed]
        v <- diag(p %*% outer(X, t, '-')^2)
      } else {
        stop("invalid estimation method for thetas")
      }
      t <- ifelse(abs(t) > bound_t, sign(t) * bound_t, t)
      if(scale == "theta") t[!t_fixed] <- (t[!t_fixed] - mean(t[!t_fixed])) / sd(t[!t_fixed]) * scale_sd + scale_mean
    }
    # debug
    h <- c(mean(abs(h_t[!t_fixed])), mean(abs(h_a[!a_fixed])), mean(abs(h_b[!b_fixed])), mean(abs(h_c[!c_fixed])))
    h[is.nan(h) | is.na(h)] <- 0
    debugger[k, ] <- h
    # to break?
    if(all(debugger[k, c("t", "b")] <= conv)) break
  }
  output <- list(t=t, a=a, b=b, c=c)
  if(method == "eap" && report_sd) {
    output <- c(output, list(var=v))
  }
  if(debug){
    cat('\n')
    x <- cbind(debugger, k=1:iter)[1:k, ]
    x <- melt(as.data.frame(x), id.vars="k", variable.name="param")
    g <- ggplot(x, aes_string(x="k", y="value", color="param")) +
      geom_line() + xlab("iterations") + ylab("convergence") + 
      coord_cartesian(xlim=c(1, iter)) + theme_bw()
    print(g)
    output <- c(output, list(conv=debugger[1:k,], graph=g))
  }
  return(output)
}

#' @rdname estimation
#' @description a helper function to process input arguments
estimate_check_input <- function(u, t, a, b, c) {
  u <- as.matrix(u)
  n.people <- dim(u)[1]
  n.items <- dim(u)[2]
  # initiate t, t_fixed
  if(is.null(t)) {
    t <- rep(0, n.people)
    t_fixed <- rep(FALSE, n.people)
  } else if(length(t) == 1) {
    t <- rep(t, n.people)
    t_fixed <- rep(TRUE, n.people)
  } else if(length(t) == n.people) {
    index <- is.null(t) | is.na(t)
    t[index] <- 0
    t_fixed <- rep(TRUE, n.people)
    t_fixed[index] <- FALSE
  } else {
    stop("invalid theta parameters")
  }
  # initiate a, a_fixed
  if(is.null(a)) {
    a <- rep(1, n.items)
    a_fixed <- rep(FALSE, n.items)
  } else if(length(a) == 1) {
    a <- rep(a, n.items)
    a_fixed <- rep(TRUE, n.items)
  } else if(length(a) == n.items) {
    index <- is.null(a) | is.na(a)
    a[index] <- 1
    a_fixed <- rep(TRUE, n.items)
    a_fixed[index] <- FALSE
  } else {
    stop("invalid a-parameters")
  }  
  # initiate b, b_fixed
  if(is.null(b)) {
    b <- rep(0, n.items)
    b_fixed <- rep(FALSE, n.items)
  } else if(length(b) == 1) {
    b <- rep(b, n.items)
    b_fixed <- rep(TRUE, n.items)
  } else if(length(b) == n.items) {
    index <- is.null(b) | is.na(b)
    b[index] <- 0
    b_fixed <- rep(TRUE, n.items)
    b_fixed[index] <- FALSE
  } else {
    stop("invalid b-parameters")
  }
  # initiate c, c_fixed
  if(is.null(c)) {
    c <- rep(0, n.items)
    c_fixed <- rep(FALSE, n.items)
  } else if(length(c) == 1) {
    c <- rep(c, n.items)
    c_fixed <- rep(TRUE, n.items)
  } else if(length(c) == n.items) {
    index <- is.null(c) | is.na(c)
    c[index] <- 0
    c_fixed <- rep(TRUE, n.items)
    c_fixed[index] <- FALSE
  } else {
    stop("invalid a-parameters")
  }
  return(list(t=t, a=a, b=b, c=c, t_fixed=t_fixed, a_fixed=a_fixed, b_fixed=b_fixed, c_fixed=c_fixed))
}

