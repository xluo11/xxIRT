#' Estimation of Ability Parameters
#' @description \code{estimate.theta.mle} estimates thetas with known item parameters using maximum likelihood.
#' @param u a response matrix, with people in rows and items in columns
#' @param a a vector of item discrimination parameters
#' @param b a vector of item difficulty parameters
#' @param c a vector of item pseudo-guesing parameters
#' @param init a vectoro of initial value for theta parmaeters
#' @param iteration the maximum iterations of Newton-Raphson procedure
#' @param delta convergence criterion used to terminate Newton-Raphson procedure
#' @param bound the maximum absolute values of estimated theta parameters
#' @return a vector of estimated thetas
#' @details
#' For the maximum likelihood estimation, refer to Baker and Kim (2004), pp. 66-69.
#' @examples
#' # MLE
#' x <- gen.rsp(gen.irt(100, 20))
#' y <- estimate.theta.mle(x$rsp, x$items$a, x$items$b, x$items$c)
#' cor(x$thetas, y)
#' plot(x$thetas, y, xlim=c(-4, 4), ylim=c(-4, 4), col=rgb(.8,.2,.2,.5), pch=16)
#' abline(a=0, b=1)
#' @family estimation
#' @export
#' @importFrom stats rnorm
estimate.theta.mle <- function(u, a, b, c, init=NULL, iteration=15, delta=0.005, bound=3.5){
  u <- as.matrix(u)
  n.peo <- nrow(u)
  n.item <- ncol(u)
  if(length(a) != n.item || length(b) != n.item || length(c) != n.item) stop("wrong length in item parameters. make sure response is a matrix.")
  if(!is.null(init)) t <- init else t <- rnorm(n.peo, 0, .1)
  if(length(t) != n.peo) stop("wrong length in initial values.")
  a.matrix <- matrix(rep(a, n.peo), ncol=n.item, byrow=T)
  
  for(i in 1:iteration){
    p <- prob(irt(t, a, b, c))
    p.star <- prob(irt(t, a, b, c=0))
    L.1 <- 1.7 * a.matrix * (u - p) * p.star / p
    L.2 <- (-1) * (1.7 * a.matrix * p.star / p)^2 * p * (1 - p)
    h <- rowSums(L.1) / rowSums(L.2)
    t <- ifelse(abs(t - h) > bound, t, t - h)
    if(mean(abs(h)) < delta) break
  }
  return(t)
}

#' @rdname estimate.theta.mle
#' @description \code{estimate.theta.map} estimates thetas with known item parameters using maximum a posterior.
#' @param prior.mu the mean of the prior theta distribuiton
#' @param prior.sig the standard deviation of the prior theta distribution
#' @details
#' For the maximum a posteriori estimation, refer to Baker and Kim (2004), pp. 192.
#' @examples
#' # MAP
#' x <- gen.rsp(gen.irt(100, 20))
#' y <- estimate.theta.map(x$rsp, x$items$a, x$items$b, x$items$c)
#' cor(x$thetas, y)
#' plot(x$thetas, y, xlim=c(-3, 3), ylim=c(-3, 3), col=rgb(.8,.2,.2,.5), pch=16)
#' abline(a=0, b=1)
#' @family estimation
#' @export
#' @importFrom stats rnorm
estimate.theta.map <- function(u, a, b, c, init=NULL, prior.mu=0, prior.sig=1, iteration=15, delta=0.005, bound=3.5){
  u <- as.matrix(u)
  n.peo <- nrow(u)
  n.item <- ncol(u)
  if(length(a) != n.item || length(b) != n.item || length(c) != n.item) stop("wrong length in item parameters. make sure response is a matrix.")
  if(!is.null(init)) t <- init else t <- rnorm(n.peo, 0, .1)
  if(length(t) != n.peo) stop("wrong length in initial values.")
  a.matrix <- matrix(rep(a, n.peo), ncol=n.item, byrow=T)
  c.matrix <- matrix(rep(c, n.peo), nrow=n.peo, byrow=T)
  
  for(i in 1:iteration){
    p <- prob(irt(t, a, b, c))
    L.1 <- rowSums((1.7 * a.matrix) * (p - c.matrix) / (p * (1 - c.matrix)) * (u - p)) - (t - prior.mu) / prior.sig
    L.2 <- rowSums((-1) * (1.7 * a.matrix * (p - c.matrix) / (1 - c.matrix))^2 * (1 - p) / p) - 1 / prior.sig^2
    h <- L.1 / L.2
    t <- ifelse(abs(t - h) > bound, t, t - h)
    if(mean(abs(h)) < delta) break
  }
  return(t)
}

#' @rdname estimate.theta.mle
#' @description \code{estimate.theta.eap} estimates thetas with known item parameters using expected a posterior.
#' @details
#' For the expected a posteriori, refer to Baker and Kim (2004), pp. 193.
#' @family estimation
#' @examples
#' # EAP
#' x <- gen.rsp(gen.irt(100, 20))
#' y <- estimate.theta.eap(x$rsp, x$items$a, x$items$b, x$items$c)
#' cor(x$thetas, y)
#' plot(x$thetas, y, xlim=c(-3, 3), ylim=c(-3, 3), col=rgb(.8,.2,.2,.5), pch=16)
#' abline(a=0, b=1)
#' @export
estimate.theta.eap <- function(u, a, b, c){
  u <- as.matrix(u)
  n.peo <- nrow(u)
  n.item <- ncol(u)
  if(length(a) != n.item || length(b) != n.item || length(c) != n.item) stop("wrong length in item parameters. make sure response is a matrix.")
  X <- hermite.gauss()$x
  A <- hermite.gauss()$wgt
  L <- sapply(X, function(x){likelihood(irt(rep(x, n.peo), a, b, c, u), summary=1, fun=prod)})
  L.up <- rowSums(L * matrix(rep(A * X, n.peo), nrow=n.peo, byrow=T))
  L.down <- rowSums(L * matrix(rep(A, n.peo), nrow=n.peo, byrow=T))
  t <- L.up / L.down
  return(t)
}

#' Item Calibration
#' @description \code{estimate.item.jmle} calibrates item parameters with knwon thetas using joint maximum likelihood.
#' @param u a response matrix
#' @param theta a vector of theta parameters
#' @param model an IRT model used for calibration, taking values of "3PL", "2PL", "1PL", and "Rasch"
#' @param iteration the maximum iterations in Newton-Raphson procedure
#' @param delta the convergence criterion to terminate the Newton-Raphson procedure
#' @param a.bound the maximum value of estimated a parameters
#' @param b.bound the maximum absolute value of estimated b parameters
#' @param c.bound the maximum value of estimated c parameters
#' @param diagnose TRUE to return diangosis information
#' @return estimated item parameters and diagnosis information if required
#' @details 
#' Diagnosis information contains the average changes and values of a-, b-, c-parameters over the Newton-Raphson procedure.\cr
#' For the joint maximum likelihood estimation, refer to Baker and Kim (2004), pp. 46-54.
#' @examples
#' \dontrun{
#' # JMLE
#' x <- gen.rsp(gen.irt(3000, 30, a.sig=.4))
#' y <- estimate.item.jmle(x$rsp, x$thetas, model="3PL", diagnose=TRUE)
#' plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' }
#' @family calibration
#' @export
#' @importFrom stats aggregate
estimate.item.jmle <- function(u, theta, model="3PL", iteration=100, delta=0.01, a.bound=2.0, b.bound=3.5, c.bound=0.25, diagnose=FALSE){
  # initial constants
  u <- as.matrix(u)
  n.item <- dim(u)[2]
  
  # group, and compute f and r
  theta <- round(theta, 2)
  t <- unique(theta)
  n.t <- length(t)
  indices <- match(theta, t)
  f <- freq(indices, 1:max(indices))
  f <- as.vector(f[,-1])
  r <- aggregate(u, by=list(indices), FUN=sum)
  r <- as.matrix(r[,-1])
  
  # initial item parameters and learning rates
  if(model == "Rasch") a <- rep(.588, n.item) else a <- rep(1, n.item)
  b <- rep(0, n.item)
  c <- rep(0, n.item)
  h.a <- h.b <- h.c <- rep(0, n.item)
  
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iteration, ncol=3, dimnames=list(paste("iter",1:iteration,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iteration, n.item), dimnames=list(c("a","b","c"), paste("iter",1:iteration,sep=""), paste("item",1:n.item,sep=""))))
  
  # mle calibration
  cat("calibrating")
  for(i in 1:iteration){
    cat(".")
    # p, p.star, a.matrix, b.matrix, and c.matrix
    p <- prob(irt(t, a, b, c))
    p.star <- prob(irt(t, a, b, c=0))
    a.matrix <- matrix(rep(a, n.t), nrow=n.t, byrow=T)
    b.matrix <- matrix(rep(b, n.t), nrow=n.t, byrow=T)
    c.matrix <- matrix(rep(c, n.t), nrow=n.t, byrow=T)
    
    # a parameter
    if(model %in% c("3PL", "2PL")){
      L.1 <- (r - p * f) * outer(t, b, "-") * p.star / p
      L.11 <- (-1) * f * outer(t, b, "-")^2 * p * (1 - p) * (p.star / p)^2
      h0 <- h.a
      h.a <- colSums(L.1) / colSums(L.11)
      h.a <- ifelse(abs(h.a) > abs(h0), h.a * 0.5, h.a * 1.1)
    }
    a.new <- a - h.a
    a <- ifelse(a.new < 0 | a.new > a.bound, a, a.new)
    
    # b parameter
    L.2 <- -1.7 * a.matrix * (r - p * f) * (p.star / p)
    L.22 <- -(1.7 * a.matrix)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L.2) / colSums(L.22)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * 0.5, h.b * 1.1)
    b.new <- b - h.b
    b <- ifelse(abs(b.new) > b.bound, b, b.new)
    
    # c parameter
    if(model %in% c("3PL")){
      L.3 <- (r - p * f) / (p - c.matrix) * (p.star / p)
      L.33 <- (-1) * f * (1 - p) / (1 - c.matrix) / (p - c.matrix) * (p.star / p)
      h0 <- h.c
      h.c <- colSums(L.3) / colSums(L.33)
      h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c * 1.1)
    }
    c.new <- c - h.c
    c <- ifelse(c.new < 0 | c.new > c.bound, c, c.new)
    
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      
    
    # convergence
    if(mean(abs(h.b)) <= delta) break;
  }
  cat("Done.\n")
  
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  
  # output
  output <- list(parameters=data.frame(a,b,c))
  if(diagnose) output$diagnosis <- diagnosis
  return(output)
}

#' @rdname estimate.item.jmle
#' @description \code{estimate.item.mmle} calibrates item parameters using marginal maximum likelihood.
#' @details
#' For the marginal maximum likelihood estimation, refer to Baker and Kim (2004), pp.166-174.
#' @examples
#' \dontrun{
#' # MMLE
#' x <- gen.rsp(gen.irt(3000, 30, a.sig=.4))
#' y <- estimate.item.mmle(x$rsp, model="3PL", diagnose=TRUE)
#' plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=rgb(.8,.2,.2,.5),)
#' abline(a=0, b=1, lty=2)
#' plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' y$diagnosis$h
#' z <- estimate.theta.mle(x$rsp, y$parameters$a, y$parameters$b, y$parameters$c)
#' plot(x$thetas, z, xlim=c(-5, 5), ylim=c(-5, 5), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' }
#' @family calibration
#' @export
estimate.item.mmle <- function(u, model="3PL", iteration=100, delta=0.01, a.bound=2.0, b.bound=3.5, c.bound=0.25, diagnose=FALSE){
  # initial constants
  X <- hermite.gauss()$x
  A <- hermite.gauss()$wgt
  u <- as.matrix(u)
  n.peo <- dim(u)[1]
  n.item <- dim(u)[2]
  
  # initial item parameters and learning rates
  if(model == "Rasch") a <- rep(.588, n.item) else a <- rep(1, n.item)
  b <- rep(0, n.item)
  c <- rep(0, n.item)
  h.a <- h.b <- h.c <- rep(0, n.item)
  
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iteration, ncol=3, dimnames=list(paste("iter",1:iteration,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iteration, n.item), dimnames=list(c("a","b","c"), paste("iter",1:iteration,sep=""), paste("item",1:n.item,sep=""))))

  # mmle calibration
  cat("calibrating")
  for(i in 1:iteration){
    cat(".")
    # f and r
    L <- sapply(X, function(x){likelihood(irt(rep(x, n.peo), a, b, c, u), summary=1, fun=prod)})
    P <- L * matrix(rep(A, n.peo), nrow=n.peo, byrow=T)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% u
    
    # p, p.star, a.matrix, b.matrix, and c.matrix
    p <- prob(irt(X, a, b, c))
    p.star <- prob(irt(X, a, b, c=0))
    a.matrix <- matrix(rep(a, length(X)), nrow=length(X), byrow=T)
    b.matrix <- matrix(rep(b, length(X)), nrow=length(X), byrow=T)
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=T)
    
    # a parameter
    if(model %in% c("3PL", "2PL")){
      L.1 <- (r - p * f) * outer(X, b, "-") * p.star / p
      L.11 <- (-1) * f * outer(X, b, "-")^2 * p * (1 - p) * (p.star / p)^2
      h0 <- h.a
      h.a <- colSums(L.1) / colSums(L.11)
      h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a * 1.1)
    }
    a.new <- a - h.a
    a <- ifelse(a.new < 0 | a.new > a.bound, a, a.new)
    
    # b parameter
    L.2 <- -1.7 * a.matrix * (r - p * f) * (p.star / p)
    L.22 <- -(1.7 * a.matrix)^2 * f * p * (1 - p) * (p.star / p)
    h0 <- h.b
    h.b <- colSums(L.2) / colSums(L.22)
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * .5, h.b * 1.1)
    b.new <- b - h.b
    b <- ifelse(abs(b.new) > b.bound, b, b.new)
    
    # c parameter
    if(model %in% c("3PL")){
      L.3 <- (r - p * f) / (p - c.matrix) * (p.star / p)
      L.33 <- (-1) * f * (1 - p) / (1 - c.matrix) / (p - c.matrix) * (p.star / p)
      h0 <- h.c
      h.c <- colSums(L.3) / colSums(L.33)
      h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c * 1.1)
    }
    c.new <- c - h.c
    c <- ifelse(c.new < 0 | c.new > c.bound, c, c.new)
    
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      

    # convergence
    if(mean(abs(h.b)) <= delta) break;
  }
  cat("Done.\n")
  
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  
  # output
  output <- list(parameters=data.frame(a,b,c))
  if(diagnose) output$diagnosis <- diagnosis
  return(output)
}

#' @rdname estimate.item.jmle
#' @description \code{estimate.item.bme} calibrates item parameters using bayesian maximum likelihood.
#' @param a.mu the log mean of the lognormal prior distribution for a pameters
#' @param a.sig the log sd of the lognormal prior distribution of a parmaeters
#' @param b.mu the mean of the normal prior distribution for b pameters
#' @param b.sig the sd of the normal prior distribution of b parmaeters
#' @param c.alpha alpah of the prior beta distribution for c pameters
#' @param c.beta beta of the prior beta distribution of c parmaeters
#' @details
#' For the Bayesian maximum likelihood estimation, refer to Baker and Kim (2004), pp.183-191.
#' @examples
#' \dontrun{
#' # BME
#' x <- gen.rsp(gen.irt(3000, 30, a.sig=.4))
#' y <- estimate.item.bme(x$rsp, model="3PL", diagnose=TRUE, a.mu=0, a.sig=.4, c.alpha=5, c.beta=30)
#' plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' y$diagnosis$h
#' z <- estimate.theta.mle(x$rsp, y$parameters$a, y$parameters$b, y$parameters$c)
#' plot(x$thetas, z, xlim=c(-5, 5), ylim=c(-5, 5), pch=16, col=rgb(.8,.2,.2,.5))
#' abline(a=0, b=1, lty=2)
#' }
#' @family calibration
#' @export
#' @importFrom stats runif rnorm
estimate.item.bme <- function(u, model="3PL", a.mu=0, a.sig=0.2, b.mu=0, b.sig=1, c.alpha=5, c.beta=23, iteration=100, delta=0.01, a.bound=2.0, b.bound=3.5, c.bound=0.25, diagnose=FALSE){
  # initial constants
  X <- hermite.gauss()$x
  A <- hermite.gauss()$wgt
  u <- as.matrix(u)
  n.peo <- dim(u)[1]
  n.item <- dim(u)[2]
  
  # initial item parameters and learning rates
  if(model == "Rasch") a <- rep(.588, n.item) else a <- exp(rnorm(n.item, 0, .05))
  b <- rnorm(n.item, 0, .10)
  c <- runif(n.item, 0, .05)
  h.a <- h.b <- h.c <- rep(0, n.item)
  
  # diagnosis information
  diagnosis <- list(h=matrix(nrow=iteration, ncol=3, dimnames=list(paste("iter",1:iteration,sep=""), c("a","b","c"))),
                    par=array(NA, dim=c(3, iteration, n.item), dimnames=list(c("a","b","c"), paste("iter",1:iteration,sep=""), paste("item",1:n.item,sep=""))))
  
  # mmle calibration
  cat("calibrating")
  for(i in 1:iteration){
    cat(".")
    # f and r
    L <- sapply(X, function(x){likelihood(irt(rep(x, n.peo), a, b, c, u), summary=1, fun=prod)})
    P <- L * matrix(rep(A, n.peo), nrow=n.peo, byrow=T)
    P <- P / rowSums(P)
    f <- colSums(P)
    r <- t(P) %*% u
    
    # p, p.star, a.matrix, b.matrix, and c.matrix
    p <- prob(irt(X, a, b, c))
    p.star <- prob(irt(X, a, b, c=0))
    w <- p.star * (1 - p.star) / p / (1 -p)
    a.matrix <- matrix(rep(a, length(X)), nrow=length(X), byrow=T)
    b.matrix <- matrix(rep(b, length(X)), nrow=length(X), byrow=T)
    c.matrix <- matrix(rep(c, length(X)), nrow=length(X), byrow=T)
    
    # a parameter
    if(model %in% c("3PL", "2PL")){
      L.1 <-  1.7 * a * (1 - c) * colSums((r - f * p) * outer(X, b, "-") * w) - (log(a) - a.mu) / a.sig^2
      L.11 <- (-1) * (1.7 * a)^2 * colSums(f * outer(X, b, "-")^2 * (p - c.matrix)^2 / (1 - c.matrix)^2 * (1 - p) / p) - 1 / a.sig^2
      h0 <- h.a
      h.a <- L.1 / L.11
      h.a <- ifelse(abs(h.a) > abs(h0), h.a * .5, h.a * 1.1)
    }
    a.new <- a - h.a
    a <- ifelse(a.new < 0 | a.new > a.bound, a, a.new)
    
    # b parameter
    L.2 <- (-1) * 1.7 * a * (1 - c)  * colSums((r - f * p) * w) - (b - b.mu) / b.sig^2
    L.22 <- (-1) * (1.7 * a)^2 * colSums(f * (p - c.matrix)^2 / (1 - c.matrix)^2 * (1 - p) / p) - 1 / b.sig^2
    h0 <- h.b
    h.b <- L.2 / L.22
    h.b <- ifelse(abs(h.b) > abs(h0), h.b * .5, h.b * 1.1)
    b.new <- b - h.b
    b <- ifelse(abs(b.new) > b.bound, b, b.new)
    
    # c parameter
    if(model %in% c("3PL")){
      L.3 <- 1 / (1 - c) * colSums((r - f * p) / p) + ((c.alpha - 2) / c - (c.beta - 2) / (1 - c))
      L.33 <- (-1) * colSums(f / (1 - c.matrix)^2 * (1 - p) / p) - (c.alpha - 2) / c^2 - (c.beta - 2) / (1 - c)^2
      h0 <- h.c
      h.c <- L.3 / L.33
      h.c <- ifelse(abs(h.c) > abs(h0), h.c * 0.5, h.c * 1.1)
    }
    c.new <- c - h.c
    c <- ifelse(c.new < 0 | c.new > c.bound, c, c.new)
    
    # diagnosis
    diagnosis$h[i, ] <- c(mean(abs(h.a)), mean(abs(h.b)), mean(abs(h.c)))
    diagnosis$par[1, i, ] <- a      
    diagnosis$par[2, i, ] <- b
    diagnosis$par[3, i, ] <- c      
    
    # convergence
    if(mean(abs(h.b)) <= delta) break;
  }
  cat("Done.\n")
  
  # clean diagnosis information
  diagnosis$h <- diagnosis$h[1:i,]
  diagnosis$par <- diagnosis$par[,1:i,]
  
  # output
  output <- list(parameters=data.frame(a,b,c))
  if(diagnose) output$diagnosis <- diagnosis
  return(output)
}




