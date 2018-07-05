#' 3-parameter-logistic model
#' @name model_3pl 
NULL

#' @rdname model_3pl
#' @description \code{model_3pl_prob} computes the probability of a correct response for given parameters
#' @param t ability parameter, 1d vector
#' @param a discrimination parameters, 1d vector
#' @param b difficulty parameters, 1d vector
#' @param c guessing parameters, 1d vector
#' @param D the scaling constant, 1.702 by default
#' @return \code{model_3pl_prob} returns a 2d matrix of probabilities, dim = n_people x n_items
#' @examples
#' model_3pl_prob(c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.702)
#' model_3pl_prob(c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.0)
#' @export
model_3pl_prob <- function(t, a, b, c, D=1.702){
  p <- c + (1 - c) / (1 + exp(D * a * outer(b, t, '-')))
  t(p)
}

#' @rdname model_3pl
#' @description \code{model_3pl_info} computes the information for given parameters
#' @return \code{model_3pl_info} returns a 2d matrix of informations, dim = n_people x n_items
#' @examples
#' model_3pl_info(c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.702)
#' model_3pl_info(c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.0)
#' @export
model_3pl_info <- function(t, a, b, c, D=1.702){
  p <- t(model_3pl_prob(t, a, b, c, D))
  i <- (D * a * (p - c) / (1 - c))^2 * (1 - p) / p
  t(i)
}

#' @rdname model_3pl
#' @description \code{model_3pl_lik} computes the likelihood of observed responses
#' @param u observed responses, 2d matrix
#' @param log True to return log-likelihood
#' @return \code{model_3pl_lik} returns a 2d matrix of likelihood, dim = n_people x n_items
#' @examples
#' u <- matrix(c(1, 0, 1, 0, 1, 0), nrow=2)
#' model_3pl_lik(u, c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.702, FALSE)
#' model_3pl_lik(u, c(-1, 1), c(1, .8, .8), c(-1, 1, 1), c(0, 0, .2), 1.0, TRUE)
#' @export
model_3pl_lik <- function(u, t, a, b, c, D=1.702, log=FALSE){
  p <- model_3pl_prob(t, a, b, c, D)
  l <- p^u * (1-p)^(1-u)
  if(log) l <- log(l)
  l
}

#' @rdname model_3pl
#' @description \code{model_3pl_rescale} transforms parameterrs to a given scale
#' @param param the parameter of the new scale, t or b
#' @param mean the mean of the new scale
#' @param sd the SD of the new scale
#' @return \code{model_3pl_rescale} returns a list of rescaled t, a, b, c parameters
#' @importFrom stats sd
#' @export
model_3pl_rescale <- function(t, a, b, c, param=c("t", "b"), mean=0, sd=1){
  scale <- switch(match.arg(param), "t"=t, "b"=b)
  slope <- sd / sd(scale)
  intercept <- mean - slope * mean(scale)
  t <- slope * t + intercept
  b <- slope * b + intercept
  a <- a / slope
  list(t=t, a=a, b=b, c=c)
}


#' @rdname model_3pl
#' @description \code{model_3pl_gendata} generates data using the 3pl model
#' @param num_people the number of people to be generated
#' @param num_item the number of items to be generated
#' @param t_dist the normal distribution parameters of t-parameters, a vector: c(mean, sd)
#' @param a_dist the lognormal distribution parameters of a-parameters, a vector: c(meanlog, sdlog)
#' @param b_dist the normal distribution parameters of b-parameters, a vector: c(mean, sd)
#' @param c_dist the beta distribution pamameters of c-parameters, a vector: c(alpha, beta)
#' @param missing the proportion or number of missing responses
#' @return \code{model_3pl_gendata} returns a list of generated data
#' @examples
#' model_3pl_gendata(10, 5)
#' model_3pl_gendata(10, 5, a=1, c=0, missing=.1)
#' @importFrom stats rnorm rlnorm rbeta runif
#' @export
model_3pl_gendata <- function(num_people, num_item, t=NULL, a=NULL, b=NULL, c=NULL, D=1.702, t_dist=c(0, 1), a_dist=c(0, .2), b_dist=c(0, 1), c_dist=c(5, 46), missing=NULL){
  if(is.null(t)) t <- rnorm(num_people, mean=t_dist[1], sd=t_dist[2])
  if(is.null(a)) a <- rlnorm(num_item, meanlog=a_dist[1], sdlog=a_dist[2])
  if(is.null(b)) b <- rnorm(num_item, mean=b_dist[1], sd=b_dist[2])
  if(is.null(c)) c <- rbeta(num_item, shape1=c_dist[1], shape2=c_dist[2])
  if(length(t) == 1) t <- rep(t, num_people)
  if(length(a) == 1) a <- rep(a, num_item)
  if(length(b) == 1) b <- rep(b, num_item)
  if(length(c) == 1) c <- rep(c, num_item)
  if(length(a) != length(b) || length(c) != length(b) || length(b) != num_item)
    stop('dimensions of item parameters do not match')
  if(length(t) != num_people)
    stop('dimensions of people parameters do not match')
  p <- model_3pl_prob(t, a, b, c, D)
  x <- matrix(runif(num_people * num_item), nrow=num_people, ncol=num_item)
  u <- (p >= x) * 1
  if(!is.null(missing)){
    if(missing < 1) missing <- floor(missing * num_people * num_item)
    idx <- sample(num_people * num_item, missing)
    for(i in idx) u[ceiling(i/num_item), (i - 1) %% num_item + 1] <- NA
  }
  list(u=u, t=t, a=a, b=b, c=c)
}

#' @rdname model_3pl
#' @description \code{model_3pl_plot} plots the item characteristics curve (ICC) or 
#' item information function curve (IIFC)
#' @param type the type of plot, prob for ICC and info for IIFC
#' @param total TRUE to sum values over items
#' @param xaxis the values of x-axis
#' @return \code{model_3pl_plot} returns a ggplot graph
#' @examples
#' with(model_3pl_gendata(10, 5), model_3pl_plot(a, b, c, type="prob"))
#' with(model_3pl_gendata(10, 5), model_3pl_plot(a, b, c, type="info", total=TRUE))
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_plot <- function(a, b, c, D=1.702, type=c('prob', 'info'), total=FALSE, xaxis=seq(-4, 4, .1)){
  y <- switch(match.arg(type), "prob"=model_3pl_prob, "info"=model_3pl_info)(t=xaxis, a=a, b=b, c=c, D=D)
  if(total) y <- rowSums(y)
  z <- data.frame(theta=xaxis, y)
  z <- melt(z, id.vars="theta")
  ggplot(z, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab(toupper(type)) +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}

#' @rdname model_3pl
#' @description \code{model_3pl_plot_loglik} plots the log-likelihood curves for each response vector
#' @param show_mle TRUE to print maximum likelihood values
#' @return \code{model_3pl_plot} returns a ggplot graph
#' @examples
#' with(model_3pl_gendata(5, 50), model_3pl_plot_loglik(u, a, b, c, show_mle=TRUE))
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_plot_loglik <- function(u, a, b, c, D=1.702, xaxis=seq(-4, 4, .1), show_mle=FALSE){
  p <- model_3pl_prob(xaxis, a, b, c, D)
  lik <- log(p) %*% t(u) + log(1 - p) %*% t(1 - u)
  
  if(show_mle) print(apply(lik, 2, function(x){xaxis[which.max(x)]}))

  z <- data.frame(theta=xaxis, lik)
  z <- melt(z, id.vars="theta")
  ggplot(z, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab("Log-likelihood") +
    guides(color=FALSE) + theme_bw()
}


#' @rdname model_3pl
#' @param prior parameters of the prior distribution
model_3pl_dv_t <- function(u, t, a, b, c, D, prior=NULL){
  p <- t(model_3pl_prob(t, a, b, c, D))
  u <- t(u)
  dv1 <- D * a * (u - p) * (p - c) / p / (1 - c)
  dv1 <- colSums(dv1, na.rm=TRUE)
  dv2 <- (D * a / p / (1 - c))^2 * (u*c - p^2) * (p - c) * (1 - p)
  dv2 <- colSums(dv2, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 - (t - prior[1]) / prior[2]^2
    dv2 <- dv2 - 1 / prior[2]^2
  }
  list(dv1=dv1, dv2=dv2)
}

#' @rdname model_3pl
#' @param post_prob posterior distribution of the theta
#' @importFrom stats dnorm
model_3pl_dv_a <- function(u, t, a, b, c, D, prior=NULL, post_prob=NULL){
  p <- t(model_3pl_prob(t, a, b, c, D))
  u <- t(u)
  if(is.null(post_prob)) post_prob <- 1
  dv1 <- D * -1 * outer(b, t, "-") * (u - p) * (p - c) / p / (1 - c)
  dv1 <- colSums(t(dv1) * post_prob, na.rm=TRUE)
  dv2 <- (D * -1 * outer(b, t, "-") / p / (1 - c))^2 * (u*c - p^2) * (p - c) * (1 - p)
  dv2 <- colSums(t(dv2) * post_prob, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 - (a - prior[1]) / prior[2]^2 * dnorm(a, prior[1], prior[2])
    dv2 <- dv2 - ((a - prior[1]) / prior[2] + 1 / prior[2])^2 * dnorm(a, prior[1], prior[2])
  }
  list(dv1=dv1, dv2=dv2)
}

#' @rdname model_3pl
model_3pl_dv_b <- function(u, t, a, b, c, D, prior=NULL, post_prob=NULL){
  p <- t(model_3pl_prob(t, a, b, c, D))
  u <- t(u)
  if(is.null(post_prob)) post_prob <- 1
  dv1 <- -D * a * (u - p) * (p - c) / p / (1 - c)
  dv1 <- colSums(t(dv1) * post_prob, na.rm=TRUE)
  dv2 <- (D * a / p / (1 - c))^2 * (u*c - p^2) * (p - c) * (1 - p)
  dv2 <- colSums(t(dv2) * post_prob, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 - (b - prior[1]) / prior[2]^2
    dv2 <- dv2 -  1 / prior[2]^2
  }
  list(dv1=dv1, dv2=dv2)  
}

#' @rdname model_3pl
model_3pl_dv_c <- function(u, t, a, b, c, D, prior=NULL, post_prob=NULL){
  p <- t(model_3pl_prob(t, a, b, c, D))
  u <- t(u)
  if(is.null(post_prob)) post_prob <- 1
  dv1 <- (u - p) / p / (1 - c)
  dv1 <- colSums(t(dv1) * post_prob, na.rm=TRUE)
  dv2 <- (2*u*p - u - p^2) / p^2 / (1 - c)^2
  dv2 <- colSums(t(dv2) * post_prob, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 + (prior[1] - 1) / c - (prior[2] - 1) / (1 - c)
    dv2 <- dv2 - (prior[1] - 1) / c^2 - (prior[2] - 1) / (1 - c)^2
  }
  list(dv1=dv1, dv2=dv2)  
}

#' @rdname model_3pl
#' @importFrom stats rnorm
model_3pl_estimate_inits <- function(u, t, a, b, c){
  if(!all(u %in% c(0, 1, NA))) stop('invalid responses. only [0, 1] are allowed.')
  num_people <- dim(u)[1]
  num_item <- dim(u)[2]
  if(length(t) == 1) t <- rep(t, num_people)
  if(length(a) == 1) a <- rep(a, num_item)
  if(length(b) == 1) b <- rep(b, num_item)
  if(length(c) == 1) c <- rep(c, num_item)
  t_free <- is.na(t)
  a_free <- is.na(a)
  b_free <- is.na(b)
  c_free <- is.na(c)
  t[t_free] <- rnorm(sum(t_free), 0, .1)
  b[b_free] <- rnorm(sum(b_free), 0, .1)
  a[a_free] <- 1.00
  c[c_free] <- 0.01
  list(u=u, t=t, a=a, b=b, c=c, t_free=t_free, a_free=a_free, b_free=b_free, c_free=c_free)
}

#' @rdname model_3pl
#' @param h change of parameters in the newton-raphson method
#' @param is_free TRUE to estimate parameters and FALSE to fix parameters
model_3pl_estimate_nr <- function(param, h, is_free, h_max, bounds){
  h[h > h_max] <- h_max
  h[h < -h_max] <- -h_max
  h[!is_free] <- 0
  param <- param - h
  param[param < bounds[1]] <- bounds[1]
  param[param > bounds[2]] <- bounds[2]
  h <- sum(abs(h)) / sum(is_free)
  list(param=param, h=h)
}

#' @rdname model_3pl
#' @description \code{model_3pl_jmle} estimates parameters using the joint MLE method
#' @param num_iter the maximum number of overall iterations
#' @param num_nr the maximum number of the newton-raphson iterations
#' @param h_max the maximum value of h in the newton-raphson method
#' @param conv the convergence criterion in -2 log-likelihood of model fit
#' @param decay the epoch decay parameter
#' @param scale the scale of theta parameters
#' @param bounds the bounds of parameters, a list
#' @param priors the priors of parameters used in the maximum a posteriori estimation
#' @param debug TRUE to print debugging information
#' @return \code{model_3pl_jmle} returns estimated parameters
#' @examples
#' \dontrun{
#' data_tru <- model_3pl_gendata(3000, 50)
#' data_est <- model_3pl_jmle(u=data_tru$u, scale=c(0, 1), priors=NULL, debug=TRUE)
#' evaluate_3pl_estimation(data_tru, data_est)
#' }
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_jmle <- function(u, t=NA, a=NA, b=NA, c=NA, D=1.702, num_iter=100, num_nr=15, h_max=1.0, conv=.1, decay=.95, scale=NULL, bounds=list(t=c(-4, 4), a=c(0.1, 2), b=c(-4, 4), c=c(0.01, 0.3)), priors=list(t=c(0, 1), a=c(0, .2), b=c(0, 1), c=c(5, 46)), debug=FALSE){
  params <- model_3pl_estimate_inits(u, t, a, b, c)
  u <- params$u
  t <- params$t
  a <- params$a
  b <- params$b
  c <- params$c
  t_free <- params$t_free
  a_free <- params$a_free
  b_free <- params$b_free
  c_free <- params$c_free
  
  h_t <- h_a <- h_b <- h_c <- h_max
  model_fit <- -2 * log(0.001) * length(u)
  debug_info <- list(t=NULL, a=NULL, b=NULL, c=NULL, fit=NULL)

  # estimate parameters
  for (i in 1:num_iter){
    # debugging information
    debug_info$t <- c(debug_info$t, h_t)
    debug_info$a <- c(debug_info$a, h_a)
    debug_info$b <- c(debug_info$b, h_b)
    debug_info$c <- c(debug_info$c, h_c)
    debug_info$fit <- c(debug_info$fit, model_fit)
    
    model_fit <- -2.0 * sum(model_3pl_lik(u, t, a, b, c, D, log=TRUE), na.rm=TRUE)
    if(debug) cat('step #', i, ': model fit = ', round(model_fit, 3), '\n', sep='')
    if(debug_info$fit[i] - model_fit < conv) break

    # update t-parameters
    if(any(t_free)){
      for(j in 1:num_nr){
        dv <- model_3pl_dv_t(u, t, a, b, c, D, priors$t)
        x <- model_3pl_estimate_nr(t, (dv$dv1/dv$dv2)*decay^i, t_free, h_max, bounds$t)
        t <- x$param
        if(x$h < 0.005 || abs(x$h - h_t) < 0.001) break
        h_t <- x$h
      }
      if(!is.null(scale)) t <- (t - mean(t)) / sd(t) * scale[2] + scale[1] 
    }
    
    # update b-parameters
    if(any(b_free)){
      for(j in 1:num_nr){
        dv <- model_3pl_dv_b(u, t, a, b, c, D, priors$b)
        x <- model_3pl_estimate_nr(b, (dv$dv1/dv$dv2)*decay^i, b_free, h_max, bounds$b)
        b <- x$param
        if(x$h < 0.005 || abs(x$h - h_b) < 0.001) break
        h_b <- x$h
      }
    }
    
    # update a-parameters
    if(any(a_free)){
      for(j in 1:num_nr){
        dv <- model_3pl_dv_a(u, t, a, b, c, D, priors$a)
        x <- model_3pl_estimate_nr(a, dv$dv1/dv$dv2, a_free, h_max, bounds$a)
        a <- x$param
        if(x$h < 0.001 || abs(x$h - h_a) < 0.0005) break
        h_a <- x$h
      }
    }
    
    # update c-parameters
    if(any(c_free)){
      for(j in 1:num_nr){
        dv <- model_3pl_dv_c(u, t, a, b, c, D, priors$c)
        x <- model_3pl_estimate_nr(c, dv$dv1/dv$dv2, c_free, h_max, bounds$c)
        c <- x$param
        if(x$h < 0.001 || abs(x$h - h_c) < 0.0005) break
        h_c <- x$h
      }
    }
  }
  
  # debugging
  if(debug){
    x <- with(debug_info, data.frame(iteration=1:i, fit=fit, t=t, a=a, b=b, c=c))
    x <- x[-1,]
    x <- melt(x, id.vars="iteration")
    g <- ggplot(x, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + theme_bw()
    print(g)
  }
  
  list(t=t, a=a, b=b, c=c)
}


#' @rdname model_3pl
#' @param quad_t values of quadrature points
#' @param quad_w weights of quadrature points
model_3pl_posterior_dist <- function(u, a, b, c, D, quad_t, quad_w){
  num_people <- dim(u)[1]
  num_quad <- length(quad_t)
  p <- model_3pl_prob(quad_t, a, b, c, D)
  lik <- log(p) %*% t(u) + log(1 - p) %*% t(1 - u)
  lik <- t(exp(lik) * quad_w)
  lik / rowSums(lik, na.rm=TRUE)
}


#' @rdname model_3pl
#' @description \code{model_3pl_mmle} estimates parameters using the marginal MLE method
#' @param num_quad the number of quadrature points
#' @examples
#' \dontrun{
#' data_tru <- model_3pl_gendata(3000, 50)
#' data_est <- model_3pl_mmle(u=data_tru$u, scale=NULL, priors=NULL, debug=TRUE)
#' evaluate_3pl_estimation(data_tru, data_est)
#' }
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_mmle <- function(u, a=NA, b=NA, c=NA, D=1.702, num_iter=100, num_nr=15, num_quad=c('11', '20'), h_max=1.0, conv=.1, decay=.98, scale=NULL, bounds=list(t=c(-4, 4), a=c(0.1, 2), b=c(-4, 4), c=c(0.01, 0.3)), priors=list(t=c(0, 1), a=c(0, .2), b=c(0, 1), c=c(5, 46)), debug=FALSE){
  num_people <- dim(u)[1]
  num_item <- dim(u)[2]

  params <- model_3pl_estimate_inits(u, NA, a, b, c)
  u <- params$u
  t <- params$t
  a <- params$a
  b <- params$b
  c <- params$c
  t_free <- params$t_free
  a_free <- params$a_free
  b_free <- params$b_free
  c_free <- params$c_free
  
  h_t <- h_a <- h_b <- h_c <- h_max
  model_fit <- Inf
  debug_info <- list(a=NULL, b=NULL, c=NULL, fit=NULL)
  
  num_quad <- as.integer(match.arg(num_quad))
  quad <- hermite_gauss(num_quad)
  quad_t <- quad$t
  quad_w <- quad$w
  if(!is.null(scale)) quad_t <- quad_t * scale[2] + scale[1]

  # estimate parameters
  for (i in 1:num_iter){
    # debugging information
    debug_info$a <- c(debug_info$a, h_a)
    debug_info$b <- c(debug_info$b, h_b)
    debug_info$c <- c(debug_info$c, h_c)
    debug_info$fit <- c(debug_info$fit, model_fit)
    
    post_p <- model_3pl_posterior_dist(u, a, b, c, D, quad_t, quad_w)
    model_fit <- matrix(NA, nrow=num_people, ncol=num_quad)
    for(j in 1:num_quad)
      model_fit[,j] <- rowSums(model_3pl_lik(u, rep(quad_t[j], num_people), a, b, c, D, log=TRUE), na.rm=TRUE)
    model_fit <- -2.0 * sum(model_fit * post_p)
    if(debug) cat('step #', i, ': model fit = ', round(model_fit, 3), '\n', sep='')
    if(debug_info$fit[i] - model_fit < conv) break

    # update b-parameters
    if(any(b_free)){
      for(j in 1:num_nr){
        post_p <- model_3pl_posterior_dist(u, a, b, c, D, quad_t, quad_w)
        dv1 <- dv2 <- matrix(NA, nrow=num_item, ncol=num_quad)
        for(k in 1:num_quad){
          dv <- model_3pl_dv_b(u, rep(quad_t[k], num_people), a, b, c, D, priors$b, post_p[,k])
          dv1[,k] <- dv$dv1
          dv2[,k] <- dv$dv2
        }
        dv1 <- rowSums(dv1, na.rm=TRUE)
        dv2 <- rowSums(dv2, na.rm=TRUE)
        x <- model_3pl_estimate_nr(b, (dv1/dv2)*decay^i, b_free, h_max, bounds$b)
        b <- x$param
        if(x$h < 0.005 || abs(x$h - h_b) < 0.001) break
        h_b <- x$h
      }
    }
    
    # update a-parameters
    if(any(a_free)){
      for(j in 1:num_nr){
        post_p <- model_3pl_posterior_dist(u, a, b, c, D, quad_t, quad_w)
        dv1 <- dv2 <- rep(0, num_item)
        for(k in 1:num_quad){
          dv <- model_3pl_dv_a(u, rep(quad_t[k], num_people), a, b, c, D, priors$a, post_p[,k])
          dv1 <- dv1 + dv$dv1
          dv2 <- dv2 + dv$dv2
        }
        x <- model_3pl_estimate_nr(a, dv1/dv2, a_free, h_max, bounds$a)
        a <- x$param
        if(x$h < 0.001 || abs(x$h - h_a) < 0.0005) break
        h_a <- x$h
      }
    }
    
    # update c-parameters
    if(any(c_free)){
      for(j in 1:num_nr){
        post_p <- model_3pl_posterior_dist(u, a, b, c, D, quad_t, quad_w)
        dv1 <- dv2 <- rep(0, num_item)
        for(k in 1:num_quad){
          dv <- model_3pl_dv_c(u, rep(quad_t[k], num_people), a, b, c, D, priors$c, post_p[,k])
          dv1 <- dv1 + dv$dv1
          dv2 <- dv2 + dv$dv2
        }
        x <- model_3pl_estimate_nr(c, dv1/dv2, c_free, h_max, bounds$c)
        c <- x$param
        if(x$h < 0.001 || abs(x$h - h_c) < 0.0005) break
        h_c <- x$h
      }
    }
  }
  
  # update t-parameters
  for(j in 1:num_nr){
    dv <- model_3pl_dv_t(u, t, a, b, c, D, priors$t)
    x <- model_3pl_estimate_nr(t, (dv$dv1/dv$dv2)*decay^j, t_free, h_max, bounds$t)
    t <- x$param
    if(x$h < 0.005 || abs(x$h - h_t) < 0.001) break
    h_t <- x$h
  }
  
  # debugging
  if(debug){
    x <- with(debug_info, data.frame(iteration=1:i, fit=fit, a=a, b=b, c=c))
    x <- x[-1,]
    x <- melt(x, id.vars="iteration")
    g <- ggplot(x, aes_string(x="iteration", y="value", color="variable")) +
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + theme_bw()
    print(g)
  }
  
  list(t=t, a=a, b=b, c=c)
}


#' @rdname model_3pl
#' @description \code{model_3pl_eap_scoring} computes scores using the EAP method
#' @return \code{model_3pl_eap_scoring} returns the EAP scores
#' @export
model_3pl_eap_scoring <- function(u, a, b, c, D){
  quad <- hermite_gauss(20)
  post_dist <- model_3pl_posterior_dist(u, a, b, c, D, quad$t, quad$w)
  as.vector(post_dist %*% quad$t)
}


