#' Estimate 3-parameter-logistic model
#' @description Estimate the 3PL model using the maximum likelihood estimation 
#' @name estimate_3pl 
NULL

#' @rdname estimate_3pl
#' @description \code{model_3pl_eap_scoring} scores response vectors using the EAP method
#' @param prior the prior distribution
#' @examples 
#' with(model_3pl_gendata(10, 40), cbind(true=t, est=model_3pl_eap_scoring(u, a, b, c)$t))
#' @importFrom stats dnorm 
#' @export
model_3pl_eap_scoring <- function(u, a, b, c, D=1.702, prior=c(0, 1), bound=c(-3, 3)){
  if(is.null(prior)) prior <- c(0, 1)
  quad <- hermite_gauss('11')
  quad$w <- quad$w * exp(quad$t^2) * dnorm(quad$t, prior[1], prior[2])
  p <- model_3pl_prob(quad$t, a, b, c, D)
  lh <- exp(ifelse(is.na(u), 0, u) %*% t(log(p)) + ifelse(is.na(1 - u), 0, 1 - u) %*% t(log(1 - p)))
  t <- colSums(t(lh) * quad$w * quad$t) / colSums(t(lh) * quad$w)
  t[t < bound[1]] <- bound[1]
  t[t > bound[2]] <- bound[2]
  t_sd <- colSums(t(lh) * quad$w * outer(quad$t, t, '-')^2) / colSums(t(lh) * quad$w)
  t_sd <- sqrt(t_sd)
  list(t=t, sd=t_sd)
}

#' @rdname estimate_3pl
#' @description \code{model_3pl_map_scoring} scores response vectors using the MAP method
#' @examples 
#' with(model_3pl_gendata(10, 40), cbind(true=t, est=model_3pl_map_scoring(u, a, b, c)$t))
#' @export
model_3pl_map_scoring <- function(u, a, b, c, D=1.702, prior=c(0, 1), bound=c(-3, 3), nr_iter=30, nr_conv=1e-3){
  t <- rnorm(dim(u)[1], 0, .01)
  t_free <- rep(T, length(t))
  for(m in 1:nr_iter){
    dv_t <- model_3pl_dv_jmle(model_3pl_dv_Pt(t, a, b, c, D), u)
    dv_t$dv1 <- colSums(dv_t$dv1, na.rm=T)
    dv_t$dv2 <- colSums(dv_t$dv2, na.rm=T)
    if(!is.null(prior)){
      dv_t$dv1 <- dv_t$dv1 - (t - prior[1]) / prior[2]^2
      dv_t$dv2 <- dv_t$dv2 - 1 / prior[2]^2
    }
    nr_t <- estimate_nr_iteration(t, t_free, dv_t, 1.0, 1.0, bound)
    t <- nr_t$param
    if(max(abs(nr_t$h)) < nr_conv) break
  }
  list(t=t)
}

#' @rdname estimate_3pl
#' @keywords internal
model_3pl_dv_Pt <- function(t, a, b, c, D){
  p <- t(model_3pl_prob(t, a, b, c, D))
  dv1 <- D * a * (p - c) * (1 - p) / (1 - c)
  dv2 <- (D * a / (1 - c))^2 * (p - c) * (1 - p) * (1 + c - 2*p)
  list(dv1=dv1, dv2=dv2, p=p)
}

#' @rdname estimate_3pl
#' @keywords internal
model_3pl_dv_Pa <- function(t, a, b, c, D){
  p <- t(model_3pl_prob(t, a, b, c, D))
  dv1 <- D * t(outer(t, b, '-')) * (p - c) * (1 - p) / (1 - c)
  dv2 <- (D * t(outer(t, b, '-')) / (1 - c))^2 * (p - c) * (1 - p) * (1 + c - 2*p)
  list(dv1=dv1, dv2=dv2, p=p)
}

#' @rdname estimate_3pl
#' @keywords internal
model_3pl_dv_Pb <- function(t, a, b, c, D){
  p <- t(model_3pl_prob(t, a, b, c, D))
  dv1 <- - D * a * (p - c) * (1 - p) / (1 - c)
  dv2 <- (D * a / (1 - c))^2 * (p - c) * (1 - p) * (1 + c - 2*p)
  list(dv1=dv1, dv2=dv2, p=p)
}

#' @rdname estimate_3pl
#' @keywords internal
model_3pl_dv_Pc <- function(t, a, b, c, D){
  p <- t(model_3pl_prob(t, a, b, c, D))
  dv1 <- (1 - p) / (1 - c)
  dv2 <- array(0, dim=dim(dv1))
  list(dv1=dv1, dv2=dv2, p=p)
}

#' @rdname estimate_3pl
#' @description \code{model_3pl_dv_jmle} calculates the first and second derivatives for
#' the joint maximum likelihood estimation
#' @keywords internal
model_3pl_dv_jmle <- function(dv, u){
  dv1 <- (t(u) - dv$p) / dv$p / (1 - dv$p) * dv$dv1
  dv2 <- (t(u) - dv$p) / dv$p / (1 - dv$p) * dv$dv2 - ((t(u) - dv$p) / dv$p / (1 - dv$p) * dv$dv1)^2
  list(dv1=dv1, dv2=dv2)
}


#' @rdname estimate_3pl
#' @description \code{model_3pl_estimate_jmle} estimates the parameters using the 
#' joint maximum likelihood estimation (JMLE) method
#' @param u observed response matrix, 2d matrix
#' @param t ability parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param a discrimination parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param b difficulty parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param c pseudo-guessing parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param D the scaling constant, 1.702 by default
#' @param iter the maximum iterations
#' @param conv the convergence criterion of the -2 log-likelihood
#' @param nr_iter the maximum iterations of newton-raphson
#' @param nr_conv the convegence criterion for newton-raphson
#' @param scale the meand and SD of the theta scale, N(0, 1) for JMLE by default
#' @param bounds_t bounds of ability parameters
#' @param bounds_a bounds of discrimination parameters
#' @param bounds_b bounds of difficulty parameters
#' @param bounds_c bounds of guessing parameters
#' @param priors a list of prior distributions
#' @param decay decay rate
#' @param debug TRUE to print debuggin information
#' @param true_params a list of true parameters for evaluating the estimation accuracy 
#' @examples
#' \dontrun{
#' # generate data
#' x <- model_3pl_gendata(2000, 40)
#' # free estimation
#' y <- model_3pl_estimate_jmle(x$u, true_params=x)
#' # fix c-parameters
#' y <- model_3pl_estimate_jmle(x$u, c=0, true_params=x)
#' # no priors
#' y <- model_3pl_estimate_jmle(x$u, priors=NULL, iter=30, debug=T)
#' }
#' @importFrom stats cor
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_3pl_estimate_jmle <- function(u, t=NA, a=NA, b=NA, c=NA, D=1.702, iter=100, conv=1e-0, nr_iter=10, nr_conv=1e-3, scale=c(0, 1), bounds_t=c(-3, 3), bounds_a=c(.01, 2), bounds_b=c(-3, 3), bounds_c=c(0, .25), priors=list(t=c(0, 1), a=c(-.1, .2), b=c(0, 1), c=c(4, 20)), decay=1, debug=FALSE, true_params=NULL){
  # internal config
  h_max <- 1
  tracking <- list(fit=rep(NA, iter), t=rep(NA, iter), a=rep(NA, iter), b=rep(NA, iter), c=rep(NA, iter))
  
  # initial values
  n_p <- nrow(u)
  n_i <- ncol(u)
  if(length(t) == 1) t <- rep(t, n_p)
  t[t_free <- is.na(t)] <- rnorm(sum(is.na(t)), 0, .01)
  if(length(a) == 1) a <- rep(a, n_i)
  a[a_free <- is.na(a)] <- rlnorm(sum(is.na(a)), -.1, .01)
  if(length(b) == 1) b <- rep(b, n_i)
  b[b_free <- is.na(b)] <- rnorm(sum(is.na(b)), 0, .01)
  if(length(c) == 1) c <- rep(c, n_i)
  c[c_free <- is.na(c)] <- rbeta(sum(is.na(c)), 4, 20)

  for(k in 1:iter){
    # t parameters
    if(any(t_free)){
      for(m in 1:nr_iter){
        dv_t <- model_3pl_dv_jmle(model_3pl_dv_Pt(t, a, b, c, D), u)
        dv_t$dv1 <- colSums(dv_t$dv1, na.rm=T)
        dv_t$dv2 <- colSums(dv_t$dv2, na.rm=T)
        if(!is.null(priors$t)){
          dv_t$dv1 <- dv_t$dv1 - (t - priors$t[1]) / priors$t[2]^2
          dv_t$dv2 <- dv_t$dv2 - 1 / priors$t[2]^2
        }
        nr_t <- estimate_nr_iteration(t, t_free, dv_t, h_max, decay, bounds_t)
        t <- nr_t$param
        if(max(abs(nr_t$h)) < nr_conv) break
      }
      # rescale thetas
      if(!is.null(scale)) t <- (t - mean(t)) / sd(t) * scale[2] + scale[1]
    }
    
    # b parameters
    if(any(b_free)){
      for(m in 1:nr_iter){
        dv_b <- model_3pl_dv_jmle(model_3pl_dv_Pb(t, a, b, c, D), u)
        dv_b$dv1 <- rowSums(dv_b$dv1, na.rm=T)
        dv_b$dv2 <- rowSums(dv_b$dv2, na.rm=T)
        if(!is.null(priors$b)){
          dv_b$dv1 <- dv_b$dv1 - (b - priors$b[1]) / priors$b[2]^2
          dv_b$dv2 <- dv_b$dv2 - 1 / priors$b[2]^2
        }
        nr_b <- estimate_nr_iteration(b, b_free, dv_b, h_max, decay, bounds_b)
        b <- nr_b$param
        if(max(abs(nr_b$h)) < nr_conv) break
      }
    }

    # a parameters
    if(any(a_free)){
      for(m in 1:nr_iter){
        dv_a <- model_3pl_dv_jmle(model_3pl_dv_Pa(t, a, b, c, D), u)
        dv_a$dv1 <- rowSums(dv_a$dv1, na.rm=T)
        dv_a$dv2 <- rowSums(dv_a$dv2, na.rm=T)
        if(!is.null(priors$a)){
          dv_a$dv1 <- dv_a$dv1 - 1/a * (1 + (log(a)-priors$a[1])/priors$a[2]^2)
          dv_a$dv2 <- dv_a$dv2 - 1/a^2 * (1/priors$a[2]^2 - (1 + (log(a)-priors$a[1])/priors$a[2]^2))
        }
        nr_a <- estimate_nr_iteration(a, a_free, dv_a, h_max, decay, bounds_a)
        a <- nr_a$param
        if(max(abs(nr_a$h)) < nr_conv) break
      }
    }
    
    # estimate c parameters
    if(any(c_free)){
      for(m in 1:nr_iter){
        dv_c <- model_3pl_dv_jmle(model_3pl_dv_Pc(t, a, b, c, D), u)
        dv_c$dv1 <- rowSums(dv_c$dv1, na.rm=T)
        dv_c$dv2 <- rowSums(dv_c$dv2, na.rm=T)
        if(!is.null(priors$c)){
          dv_c$dv1 <- dv_c$dv1 - ((priors$c[2]-1)/(1-c) - (priors$c[1]-1)/c)
          dv_c$dv2 <- dv_c$dv2 - ((priors$c[1]-1)/c^2 + (priors$c[2]-1)/(1-c)^2)
        }
        nr_c <- estimate_nr_iteration(c, c_free, dv_c, h_max, decay, bounds_c)
        c <- nr_c$param
        if(max(abs(nr_c$h)) < nr_conv) break
      }
    }
    
    decay <- decay * decay
    
    # model fit
    loglh <- -2 * sum(model_3pl_lh(u, t, a, b, c, D, log=TRUE), na.rm=T)
    if(debug) cat('iter #', k, ': -2 log-likelihood = ', round(loglh, 2), '\n', sep='')
    if(k > 1  && tracking$fit[k-1] - loglh < conv) break
    tracking$fit[k] <- loglh
    if(any(t_free)) tracking$t[k] <- mean(abs(nr_t$h[t_free]))
    if(any(a_free)) tracking$a[k] <- mean(abs(nr_a$h[a_free]))
    if(any(b_free)) tracking$b[k] <- mean(abs(nr_b$h[b_free]))
    if(any(c_free)) tracking$c[k] <- mean(abs(nr_c$h[c_free]))
  }
  
  # debugging
  if(debug){
    xx <- with(tracking, data.frame(iteration=1:iter, fit=fit, t=t, a=a, b=b, c=c))[1:k, ]
    xx <- melt(xx, id.vars='iteration')
    xx <- xx[!is.na(xx$value), ]
    g <- ggplot(xx, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + 
      xlab('Iterations') + ylab('') + theme_bw()
    print(g)
  }
  
  # compare with true parameters
  if(!is.null(true_params)){
    xx <- rbind(data.frame(true=true_params$t, est=t, params='t'),
                data.frame(true=true_params$a, est=a, params='a'),
                data.frame(true=true_params$b, est=b, params='b'),
                data.frame(true=true_params$c, est=c, params='c'))
    g <- ggplot(xx, aes_string(x="true", y="est", color="params")) + 
      geom_point(alpha=.3) + geom_smooth(method='gam', se=F) + 
      facet_wrap(~params, nrow=1, scales='free') + guides(color=F) +
      xlab('True Parameters') + ylab('Est. Parameters') + theme_bw()
    print(g)
    if(any(t_free)) cat('t: corr = ', round(cor(t, true_params$t), 3), ', rmse = ', round(rmse(t, true_params$t), 3),'\n', sep='')
    if(any(a_free)) cat('a: corr = ', round(cor(a, true_params$a), 3), ', rmse = ', round(rmse(a, true_params$a), 3),'\n', sep='')
    if(any(b_free)) cat('b: corr = ', round(cor(b, true_params$b), 3), ', rmse = ', round(rmse(b, true_params$b), 3),'\n', sep='')
    if(any(c_free)) cat('c: corr = ', round(cor(c, true_params$c), 3), ', rmse = ', round(rmse(c, true_params$c), 3),'\n', sep='')
  }
  
  list(t=t, a=a, b=b, c=c)
}


#' @rdname estimate_3pl
#' @description \code{model_3pl_dv_mmle} calculates the first and second derivatives for
#' the marginal maximum likelihood estimation
#' @param pdv_fn the function to compute derivatives of P w.r.t the estimating parameters 
#' @keywords internal
model_3pl_dv_mmle <- function(pdv_fn, u, quad, a, b, c, D){
  n_p <- dim(u)[1]
  n_i <- dim(u)[2]
  n_q <- length(quad$t)

  p <- model_3pl_prob(quad$t, a, b, c, D)
  p_u1 <- t(ifelse(is.na(u), 0, u))
  p_u0 <- t(ifelse(is.na(u), 0, 1-u))
  ln_p <- log(p)
  ln_q <- log(1-p)
  p0 <- array(NA, c(n_i, n_p, n_q))
  p1 <- array(NA, c(n_p, n_q))
  for(q in 1:n_q){
    p0[,,q] <- p_u1*ln_p[q,] + p_u0*ln_q[q,]
    p1[,q] <- colSums(p0[,,q], na.rm=T)
  }
  p0 <- aperm(exp(p0), c(2,1,3))
  p1 <- exp(p1)
  p2 <- (p1 %*% quad$w)[,1]
  pdv <- pdv_fn(quad$t, a, b, c, D)
  
  dv_common <- t(quad$w * t(p1 / p2))
  dv_u0 <- t((-1)^(u+1))
  dv1 <- dv2 <- array(0, c(n_p, n_i))
  for(q in 1:n_q)
    dv1 <- dv1 + dv_common[,q] / p0[,,q] * t(dv_u0*pdv$dv1[,q])
  for(q in 1:n_q)
    dv2 <- dv2 + dv_common[,q] / p0[,,q] * t(dv_u0*pdv$dv2[,q])
  dv2 <- dv2 - dv1^2
  dv1 <- colSums(dv1, na.rm=T)
  dv2 <- colSums(dv2, na.rm=T)
  list(dv1=dv1, dv2=dv2)
}

#' @rdname estimate_3pl
#' @description \code{model_3pl_estimate_mmle} estimates the parameters using the 
#' marginal maximum likelihood estimation (MMLE) method
#' @param quad_degree the number of quadrature points
#' @param scoring the scoring method: 'eap' or 'map'
#' @examples
#' \dontrun{
#' # generate data
#' x <- model_3pl_gendata(2000, 40)
#' # free estimation
#' y <- model_3pl_estimate_mmle(x$u, true_params=x)
#' # fix c-parameters
#' y <- model_3pl_estimate_mmle(x$u, c=0, true_params=x)
#' # no priors
#' y <- model_3pl_estimate_mmle(x$u, priors=NULL, iter=30, debug=T)
#' }
#' @importFrom stats cor
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_3pl_estimate_mmle <- function(u, t=NA, a=NA, b=NA, c=NA, D=1.702, iter=100, conv=1e-0, nr_iter=10, nr_conv=1e-3, bounds_t=c(-3, 3), bounds_a=c(.01, 2), bounds_b=c(-3, 3), bounds_c=c(0, .25), priors=list(t=c(0, 1), a=c(-.1, .2), b=c(0, 1), c=c(4, 20)), decay=1, quad_degree='11', scoring=c('eap', 'map'), debug=FALSE, true_params=NULL){
  # internal config
  h_max <- 1
  if(is.null(priors$t)) priors$t <- c(0, 1)
  quad <- hermite_gauss(quad_degree)
  quad$w <- quad$w * exp(quad$t^2) * dnorm(quad$t, priors$t[1], priors$t[2])
  tracking <- list(fit=rep(NA, iter), t=rep(NA, iter), a=rep(NA, iter), b=rep(NA, iter), c=rep(NA, iter))
  
  # initial values
  n_p <- nrow(u)
  n_i <- ncol(u)
  if(length(t) == 1) t <- rep(t, n_p)
  t[t_free <- is.na(t)] <- rnorm(sum(is.na(t)), 0, .01)
  if(length(a) == 1) a <- rep(a, n_i)
  a[a_free <- is.na(a)] <- rlnorm(sum(is.na(a)), -.1, .01)
  if(length(b) == 1) b <- rep(b, n_i)
  b[b_free <- is.na(b)] <- rnorm(sum(is.na(b)), 0, .01)
  if(length(c) == 1) c <- rep(c, n_i)
  c[c_free <- is.na(c)] <- rbeta(sum(is.na(c)), 4, 20)
  
  for(k in 1:iter){
    # b parameters
    if(any(b_free)){
      for(m in 1:nr_iter){
        dv_b <- model_3pl_dv_mmle(model_3pl_dv_Pb, u, quad, a, b, c, D)
        if(!is.null(priors$b)){
          dv_b$dv1 <- dv_b$dv1 - (b - priors$b[1]) / priors$b[2]^2
          dv_b$dv2 <- dv_b$dv2 - 1 / priors$b[2]^2
        }
        nr_b <- estimate_nr_iteration(b, b_free, dv_b, h_max, decay, bounds_b)
        b <- nr_b$param
        if(max(abs(nr_b$h)) < nr_conv) break
      }
    }
    
    # a parameters
    if(any(a_free)){
      for(m in 1:nr_iter){
        dv_a <- model_3pl_dv_mmle(model_3pl_dv_Pa, u, quad, a, b, c, D)
        if(!is.null(priors$a)){
          dv_a$dv1 <- dv_a$dv1 - 1/a * (1 + (log(a)-priors$a[1])/priors$a[2]^2)
          dv_a$dv2 <- dv_a$dv2 - 1/a^2 * (1/priors$a[2]^2 - (1 + (log(a)-priors$a[1])/priors$a[2]^2))
        }
        nr_a <- estimate_nr_iteration(a, a_free, dv_a, h_max, decay * .2, bounds_a)
        a <- nr_a$param
        if(max(abs(nr_a$h)) < nr_conv) break
      }
    }
    
    # estimate c parameters
    if(any(c_free)){
      for(m in 1:nr_iter){
        dv_c <- model_3pl_dv_mmle(model_3pl_dv_Pc, u, quad, a, b, c, D)
        if(!is.null(priors$c)){
          dv_c$dv1 <- dv_c$dv1 - ((priors$c[2]-1)/(1-c) - (priors$c[1]-1)/c)
          dv_c$dv2 <- dv_c$dv2 - ((priors$c[1]-1)/c^2 + (priors$c[2]-1)/(1-c)^2)
        }
        nr_c <- estimate_nr_iteration(c, c_free, dv_c, h_max, decay, bounds_c)
        c <- nr_c$param
        if(max(abs(nr_c$h)) < nr_conv) break
      }
    }
    
    decay <- decay * decay
    
    # scoring
    if(any(t_free))
      t[t_free] <- switch(match.arg(scoring, scoring), 'eap'=model_3pl_eap_scoring, 'map'=model_3pl_map_scoring)(u, a, b, c, D, prior=priors$t, bound=bounds_t)$t[t_free]

    # model fit
    loglik <- -2 * sum(model_3pl_lh(u, t, a, b, c, D, log=TRUE), na.rm=T)
    if(debug) cat('iter #', k, ': -2 log-likelihood = ', round(loglik, 2), '\n', sep='')
    if(k > 1 && tracking$fit[k-1] - loglik < conv) break
    tracking$fit[k] <- loglik
    if(any(a_free)) tracking$a[k] <- mean(abs(nr_a$h[a_free]))
    if(any(b_free)) tracking$b[k] <- mean(abs(nr_b$h[b_free]))
    if(any(c_free)) tracking$c[k] <- mean(abs(nr_c$h[c_free]))
  }
  
  # debugging
  if(debug){
    xx <- with(tracking, data.frame(iteration=1:iter, fit=fit, a=a, b=b, c=c))[1:k, ]
    xx <- melt(xx, id.vars='iteration')
    xx <- xx[!is.na(xx$value), ]
    g <- ggplot(xx, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + 
      xlab('Iterations') + ylab('') + theme_bw()
    print(g)
  }
  
  # compare with true parameters
  if(!is.null(true_params)){
    xx <- rbind(data.frame(true=true_params$t, est=t, params='t'),
                data.frame(true=true_params$a, est=a, params='a'),
                data.frame(true=true_params$b, est=b, params='b'),
                data.frame(true=true_params$c, est=c, params='c'))
    g <- ggplot(xx, aes_string(x="true", y="est", color="params")) + 
      geom_point(alpha=.3) + geom_smooth(method='gam', se=F) + 
      facet_wrap(~params, nrow=1, scales='free') + guides(color=F) +
      xlab('True Parameters') + ylab('Est. Parameters') + theme_bw()
    print(g)
    if(any(t_free)) cat('t: corr = ', round(cor(t, true_params$t), 3), ', rmse = ', round(rmse(t, true_params$t), 3),'\n', sep='')
    if(any(a_free)) cat('a: corr = ', round(cor(a, true_params$a), 3), ', rmse = ', round(rmse(a, true_params$a), 3),'\n', sep='')
    if(any(b_free)) cat('b: corr = ', round(cor(b, true_params$b), 3), ', rmse = ', round(rmse(b, true_params$b), 3),'\n', sep='')
    if(any(c_free)) cat('c: corr = ', round(cor(c, true_params$c), 3), ', rmse = ', round(rmse(c, true_params$c), 3),'\n', sep='')
  }
  
  list(t=t, a=a, b=b, c=c)
}


#' @rdname estimate_3pl
#' @param index the indices of items being plotted
#' @param intervals intervals on the x-axis
#' @param show_points TRUE to show points
#' @examples 
#' with(model_3pl_gendata(1000, 20), model_3pl_fitplot(u, t, a, b, c, index=c(1, 3, 5)))
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_3pl_fitplot <- function(u, t, a, b, c, D=1.702, index=NULL, intervals=seq(-3, 3, .5), show_points=TRUE){
  if(is.null(index)) index <- seq(b)
  groups <- cut(t, intervals, labels=(intervals[-length(intervals)] + intervals[-1]) / 2)
  
  obs <- aggregate(u, by=list(intervals=groups), mean, na.rm=TRUE)[, c(1, index+1)]
  obs <- melt(obs, id.vars='intervals', variable.name='items')
  obs[, 'type'] <- 'Observed'
  p <- model_3pl_prob(t, a, b, c, D)
  exp <- aggregate(p, by=list(intervals=groups), mean, na.rm=TRUE)[, c(1, index+1)]
  exp <- melt(exp, id.vars='intervals', variable.name='items')
  exp[, 'type'] <- 'Expected'
  data <- rbind(obs, exp)
  data$intervals <- as.numeric(levels(data$intervals)[data$intervals])
  levels(data$items) <- gsub('V', 'Item ', levels(data$items))
  
  g <- ggplot(data, aes_string('intervals', 'value', color='type', group='type')) + 
    geom_line() + facet_wrap(~items) + xlab(expression(theta)) + ylab('Probability') + 
    scale_color_discrete(guide=guide_legend("")) + theme_bw()
  if(show_points) g <- g + geom_point(fill='white', pch=1)
  g
}
