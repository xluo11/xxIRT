#' Estimate Generalizaed Partial Credit Model
#' @description Estimate the GPCM using the maximum likelihood estimation 
#' @name estimate_gpcm
NULL

#' @rdname estimate_gpcm
#' @description \code{model_gpcm_eap_scoring} scores response vectors using the EAP method
#' @param prior the prior distribution
#' @examples 
#' with(model_gpcm_gendata(10, 40, 3), cbind(true=t, est=model_gpcm_eap_scoring(u, a, b, d)$t))
#' @importFrom stats dnorm 
#' @export
model_gpcm_eap_scoring <- function(u, a, b, d, D=1.702, prior=c(0, 1), bound=c(-3, 3)){
  quad <- hermite_gauss('11')
  quad$w <- quad$w * exp(quad$t^2) * dnorm(quad$t, prior[1], prior[2])
  n_p <- dim(u)[1]
  n_i <- dim(u)[2]
  n_q <- length(quad$t)
  
  p <- model_gpcm_prob(quad$t, a, b, d, D)
  ix <- model_polytomous_3dindex(u)
  lh <- array(NA, c(n_p, n_i, n_q))
  for(q in 1:n_q)
    lh[,,q] <- array(p[q,,][ix[,-1]], c(n_p, n_i))
  lh <- apply(lh, c(1, 3), prod, na.rm=T)
  t <- ((lh / (lh %*% quad$w)[,1]) %*% (quad$w * quad$t))[,1]
  t_sd <- ((lh / (lh %*% quad$w)[,1] * outer(t, quad$t, '-')^2) %*% quad$w)[,1]
  t_sd <- sqrt(t_sd)
  list(t=t, sd=t_sd)
}

#' @rdname estimate_gpcm
#' @description \code{model_gpcm_map_scoring} scores response vectors using maximum a posteriori
#' @examples 
#' with(model_gpcm_gendata(10, 40, 3), cbind(true=t, est=model_gpcm_map_scoring(u, a, b, d)$t))
#' @export
model_gpcm_map_scoring <- function(u, a, b, d, D=1.702, prior=NULL, bound=c(-3, 3), nr_iter=30, nr_conv=1e-3){
  ix <- model_polytomous_3dindex(u)
  t <- rnorm(dim(u)[1], 0, .01)
  t_free <- rep(T, length(t))
  for(i in 1:nr_iter){
    dv <- model_gpcm_dv_jmle(ix, model_gpcm_dv_Pt(t, a, b, d, D))
    dv$dv1 <- rowSums(dv$dv1, na.rm=T)
    dv$dv2 <- rowSums(dv$dv2, na.rm=T)
    if(!is.null(prior)){
      dv$dv1 <- dv$dv1 - (t - prior[1]) / prior[2]^2
      dv$dv2 <- dv$dv2 - 1 / prior[2]^2      
    }
    nr <- estimate_nr_iteration(t, t_free, dv, 1.0, 1.0, bound)
    t <- nr$param
    if(max(abs(nr$h)) < nr_conv) break
  }
  list(t=t)  
}

#' @rdname estimate_gpcm
#' @keywords internal
model_gpcm_dv_Pt <- function(t, a, b, d, D){
  p <- model_gpcm_prob(t, a, b, d, D)
  p <- ifelse(is.na(p), 0, p)
  n_p <- dim(p)[1]
  n_i <- dim(p)[2]
  n_c <- dim(p)[3]
  dv1 <- apply(p, 2, function(x) x %*% 1:n_c)
  dv1 <- -1 * outer(dv1, 1:n_c, '-') * D * p
  dv1 <- aperm(aperm(dv1, c(2,1,3)) * a, c(2,1,3))
  dv2 <- apply(dv1, 2, function(x) rep(x %*% 1:n_c, n_c))
  dv2 <- aperm(array(dv2, c(n_p, n_c, n_i)), c(1,3,2)) * D * p
  dv2 <- aperm(aperm(dv2, c(2,1,3)) * a, c(2,1,3))
  dv2 <- dv1^2 / p - dv2
  list(p=p, dv1=dv1, dv2=dv2)
}

#' @rdname estimate_gpcm
#' @keywords internal
model_gpcm_dv_Pa <- function(t, a, b, d, D){
  p <- model_gpcm_prob(t, a, b, d, D)
  n_p <- dim(p)[1]
  n_i <- dim(p)[2]
  n_c <- dim(p)[3]
  term0 <- outer(t, b - d, '-')
  term0 <- aperm(apply(term0, 1:2, cumsum), c(2,3,1))
  term1 <- apply(p * term0, 1:2, function(x) rep(sum(x, na.rm=T), n_c))
  term1 <- aperm(term1, c(2,3,1))
  dv1 <- D * p * (term0 - term1)
  dv2 <- apply(dv1 * term0, 1:2, function(x) rep(sum(x, na.rm=T), n_c))
  dv2 <- aperm(dv2, c(2,3,1)) * D * p
  dv2 <- dv1^2 / p - dv2
  list(p=p, dv1=dv1, dv2=dv2)
}

#' @rdname estimate_gpcm
#' @keywords internal
model_gpcm_dv_Pb <- function(t, a, b, d, D){
  p <- model_gpcm_prob(t, a, b, d, D)
  p <- ifelse(is.na(p), 0, p)
  n_p <- dim(p)[1]
  n_i <- dim(p)[2]
  n_c <- dim(p)[3]
  dv1 <- apply(p, 2, function(x) x %*% 1:n_c)
  dv1 <- outer(dv1, 1:n_c, '-') * D * p
  dv1 <- aperm(aperm(dv1, c(2,1,3)) * a, c(2,1,3))
  dv2 <- apply(dv1, 2, function(x) rep(x %*% 1:n_c, n_c))
  dv2 <- aperm(array(dv2, c(n_p, n_c, n_i)), c(1,3,2)) * D * p
  dv2 <- aperm(aperm(dv2, c(2,1,3)) * a, c(2,1,3))
  dv2 <- dv1^2 / p + dv2
  list(p=p, dv1=dv1, dv2=dv2)
}

#' @rdname estimate_gpcm
#' @keywords internal
model_gpcm_dv_Pd <- function(t, a, b, d, D){
  p <- model_gpcm_prob(t, a, b, d, D)
  p <- ifelse(is.na(p), 0, p)
  n_p <- dim(p)[1]
  n_i <- dim(p)[2]
  n_c <- dim(p)[3]
  pdv1 <- pdv2 <- array(NA, c(n_p, n_i, n_c, n_c))
  for(k in 1:n_c){
    dv1 <- apply(p, 1:2, function(x) (k <= 1:n_c) * 1L - sum(x[k:n_c]))
    dv1 <- aperm(dv1, c(2,3,1))
    dv1 <- D * p * dv1
    dv1 <- aperm(aperm(dv1, c(2,1,3)) * a, c(2,1,3))
    pdv1[,,,k] <- dv1
    dv2 <- apply(dv1, 1:2, function(x) rep(sum(x[k:n_c]), n_c))
    dv2 <- aperm(dv2, c(2,3,1))
    dv2 <- D * p * dv2
    dv2 <- aperm(aperm(dv2, c(2,1,3)) * a, c(2,1,3))
    dv2 <- dv1^2 / p - dv2
    pdv2[,,,k] <- dv2
  }
  list(p=p, dv1=pdv1, dv2=pdv2)
}

#' @rdname estimate_gpcm
#' @param ix the 3d indices
#' @param dvp the derivatives of P
#' @keywords internal
model_gpcm_dv_jmle <- function(ix, dvp){
  n_p <- max(ix[,1])
  n_i <- max(ix[,2])
  dv1 <- array(with(dvp, dv1[ix]/p[ix]), c(n_p, n_i))
  dv2 <- array(with(dvp, dv2[ix]/p[ix]), c(n_p, n_i)) - dv1^2
  list(dv1=dv1, dv2=dv2)  
}

#' @rdname estimate_gpcm
#' @description \code{model_gpcm_estimate_jmle} estimates the parameters using the 
#' joint maximum likelihood estimation (JMLE) method
#' @param u the observed response matrix, 2d matrix
#' @param t ability parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param a discrimination parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param b difficulty parameters, 1d vector (fixed value) or NA (freely estimate)
#' @param d category parameters, 2d matrix (fixed value) or NA (freely estimate)
#' @param D the scaling constant, 1.702 by default
#' @param iter the maximum iterations
#' @param conv the convergence criterion of the -2 log-likelihood
#' @param nr_iter the maximum iterations of newton-raphson
#' @param nr_conv the convegence criterion for newton-raphson
#' @param scale the scale of theta parameters
#' @param bounds_t bounds of ability parameters
#' @param bounds_a bounds of discrimination parameters
#' @param bounds_b bounds of location parameters
#' @param bounds_d bounds of category parameters
#' @param priors a list of prior distributions
#' @param decay decay rate
#' @param debug TRUE to print debuggin information
#' @param true_params a list of true parameters for evaluating the estimation accuracy 
#' @examples
#' \dontrun{
#' # generate data
#' x <- model_gpcm_gendata(1000, 40, 3)
#' # free calibration
#' y <- model_gpcm_estimate_jmle(x$u, true_params=x)
#' # no priors
#' y <- model_gpcm_estimate_jmle(x$u, priors=NULL, true_params=x)
#' }
#' @importFrom stats cor
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_gpcm_estimate_jmle <- function(u, t=NA, a=NA, b=NA, d=NA, D=1.702, iter=100, nr_iter=10, conv=1e-0, nr_conv=1e-3, scale=c(0, 1), bounds_t=c(-4, 4), bounds_a=c(.01, 2), bounds_b=c(-4, 4), bounds_d=c(-4, 4), priors=list(t=c(0, 1), a=c(-.1, .2), b=c(0, 1), d=c(0, 1)), decay=1, debug=FALSE, true_params=NULL){
  # configuration
  h_max <- 1.0
  tracking <- list(fit=rep(NA, iter), t=rep(NA, iter), a=rep(NA, iter), b=rep(NA, iter), d=rep(NA, iter))
  
  # initial values
  n_p <- dim(u)[1]
  n_i <- dim(u)[2]
  n_c <- length(unique(as.vector(u)))
  u_ix <- model_polytomous_3dindex(u)
  if(length(t) == 1) t <- rep(t, n_p)
  t[t_free <- is.na(t)] <- rnorm(sum(is.na(t)), 0, .01)
  if(length(a) == 1) a <- rep(a, n_i)
  a[a_free <- is.na(a)] <- rlnorm(sum(is.na(a)), -.1, .01)
  if(length(b) == 1) b <- rep(b, n_i)
  b[b_free <- is.na(b)] <- rnorm(sum(is.na(b)), 0, .01)
  if(length(d) == 1) d <- array(d, dim=c(n_i, n_c))
  d[d_free <- is.na(d)] <- rnorm(sum(is.na(d)), 0, .01)
  d_free[, 1] <- FALSE
  d[, 1] <- 0
  d[,-1] <- d[,-1] - rowMeans(d[,-1])
  
  # estimate parameters
  for (k in 1:iter){
    # t parameters
    if(any(t_free)){
      for(j in 1:nr_iter){
        dv_t <- model_gpcm_dv_jmle(u_ix, model_gpcm_dv_Pt(t, a, b, d, D))
        dv_t$dv1 <- rowSums(dv_t$dv1, na.rm=T)
        dv_t$dv2 <- rowSums(dv_t$dv2, na.rm=T)
        if(!is.null(priors$t)){
          dv_t$dv1 <- dv_t$dv1 - (t - priors$t[1]) / priors$t[2]^2
          dv_t$dv2 <- dv_t$dv2 - 1 / priors$t[2]^2
        }
        nr_t <- estimate_nr_iteration(t, t_free, dv_t, h_max, decay, bounds_t)
        t <- nr_t$param
        if(max(abs(nr_t$h)) < nr_conv) break
      }
      # rescale theta
      if(!is.null(scale)) t <- (t - mean(t)) / sd(t) * scale[2] + scale[1]
    }
    
    # b parameters
    if(any(b_free)){
      for(j in 1:nr_iter){
        dv_b <- model_gpcm_dv_jmle(u_ix, model_gpcm_dv_Pb(t, a, b, d, D))
        dv_b$dv1 <- colSums(dv_b$dv1, na.rm=T)
        dv_b$dv2 <- colSums(dv_b$dv2, na.rm=T)
        if(!is.null(priors$b)){
          dv_b$dv1 <- dv_b$dv1 - (b - priors$b[1]) / priors$b[2]^2
          dv_b$dv2 <- dv_b$dv2 - 1 / priors$b[2]^2
        }
        nr_b <- estimate_nr_iteration(b, b_free, dv_b, h_max, decay, bounds_b)
        b <- nr_b$param
        if(max(abs(nr_b$h)) < nr_conv) break
      }
    }
  
    # d parameters
    if(any(d_free)){
      for(j in 1:nr_iter){
        dv <- model_gpcm_dv_Pd(t, a, b, d, D)
        dv_dh <- array(0, c(n_i, n_c))
        for(m in 2:n_c){
          dv_d <- model_gpcm_dv_jmle(u_ix, with(dv, list(p=p, dv1=dv1[,,,m], dv2=dv2[,,,m])))
          dv_d$dv1 <- colSums(dv_d$dv1, na.rm=T)
          dv_d$dv2 <- colSums(dv_d$dv2, na.rm=T)
          if(!is.null(priors$d)){
            dv_d$dv1 <- dv_d$dv1 - (d[,m] - priors$d[1]) / priors$d[2]^2
            dv_d$dv2 <- dv_d$dv2 - 1 / priors$d[2]^2
          }
          nr_d <- estimate_nr_iteration(d[,m], d_free[,m], dv_d, h_max, decay, bounds_d)
          d[,m] <- nr_d$param
          dv_dh[,m] <- nr_d$h
        }
        d[,-1] <- d[,-1] - rowMeans(d[,-1])
        if(max(abs(dv_dh[,-1])) < nr_conv) break
      }
    }
    
    # a parameters
    if(any(a_free)){
      for(j in 1:nr_iter){
        dv_a <- model_gpcm_dv_jmle(u_ix, model_gpcm_dv_Pa(t, a, b, d, D))
        dv_a$dv1 <- colSums(dv_a$dv1, na.rm=T)
        dv_a$dv2 <- colSums(dv_a$dv2, na.rm=T)
        if(!is.null(priors$a)){
          dv_a$dv1 <- dv_a$dv1 - 1/a * (1 + (log(a)-priors$a[1])/priors$a[2]^2)
          dv_a$dv2 <- dv_a$dv2 - 1/a^2 * (1/priors$a[2]^2 - (1 + (log(a)-priors$a[1])/priors$a[2]^2))
        }
        nr_a <- estimate_nr_iteration(a, a_free, dv_a, h_max, decay, bounds_a)
        a <- nr_a$param
        if(max(abs(nr_a$h)) < nr_conv) break
      }
    }
    
    decay <- decay * decay
    
    # model fit
    loglh <- -2 * sum(model_gpcm_lh(u, t, a, b, d, D, log=T), na.rm=T)
    if(debug) cat('iter #', k, ': -2 log-likelihood = ', round(loglh, 2), '\n', sep='')
    if(k > 1  && tracking$fit[k-1] - loglh < conv) break
    tracking$fit[k] <- loglh
    if(any(t_free)) tracking$t[k] <- mean(abs(nr_t$h[t_free]))
    if(any(a_free)) tracking$a[k] <- mean(abs(nr_a$h[a_free]))
    if(any(b_free)) tracking$b[k] <- mean(abs(nr_b$h[b_free]))
    if(any(d_free)) tracking$d[k] <- mean(abs(dv_dh[d_free]))
  }
  
  # debugging
  if(debug){
    xx <- with(tracking, data.frame(iteration=1:iter, fit=fit, t=t, a=a, b=b, d=d))[1:k, ]
    xx <- melt(xx, id.vars='iteration')
    xx <- xx[!is.na(xx$value),]
    g <- ggplot(xx, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + 
      xlab('Iterations') + ylab('') + theme_bw()
    print(g)
  }
  
  # compare with true parameters
  if(!is.null(true_params)){
    xx <- rbind(data.frame(true=true_params$t, est=t, params='t'),
                data.frame(true=true_params$a, est=a, params='a'),
                data.frame(true=true_params$b, est=b, params='b'))
    for(i in 2:n_c)
      xx <- rbind(xx, data.frame(true=true_params$d[,i], est=d[,i], params=paste('d',i,sep='')))
    g <- ggplot(xx, aes_string(x="true", y="est", color="params")) + 
      geom_point(alpha=.3) + geom_smooth(method='gam', se=F) + 
      facet_wrap(~params, nrow=1, scales='free') + guides(color=F) +
      xlab('True Parameters') + ylab('Est. Parameters') + theme_bw()
    print(g)
    if(any(t_free)) cat('t: corr = ', round(cor(t, true_params$t), 3), ', rmse = ', round(rmse(t, true_params$t), 3),'\n', sep='')
    if(any(a_free)) cat('a: corr = ', round(cor(a, true_params$a), 3), ', rmse = ', round(rmse(a, true_params$a), 3),'\n', sep='')
    if(any(b_free)) cat('b: corr = ', round(cor(b, true_params$b), 3), ', rmse = ', round(rmse(b, true_params$b), 3),'\n', sep='')
    for(i in 2:n_c) if(any(d_free[,i])) cat('d_', i, ': corr = ', round(cor(d[,i], true_params$d[,i]), 3), ', rmse = ', round(rmse(d[,i], true_params$d[,i]), 3),'\n', sep='')
  }
  
  list(t=t, a=a, b=b, d=d)
}

#' @rdname estimate_gpcm
#' @keywords internal
model_gpcm_dv_mmle <- function(u_ix, quad,  pdv){
  n_p <- max(u_ix[,1])
  n_i <- max(u_ix[,2])
  n_q <- length(quad$t)
  p0 <- array(NA, c(n_p, n_i, n_q))
  for(q in 1:n_q)
    p0[,,q] <- array(pdv$p[q,,][u_ix[,-1]], c(n_p, n_i))
  p1 <- apply(p0, c(1, 3), prod, na.rm=T)
  p2 <- (p1 %*% quad$w)[,1]

  dv1 <- dv2 <- array(0, c(n_p, n_i))
  dv_common <- t(t(p1 / p2) * quad$w)
  for(q in 1:n_q)
    dv1 <- dv1 + dv_common[,q] / p0[,,q] * array(pdv$dv1[q,,][u_ix[,-1]], c(n_p, n_i))
  for(q in 1:n_q)
    dv2 <- dv2 + dv_common[,q] / p0[,,q] * (array(pdv$dv2[q,,][u_ix[,-1]], c(n_p, n_i)) - array(pdv$dv1[q,,][u_ix[,-1]], c(n_p, n_i)) * dv1)
  list(dv1=dv1, dv2=dv2)          
}


#' @rdname estimate_gpcm
#' @description \code{model_gpcm_estimate_mmle} estimates the parameters using the 
#' marginal maximum likelihood estimation (MMLE) method
#' @param quad_degree the number of quadrature points
#' @param scoring the scoring method: 'eap' or 'map'
#' @examples
#' \dontrun{
#' # generate data
#' x <- model_gpcm_gendata(1000, 40, 3)
#' # free estimation
#' y <- model_gpcm_estimate_mmle(x$u, true_params=x)
#' # no priors
#' y <- model_gpcm_estimate_mmle(x$u, priors=NULL, true_params=x)
#' }
#' @importFrom stats cor
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_gpcm_estimate_mmle <- function(u, t=NA, a=NA, b=NA, d=NA, D=1.702, iter=100, nr_iter=10, conv=1e-0, nr_conv=1e-3, bounds_t=c(-4, 4), bounds_a=c(.01, 2), bounds_b=c(-4, 4), bounds_d=c(-4, 4), priors=list(t=c(0, 1), a=c(-.1, .2), b=c(0, 1), d=c(0, 1)), decay=1, quad_degree='11', scoring=c('eap', 'map'), debug=FALSE, true_params=NULL){
  # configuration
  h_max <- 1.0
  if(is.null(priors$t)) priors$t <- c(0, 1)
  quad <- hermite_gauss(quad_degree)
  quad$w <- quad$w * exp(quad$t^2) * dnorm(quad$t, priors$t[1], priors$t[2])
  tracking <- list(fit=rep(NA, iter), t=rep(NA, iter), a=rep(NA, iter), b=rep(NA, iter), d=rep(NA, iter))
  
  # initial values
  n_p <- dim(u)[1]
  n_i <- dim(u)[2]
  n_c <- max(u) + 1
  u_ix <- model_polytomous_3dindex(u)
  if(length(t) == 1) t <- rep(t, n_p)
  t[t_free <- is.na(t)] <- rnorm(sum(is.na(t)), 0, .01)
  if(length(a) == 1) a <- rep(a, n_i)
  a[a_free <- is.na(a)] <- rlnorm(sum(is.na(a)), -.1, .01)
  if(length(b) == 1) b <- rep(b, n_i)
  b[b_free <- is.na(b)] <- rnorm(sum(is.na(b)), 0, .01)
  if(length(d) == 1) d <- array(d, dim=c(n_i, n_c))
  d[d_free <- is.na(d)] <- rnorm(sum(is.na(d)), 0, .01)
  d_free[, 1] <- FALSE
  d[, 1] <- 0
  d[,-1] <- d[,-1] - rowMeans(d[,-1])
  
  # estimate parameters
  for (k in 1:iter){
    # b parameters
    if(any(b_free)){
      for(n in 1:nr_iter){
        dv_b <- model_gpcm_dv_mmle(u_ix, quad, model_gpcm_dv_Pb(quad$t, a, b, d, D))
        dv_b$dv1 <- colSums(dv_b$dv1, na.rm=T)
        dv_b$dv2 <- colSums(dv_b$dv2, na.rm=T)
        if(!is.null(priors$b)){
          dv_b$dv1 <- dv_b$dv1 - (b - priors$b[1]) / priors$b[2]^2
          dv_b$dv2 <- dv_b$dv2 - 1 / priors$b[2]^2
        }
        nr_b <- estimate_nr_iteration(b, b_free, dv_b, h_max, decay, bounds_b)
        b <- nr_b$param
        if(max(abs(nr_b$h)) < nr_conv) break
      }
    }
    
    # d parameters
    if(any(d_free)){
      for(j in 1:nr_iter){
        dv <- model_gpcm_dv_Pd(t, a, b, d, D)
        dv_h <- array(0, c(n_i, n_c))
        for(m in 2:n_c){
          dv_d <- model_gpcm_dv_mmle(u_ix, quad, with(dv, list(p=p, dv1=dv1[,,,m], dv2=dv2[,,,m])))
          dv_d$dv1 <- colSums(dv_d$dv1, na.rm=T)
          dv_d$dv2 <- colSums(dv_d$dv2, na.rm=T)
          if(!is.null(priors$d)){
            dv_d$dv1 <- dv_d$dv1 - (d[,m] - priors$d[1]) / priors$d[2]^2
            dv_d$dv2 <- dv_d$dv2 - 1 / priors$d[2]^2
          }
          nr_d <- estimate_nr_iteration(d[,m], d_free[,m], dv_d, h_max, decay, bounds_d)
          d[,m] <- nr_d$param
          dv_h[,m] <- nr_d$h
        }
        d[,-1] <- d[,-1] - rowMeans(d[,-1])
        if(max(abs(dv_h[,-1])) < nr_conv) break
      }
    }
    
    # a parameters
    if(any(a_free)){
      for(j in 1:nr_iter){
        dv_a <- model_gpcm_dv_mmle(u_ix, quad, model_gpcm_dv_Pa(quad$t, a, b, d, D))
        dv_a$dv1 <- colSums(dv_a$dv1, na.rm=T)
        dv_a$dv2 <- colSums(dv_a$dv2, na.rm=T)
        if(!is.null(priors$a)){
          dv_a$dv1 <- dv_a$dv1 - 1/a * (1 + (log(a)-priors$a[1])/priors$a[2]^2)
          dv_a$dv2 <- dv_a$dv2 - 1/a^2 * (1/priors$a[2]^2 - (1 + (log(a)-priors$a[1])/priors$a[2]^2))
        }
        nr_a <- estimate_nr_iteration(a, a_free, dv_a, h_max, decay, bounds_a)
        a <- nr_a$param
        if(max(abs(nr_a$h)) < nr_conv) break
      }
    }
    
    # scoring
    if(any(t_free))
      t[t_free] <- switch(match.arg(scoring, scoring), 'eap'=model_gpcm_eap_scoring, 'map'=model_gpcm_map_scoring)(u, a, b, d, D, prior=priors$t, bound=bounds_t)$t[t_free]
    
    decay <- decay * decay
    
    # model fit
    loglh <- -2 * sum(model_gpcm_lh(u, t, a, b, d, D, log=T), na.rm=T)
    if(debug) cat('iter #', k, ': -2 log-likelihood = ', round(loglh, 2), '\n', sep='')
    if(k > 1  && tracking$fit[k-1] - loglh < conv) break
    tracking$fit[k] <- loglh
    if(any(a_free)) tracking$a[k] <- mean(abs(nr_a$h[a_free]))
    if(any(b_free)) tracking$b[k] <- mean(abs(nr_b$h[b_free]))
    if(any(d_free)) tracking$d[k] <- mean(abs(nr_d$h[d_free]))
  }
  
  # debugging
  if(debug){
    xx <- with(tracking, data.frame(iteration=1:iter, fit=fit, t=t, a=a, b=b, d=d))[1:k, ]
    xx <- melt(xx, id.vars='iteration')
    xx <- xx[!is.na(xx$value),]
    g <- ggplot(xx, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + 
      xlab('Iterations') + ylab('') + theme_bw()
    print(g)
  }
  
  # compare with true parameters
  if(!is.null(true_params)){
    xx <- rbind(data.frame(true=true_params$t, est=t, params='t'),
                data.frame(true=true_params$a, est=a, params='a'),
                data.frame(true=true_params$b, est=b, params='b'))
    for(i in 2:n_c)
      xx <- rbind(xx, data.frame(true=true_params$d[,i], est=d[,i], params=paste('d',i,sep='')))
    g <- ggplot(xx, aes_string(x="true", y="est", color="params")) + 
      geom_point(alpha=.3) + geom_smooth(method='gam', se=F) + 
      facet_wrap(~params, nrow=1, scales='free') + guides(color=F) +
      xlab('True Parameters') + ylab('Est. Parameters') + theme_bw()
    print(g)
    if(any(t_free)) cat('t: corr = ', round(cor(t, true_params$t), 3), ', rmse = ', round(rmse(t, true_params$t), 3),'\n', sep='')
    if(any(a_free)) cat('a: corr = ', round(cor(a, true_params$a), 3), ', rmse = ', round(rmse(a, true_params$a), 3),'\n', sep='')
    if(any(b_free)) cat('b: corr = ', round(cor(b, true_params$b), 3), ', rmse = ', round(rmse(b, true_params$b), 3),'\n', sep='')
    for(i in 2:n_c) if(any(d_free[,i])) cat('d_', i, ': corr = ', round(cor(d[,i], true_params$d[,i]), 3), ', rmse = ', round(rmse(d[,i], true_params$d[,i]), 3),'\n', sep='')
  }
  
  list(t=t, a=a, b=b, d=d)
}

#' @rdname estimate_gpcm
#' @param insert_d0 insert an initial category value
#' @param index the indices of items being plotted
#' @param intervals intervals on the x-axis
#' @param show_points TRUE to show points
#' @examples 
#' with(model_gpcm_gendata(1000, 20, 3), model_gpcm_fitplot(u, t, a, b, d, index=c(1, 3, 5)))
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
model_gpcm_fitplot <- function(u, t, a, b, d, D=1.702, insert_d0=NULL, index=NULL, intervals=seq(-3, 3, .5), show_points=TRUE){
  if(is.null(index)) index <- seq(b)
  groups <- cut(t, intervals, labels=(intervals[-length(intervals)] + intervals[-1]) / 2)
  
  obs <- aggregate(u, by=list(intervals=groups), mean, na.rm=TRUE)[, c(1, index+1)]
  obs <- melt(obs, id.vars='intervals', variable.name='items')
  obs[, 'type'] <- 'Observed'
  p <- model_gpcm_prob(t, a, b, d, D, insert_d0)
  p <- apply(p, 1:2, function(x) sum(x * (seq(x)-1), na.rm=T))
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
