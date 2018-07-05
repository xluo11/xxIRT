#' Generalized partial credit model
#' @name model_gpcm
NULL

#' @rdname model_gpcm
#' @description \code{model_gpcm_prob} computes the probability of all score categories
#' @param t ability parameters, a vector
#' @param a discrimination parameters, a vector
#' @param b item location parameters when it's a matrix; item-category parameters when it's a vector
#' @param d NULL or item category parameters
#' @param D the scaling constant
#' @param add_initial the initial value added to item category parameters
#' @details 
#' The GPCM has two kinds of parameterization: (1) \code{b} as the item-category parameters or 
#' (2) \code{b - d} as the item-category parameters. In (1), b must be a matrix and d NULL. 
#' In (2), b must be a vector and d a matrix. Use \code{NA} to represent unused category. Use
#' \code{initial} to insert the values of the initial category.  
#' @export 
model_gpcm_prob <- function(t, a, b, d, D=1.702, add_initial=NULL){
  if(is.null(d)){
    if(!is.null(add_initial))
      b <- cbind(add_initial, b)
  } else {
    if(!is.null(add_initial))
      d <- cbind(add_initial, d)
    b <- b - d
  }
  p <- -1 * outer(b, t, "-") * a * D
  p <- aperm(exp(apply(p, c(1, 3), cumsum)), c(3, 2, 1))
  p <- aperm(apply(p, c(1, 2), function(x) x / sum(x, na.rm=TRUE)), c(2, 3, 1))
  p
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_info} computes the informations of all score categories
#' @export 
model_gpcm_info <- function(t, a, b, d, D=1.702, add_initial=NULL){
  p <- model_gpcm_prob(t, a, b, d, D, add_initial)
  num_people <- dim(p)[1]
  num_item <- dim(p)[2]
  num_category <- dim(p)[3]
  if(length(a) == 1) a <- rep(a, num_item)
  rs <- array(NA, dim=c(num_people, num_item, num_category))
  for(i in 1:num_item){
    x <- a[i] * (1:num_category)
    dv1 <- t(p[,i,]) * x
    dv1 <- dv1 - t(p[,i,] * colSums(dv1, na.rm=TRUE))
    rs[, i, ] <- p[,i,] * colSums(dv1 * x, na.rm=TRUE)
  }
  rs
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_onehot_response} converts 2-dimensional score
#' matrix to a 3-dimensional one-hot vector of score category
model_gpcm_onehot_response <- function(u, num_category=NULL){
  if(is.null(num_category)) num_category <- length(unique(as.vector(u)))
  rs <- apply(u, c(1, 2), function(x){
    rs <- rep(0, num_category)
    if(!is.null(x) & !is.na(x)) rs[x + 1] <- 1
    rs
  })
  aperm(rs, c(2, 3, 1))
}


#' @rdname model_gpcm
#' @param data the 3-dimensional data
model_gpcm_extract_3Ddata <- function(data, u){
  if(length(dim(data)) != 3) stop('data is not 3-dimensional')
  if(length(dim(u)) != 2) stop('response is not 2-dimensional')
  num_people <- dim(u)[1]
  num_item <- dim(u)[2]
  matrix(data[cbind(rep(1:num_people, num_item), rep(1:num_item, each=num_people), as.vector(u + 1))], nrow=num_people, ncol=num_item)
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_lik} computes the (log-)likelihood of the responses
#' @param u the observed scores (starting from 0), 2d matrix
#' @param log TRUE to return log-likelihood
#' @export
model_gpcm_lik <- function(u, t, a, b, d, D=1.702, add_initial=NULL, log=FALSE){
  p <- model_gpcm_prob(t, a, b, d, D, add_initial)
  lik <- model_gpcm_extract_3Ddata(p, u)
  if(log) lik <- log(lik)
  lik
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_gendata} generates data using the GPCM model
#' @param num_people the number of people to be generated
#' @param num_item the number of items to be generated
#' @param num_category the number of score categories
#' @param sort_b TRUE to sort b parameters of each item
#' @param set_initial the value of the initial category
#' @param t_dist the normal distribution parameters of t-parameters, a vector: c(mean, sd)
#' @param a_dist the lognormal distribution parameters of a-parameters, a vector: c(meanlog, sdlog)
#' @param b_dist the normal distribution parameters of b-parameters, a vector: c(mean, sd)
#' @param missing the proportion or number of missing responses
#' @return \code{model_gpcm_gendata} returns a list of generated data
#' @examples
#' model_gpcm_gendata(10, 5, 3)
#' model_gpcm_gendata(10, 5, 3, set_initial=0, missing=.1)
#' @importFrom stats rnorm rlnorm runif
#' @export
model_gpcm_gendata <- function(num_people, num_item, num_category, sort_b=TRUE, t=NULL, a=NULL, b=NULL, D=1.702, set_initial=NULL, t_dist=c(0, 1), a_dist=c(0, .2), b_dist=c(0, 1), missing=NULL){
  if(is.null(t)) t <- rnorm(num_people, mean=t_dist[1], sd=t_dist[2])
  if(is.null(a)) a <- rlnorm(num_item, meanlog=a_dist[1], sdlog=a_dist[2])
  if(is.null(b)) b <- matrix(rnorm(num_item * num_category, mean=b_dist[1], sd=b_dist[2]), nrow=num_item, ncol=num_category)
  if(length(t) == 1) t <- rep(t, num_people)
  if(length(a) == 1) a <- rep(a, num_item)
  if(length(a) != num_item || dim(b)[1] != num_item || dim(b)[2] != num_category)
    stop('dimensions of item parameters do not match')
  if(length(t) != num_people)
    stop('dimensions of people parameters do not match')
  if(sort_b) b <- t(apply(b, 1, sort))
  if(!is.null(set_initial)) b[, 1] <- set_initial
  p <- model_gpcm_prob(t, a, b, NULL, D, NULL)
  u <- array(NA, dim=c(num_people, num_item))
  for(i in 1:num_item){
    x <- apply(p[, i, ], 1, cumsum) <= matrix(rep(runif(num_people), each=num_category), ncol=num_people)
    u[,i] <- colSums(x, na.rm=TRUE)
  }
  if(!is.null(missing)){
    if(missing < 1) missing <- floor(missing * num_people * num_item)
    idx <- sample(num_people * num_item, missing)
    for(i in idx) u[ceiling(i/num_item), (i - 1) %% num_item + 1] <- NA
  }
  list(u=u, t=t, a=a, b=b)
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_plot} plots the item characteristics curve (ICC) or 
#' item information function curve (IIFC)
#' @param type the type of plot, prob for ICC and info for IIFC
#' @param total TRUE to sum values over items
#' @param by_item TRUE to combine categories
#' @param xaxis the values of x-axis
#' @return \code{model_gpcm_plot} returns a ggplot graph
#' @examples
#' # Figure 1 in Muraki, 1992 (APM)
#' model_gpcm_plot(a=c(1,1,.7), b=matrix(c(-2,0,2,-.5,0,2,-.5,0,2), nrow=3, byrow=TRUE), 
#'     d=NULL, D=1.0, add_initial=0, xaxis=seq(-4, 4, .1), type='prob')
#' # Figure 2 in Muraki, 1992 (APM)
#' model_gpcm_plot(a=.7, b=matrix(c(.5,0,NA,0,0,0), nrow=2, byrow=TRUE), 
#'     d=NULL, D=1.0, add_initial=0, xaxis=seq(-4, 4, .1))
#' # Figure 3 in Muraki, 1992 (APM)
#' model_gpcm_plot(a=c(.778,.946), b=matrix(c(1.759,-1.643,3.970,-2.764), nrow=2, byrow=TRUE), 
#'     d=NULL, D=1.0, add_initial=0)
#' # Figure 1 in Muraki, 1993 (APM)
#' model_gpcm_plot(a=1, b=matrix(c(0,-2,4,0,-2,2,0,-2,0,0,-2,-2,0,-2,-4), nrow=5, byrow=TRUE), 
#'     d=NULL, D=1.0)
#' # Figure 2 in Muraki, 1993 (APM)
#' model_gpcm_plot(a=1, b=matrix(c(0,-2,4,0,-2,2,0,-2,0,0,-2,-2,0,-2,-4), nrow=5, byrow=TRUE), 
#'     d=NULL, D=1.0, type='info', by_item=TRUE)
#' @import ggplot2
#' @importFrom stats aggregate
#' @export
model_gpcm_plot <- function(a, b, d, D=1.702, add_initial=NULL, type=c('prob', 'info'), by_item=FALSE, total=FALSE, xaxis=seq(-6, 6, .1)){
  rs <- switch(match.arg(type), "prob"=model_gpcm_prob, "info"=model_gpcm_info)(xaxis, a, b, d, D, add_initial)
  num_people <- dim(rs)[1]
  num_item <- dim(rs)[2]
  num_category <- dim(rs)[3]
  y <- NULL
  for(i in 1:num_item)
    y <- rbind(y, data.frame(theta=rep(xaxis, num_category), item=paste('Item', i), category=paste('Category', rep(1:num_category, each=num_people)), x=as.vector(rs[, i, ])))
  if(by_item) y <- rbind(y, cbind(aggregate(y$x, by=list(theta=y$theta, item=y$item), sum), category='Total'))
  if(total) y <- cbind(aggregate(y$x, by=list(theta=y$theta, category=y$category), sum), item='Total')
  
  ggplot(y, aes_string(x="theta", y="x", color="category")) +
    geom_line() + facet_wrap(~item, scales='free') +
    xlab(expression(theta)) + ylab(toupper(type)) +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_plot_loglik} plots the log-likelihood curves for each response vector
#' @param show_mle TRUE to print maximum likelihood values
#' @return \code{model_gpcm_plot} returns a ggplot graph
#' @examples
#' with(model_gpcm_gendata(5, 50, 3, set_initial=0), 
#'     model_gpcm_plot_loglik(u, a, b, NULL, show_mle=TRUE))
#' @import ggplot2
#' @export
model_gpcm_plot_loglik <- function(u, a, b, d, D=1.702, add_initial=NULL, xaxis=seq(-6, 6, .1), show_mle=FALSE){
  num_people <- dim(u)[1]
  num_item <- dim(u)[2]
  num_theta <- length(xaxis)
  rs <- array(NA, dim=c(num_people, num_theta))
  for(i in 1:num_theta)
    rs[, i] <- rowSums(model_gpcm_lik(u, rep(xaxis[i], num_people), a, b, d, D, add_initial, log=TRUE))
  if(show_mle) print(apply(rs, 1, function(x){xaxis[which.max(x)]}))
 
  rs <- data.frame(theta=rep(xaxis, each=num_people), people=rep(1:num_people, num_theta), value=as.vector(rs))
  rs$people <- factor(rs$people)
  ggplot(rs, aes_string(x="theta", y="value", color="people")) +
    geom_line() + xlab(expression(theta)) + ylab("Log-likelihood") +
    guides(color=FALSE) + theme_bw()
}


#' @rdname model_gpcm
#' @param prior parameters of the prior distribution
model_gpcm_dv_t <- function(u, t, a, b, D, prior=NULL){
  p <- model_gpcm_prob(t, a, b, NULL, D, NULL)
  num_people <- dim(p)[1]
  num_item <- dim(p)[2]
  num_category <- dim(p)[3]
  dv1 <- dv2 <- array(NA, dim=c(num_people, num_item))
  for(i in 1:num_item){
    x <- a[i] * (1:num_category)
    dv <- t(t(p[, i, ]) * x) - p[, i, ] * colSums(t(p[, i, ]) * x)
    dv1[, i] <- rowSums(dv / p[, i, ] * u[, i, ], na.rm=TRUE)
    dv2[, i] <- colSums(-1 * t(dv) * x, na.rm=TRUE)
  }
  dv1 <- rowSums(dv1, na.rm=TRUE)
  dv2 <- rowSums(dv2, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 - (t - prior[1]) / prior[2]^2
    dv2 <- dv2 - 1 / prior[2]^2
  }
  list(dv1=dv1, dv2=dv2)
}


#' @rdname model_gpcm
#' @param post_prob posterior distribution of the theta
model_gpcm_dv_a <- function(u, t, a, b, D, prior=NULL, post_prob=NULL){
  if(is.null(post_prob)) post_prob <- 1
  p <- model_gpcm_prob(t, a, b, NULL, D, NULL)
  num_people <- dim(p)[1]
  num_item <- dim(p)[2]
  num_category <- dim(p)[3]
  dv1 <- dv2 <- array(NA, dim=c(num_people, num_item))
  for(i in 1:num_item){
    x <- t(outer(t, b[i, ], "-")) * (1:num_category)
    dv <- p[, i, ] * t(x) - p[, i, ] * colSums(t(p[, i, ]) * x)
    dv1[, i] <- rowSums(dv / p[, i, ] * u[, i, ] * post_prob, na.rm=TRUE)
    dv2[, i] <- rowSums(-1 * dv * t(x) * post_prob, na.rm=TRUE)
  }
  dv1 <- colSums(dv1, na.rm=TRUE)
  dv2 <- colSums(dv2, na.rm=TRUE)
  if(!is.null(prior)){
    dv1 <- dv1 - (a - prior[1]) / prior[2]^2 * dnorm(a, prior[1], prior[2])
    dv2 <- dv2 - ((a - prior[1]) / prior[2] + 1 / prior[2])^2 * dnorm(a, prior[1], prior[2])
  }
  list(dv1=dv1, dv2=dv2)
}


#' @rdname model_gpcm
model_gpcm_dv_b <- function(u, t, a, b, D, prior=NULL, post_prob=NULL){
  if(is.null(post_prob)) post_prob <- 1
  p <- model_gpcm_prob(t, a, b, NULL, D, NULL)
  num_people <- dim(p)[1]
  num_item <- dim(p)[2]
  num_category <- dim(p)[3]
  dv1 <- dv2 <- array(NA, dim=c(num_item, num_category))
  for(i in 1:num_item){
    dv <- apply(p[, i, ], 1, function(x) rev(cumsum(rev(x))))
    dv_1 <- (dv - t(u[, i, ])) * a[i]
    dv1[i, ] <- colSums(t(dv_1) * post_prob, na.rm=TRUE)
    dv_2 <- apply(t(p[, i, ]) * (dv - 1), 2, function(x) rev(cumsum(rev(x)))) * a[i]^2
    dv2[i, ] <- colSums(t(dv_2) * post_prob, na.rm=TRUE)
  }
  list(dv1=dv1, dv2=dv2)
}


#' @rdname model_gpcm
#' @importFrom stats rnorm
model_gpcm_estimate_inits <- function(u, t, a, b, d, set_initial){
  num_people <- dim(u)[1]
  num_item <- dim(u)[2]
  if(is.matrix(b)) {
    num_category <- ncol(b)
  } else if(is.matrix(d)) {
    num_category <- ncol(d)
  } else {
    stop("neither b or d is a matrix")
  }
  u_onehot <- model_gpcm_onehot_response(u, num_category)
  u_dv_b <- aperm(apply(u + 1, c(1, 2), function(x) {
    if(is.na(x)) return(rep(NA, num_category))
    c(rep(1, x), rep(0, num_category - x))
    }), c(2, 3, 1))
  
  if(length(t) == 1) t <- rep(t, num_people)
  if(length(a) == 1) a <- rep(a, num_item)
  if(is.null(d) || is.na(d)){
    if(length(b) == 1) b <- matrix(b, nrow=num_item, ncol=num_category)
  } else {
    if(length(d) == 1) d <- matrix(d, nrow=num_item, ncol=num_category)
    if(length(b) == 1) b <- rep(b, num_item)
    b <- b - d
  }
  if(!is.null(set_initial)) b[, 1] <- set_initial
  t_free <- is.na(t)
  a_free <- is.na(a)
  b_free <- is.na(b)
  b_free[, 1] <- FALSE
  t[t_free] <- rnorm(sum(t_free), 0, .1)
  b[b_free] <- rnorm(sum(b_free), 0, .1)
  a[a_free] <- 1.00
  list(u_onehot=u_onehot, u_dv_b=u_dv_b, t=t, a=a, b=b, t_free=t_free, a_free=a_free, b_free=b_free)
}


#' @rdname model_gpcm
#' @param param the parameters to be updated
#' @param h change of parameters in the newton-raphson method
#' @param is_free TRUE to estimate parameters and FALSE to fix parameters
model_gpcm_estimate_nr <- function(param, h, is_free, h_max, bounds){
  h[h > h_max] <- h_max
  h[h < -h_max] <- -h_max
  h[!is_free] <- 0
  param <- param - h
  param[param < bounds[1]] <- bounds[1]
  param[param > bounds[2]] <- bounds[2]
  h <- sum(abs(h)) / sum(is_free)
  list(param=param, h=h)
}


#' @rdname model_gpcm
#' @description \code{model_gpcm_jmle} estimates the parameters using the joint MLE method
#' @param num_iter the maximum number of overall iterations
#' @param num_nr the maximum number of the newton-raphson iterations
#' @param h_max the maximum value of h in the newton-raphson method
#' @param conv the convergence criterion in -2 log-likelihood of model fit
#' @param decay the epoch decay parameter
#' @param scale the scale of theta parameters
#' @param bounds the bounds of parameters, a list
#' @param priors the priors of parameters used in the maximum a posteriori estimation
#' @param debug TRUE to print debugging information
#' @examples
#' \dontrun{
#' data_tru <- model_gpcm_gendata(1500, 30, 3, set_initial=0)
#' data_est <- model_gpcm_jmle(data_tru$u, prior=NULL, debug=TRUE)
#' evaluate_gpcm_estimation(data_tru, data_est)
#' }
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_gpcm_jmle <- function(u, t=NA, a=NA, b=NA, d=NA, D=1.702, set_initial=0, num_iter=100, num_nr=15, h_max=1.0, conv=.1, decay=0.95, scale=NULL, bounds=list(t=c(-4, 4), a=c(0.1, 2), b=c(-4, 4)), priors=list(t=c(0, 1), a=c(0, .2), b=c(0, 1)), debug=FALSE){
  params <- model_gpcm_estimate_inits(u, t, a, b, d, set_initial)
  u_onehot <- params$u_onehot
  u_dv_b <- params$u_dv_b
  t <- params$t
  a <- params$a
  b <- params$b
  t_free <- params$t_free
  a_free <- params$a_free
  b_free <- params$b_free
  
  h_t <- h_a <- h_b <- h_max
  model_fit <- -2 * log(0.001) * length(u)
  debug_info <- list(t=NULL, a=NULL, b=NULL, c=NULL, fit=NULL)
  
  # estimate parameters
  for (i in 1:num_iter){
    # debugging information
    debug_info$t <- c(debug_info$t, h_t)
    debug_info$a <- c(debug_info$a, h_a)
    debug_info$b <- c(debug_info$b, h_b)
    debug_info$fit <- c(debug_info$fit, model_fit)
    
    model_fit <- -2.0 * sum(model_gpcm_lik(u, t, a, b, NULL, D, NULL, log=TRUE), na.rm=TRUE)
    if(debug) cat('step #', i, ': model fit = ', round(model_fit, 3), '\n', sep='')
    if(debug_info$fit[i] - model_fit < conv) break
    
    # update t-parameters
    if(any(t_free)){
      for(j in 1:num_nr){
        dv <- model_gpcm_dv_t(u_onehot, t, a, b, D, priors$t)
        x <- model_gpcm_estimate_nr(t, (dv$dv1/dv$dv2)*decay^i, t_free, h_max, bounds$t)
        t <- x$param
        if(x$h < 0.005 || abs(x$h - h_t) < 0.001) break
        h_t <- x$h
      }
      if(!is.null(scale)) t <- (t - mean(t)) / sd(t) * scale[2] + scale[1] 
    }
    
    # update b-parameters
    if(any(b_free)){
      for(j in 1:num_nr){
        dv <- model_gpcm_dv_b(u_dv_b, t, a, b, D, priors$b)
        x <- model_gpcm_estimate_nr(b, (dv$dv1/dv$dv2)*decay^i, b_free, h_max, bounds$b)
        b <- x$param
        if(x$h < 0.005 || abs(x$h - h_b) < 0.001) break
        h_b <- x$h
      }
    }
    
    # update a-parameters
    if(any(a_free)){
      for(j in 1:num_nr){
        dv <- model_gpcm_dv_a(u_onehot, t, a, b, D, priors$a)
        x <- model_gpcm_estimate_nr(a, dv$dv1/dv$dv2, a_free, h_max, bounds$a)
        a <- x$param
        if(x$h < 0.001 || abs(x$h - h_a) < 0.0005) break
        h_a <- x$h
      }
    }
  }
  
  # debugging
  if(debug){
    x <- with(debug_info, data.frame(iteration=1:i, fit=fit, t=t, a=a, b=b))
    x <- x[-1,]
    x <- melt(x, id.vars="iteration")
    g <- ggplot(x, aes_string(x="iteration", y="value", color="variable")) + 
      geom_line() + facet_wrap(~variable, scales="free") + guides(color=F) + theme_bw()
    print(g)
  }
  
  list(t=t, a=a, b=b)
}
