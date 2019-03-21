#' 3-parameter-logistic model
#' @description Routine functions for the 3PL model
#' @name model_3pl 
NULL

#' @rdname model_3pl
#' @param t ability parameters, 1d vector
#' @param a discrimination parameters, 1d vector
#' @param b difficulty parameters, 1d vector
#' @param c guessing parameters, 1d vector
#' @param D the scaling constant, 1.702 by default
#' @examples
#' with(model_3pl_gendata(10, 5), model_3pl_prob(t, a, b, c))
#' @export
model_3pl_prob <- function(t, a, b, c, D=1.702){
  p <- c + (1 - c) / (1 + exp(D * a * outer(b, t, '-')))
  t(p)
}

#' @rdname model_3pl
#' @examples
#' with(model_3pl_gendata(10, 5), model_3pl_info(t, a, b, c))
#' @export
model_3pl_info <- function(t, a, b, c, D=1.702){
  p <- t(model_3pl_prob(t, a, b, c, D))
  i <- (D * a * (p - c) / (1 - c))^2 * (1 - p) / p
  t(i)
}

#' @rdname model_3pl
#' @param u observed responses, 2d matrix
#' @param log True to return log-likelihood
#' @examples
#' with(model_3pl_gendata(10, 5), model_3pl_lh(u, t, a, b, c))
#' @export
model_3pl_lh <- function(u, t, a, b, c, D=1.702, log=FALSE){
  p <- model_3pl_prob(t, a, b, c, D)
  lh <- p^u * (1-p)^(1-u)
  if(log) lh <- log(lh)
  lh
}

#' @rdname model_3pl
#' @param param the parameter of the new scale: 't' or 'b'
#' @param mean the mean of the new scale
#' @param sd the standard deviation of the new scale
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
#' @param n_p the number of people to be generated
#' @param n_i the number of items to be generated
#' @param t_dist parameters of the normal distribution used to generate t-parameters
#' @param a_dist parameters of the lognormal distribution used to generate a-parameters
#' @param b_dist parameters of the normal distribution used to generate b-parameters
#' @param c_dist parameters of the beta distribution used to generate c-parameters
#' @param missing the proportion or number of missing responses
#' @examples
#' model_3pl_gendata(10, 5)
#' model_3pl_gendata(10, 5, a=1, c=0, missing=.1)
#' @importFrom stats rnorm rlnorm rbeta runif
#' @export
model_3pl_gendata <- function(n_p, n_i, t=NULL, a=NULL, b=NULL, c=NULL, D=1.702, t_dist=c(0, 1), a_dist=c(-.1, .2), b_dist=c(0, .7), c_dist=c(5, 46), missing=NULL){
  if(is.null(t)) t <- rnorm(n_p, mean=t_dist[1], sd=t_dist[2])
  if(is.null(a)) a <- rlnorm(n_i, meanlog=a_dist[1], sdlog=a_dist[2])
  if(is.null(b)) b <- rnorm(n_i, mean=b_dist[1], sd=b_dist[2])
  if(is.null(c)) c <- rbeta(n_i, shape1=c_dist[1], shape2=c_dist[2])
  if(length(t) == 1) t <- rep(t, n_p)
  if(length(a) == 1) a <- rep(a, n_i)
  if(length(b) == 1) b <- rep(b, n_i)
  if(length(c) == 1) c <- rep(c, n_i)
  if(length(t) != n_p) stop('wrong dimensions of t parameters')
  if(length(a) != n_i) stop('wrong dimensions of a parameters')
  if(length(b) != n_i) stop('wrong dimensions of b parameters')
  if(length(c) != n_i) stop('wrong dimensions of c parameters')
  
  p <- model_3pl_prob(t, a, b, c, D)
  x <- array(runif(length(p)), dim(p))
  u <- (p >= x) * 1L
  if(!is.null(missing)){
    missing <- floor(ifelse(missing < 1, missing * n_p * n_i, missing))
    idx <- sample(length(u), missing)
    u[cbind(ceiling(idx/n_i), (idx-1)%%n_i+1)] <- NA
  }
  list(u=u, t=t, a=a, b=b, c=c)
}


#' @rdname model_3pl
#' @param type the type of plot: 'prob' for item characteristic curve (ICC) and 
#' 'info' for item information function curve (IIFC)
#' @param total TRUE to sum values over items
#' @param xaxis the values of x-axis
#' @examples
#' with(model_3pl_gendata(10, 5), model_3pl_plot(a, b, c, type="prob"))
#' with(model_3pl_gendata(10, 5), model_3pl_plot(a, b, c, type="info", total=TRUE))
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_plot <- function(a, b, c, D=1.702, type=c('prob', 'info'), total=FALSE, xaxis=seq(-4, 4, .1)){
  x <- switch(match.arg(type), "prob"=model_3pl_prob, "info"=model_3pl_info)(t=xaxis, a=a, b=b, c=c, D=D)
  if(total) x <- rowSums(x)
  x <- data.frame(theta=xaxis, x)
  x <- melt(x, id.vars="theta")
  ggplot(x, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab('') +
    guides(color=FALSE) + theme_bw() + theme(legend.key=element_blank())
}

#' @rdname model_3pl
#' @param show_mle TRUE to print maximum likelihood estimates
#' @examples
#' with(model_3pl_gendata(5, 50), model_3pl_plot_loglh(u, a, b, c, show_mle=TRUE))
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
model_3pl_plot_loglh <- function(u, a, b, c, D=1.702, xaxis=seq(-4, 4, .1), show_mle=FALSE){
  p <- model_3pl_prob(xaxis, a, b, c, D)
  lh <- log(p) %*% t(u) + log(1 - p) %*% t(1 - u)
  if(show_mle) print(apply(lh, 2, function(x){xaxis[which.max(x)]}))
  x <- data.frame(theta=xaxis, lh)
  x <- melt(x, id.vars="theta")
  ggplot(x, aes_string(x="theta", y="value", color="variable")) +
    geom_line() + xlab(expression(theta)) + ylab("Log-likelihood") +
    guides(color=FALSE) + theme_bw()
}

