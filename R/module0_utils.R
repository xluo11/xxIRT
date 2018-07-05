#' Utility functions
#' @name utils
NULL

#' @rdname utils
#' @description \code{rmse} computes the root mean squared error of two numeric vectors/matrices
#' @param x a vector/matrix of numeric values
#' @param y a vector/matrix of numeric values
#' @examples 
#' rmse(1 + rnorm(100), 2 + rnorm(100))
#' @export
rmse <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(dim(x) != dim(y)))
    stop("x and y have different dimensions")
  sqrt(colMeans((x - y)^2))
}

#' @rdname utils
#' @description \code{freq} computes the frequency and percentage of given values
#' @param values a vector of valid values, \code{NULL} for all values
#' @examples 
#' freq(sample(1:5, 100, replace=TRUE))
#' @export
freq <- function(x, values=NULL){
  if(is.null(values)) 
    values <- sort(unique(x))
  rs <- table(factor(x, levels=values, labels=values))
  rs <- data.frame(rs)
  colnames(rs) <- c("value", "freq")
  rs$perc <- rs$freq / length(x)
  rs$cum.freq <- cumsum(rs$freq)
  rs$cum.perc <- cumsum(rs$perc)
  rs$value <- values
  rs
}


#' @rdname utils
#' @param num_quad the number of quadrature points
hermite_gauss <- function(num_quad=20){
  if(num_quad == 20){
    quad_t <- c(-5.38748089001123,-4.60368244955074,-3.94476404011562,-3.34785456738321,-2.78880605842813,-2.25497400208927,-1.73853771211658,-1.23407621539532,-0.737473728545394,-0.245340708300901,0.245340708300901,0.737473728545394,1.23407621539532,1.73853771211658,2.25497400208927,2.78880605842813,3.34785456738321,3.94476404011562,4.60368244955074,5.38748089001123)
    quad_w <- c(2.22939364553415E-13,4.39934099227318E-10,1.08606937076928E-07,7.80255647853206E-06,0.000228338636016353,0.00324377334223786,0.0248105208874636,0.109017206020023,0.286675505362834,0.46224366960061,0.46224366960061,0.286675505362834,0.109017206020023,0.0248105208874636,0.00324377334223786,0.000228338636016353,7.80255647853206E-06,1.08606937076928E-07,4.39934099227318E-10,2.22939364553415E-13)
  } else if(num_quad == 11){
    quad_t <- c(-3.66847084655958,-2.78329009978165,-2.02594801582575,-1.32655708449493,-0.656809566882099,0,0.656809566882099,1.32655708449493,2.02594801582575,2.78329009978165,3.66847084655958)
    quad_w <- c(1.43956039371425E-06,0.000346819466323345,0.0119113954449115,0.117227875167708,0.429359752356125,0.654759286914591,0.429359752356125,0.117227875167708,0.0119113954449115,0.000346819466323345,1.43956039371425E-06)
  } else {
    stop('unsupported num_quad: use 20 or 11')
  }
  list(t=quad_t, w=quad_w)
}


#' @rdname utils
#' @description \code{evaluate_3pl_estimation} evaluates estimation results against true values
#' @param data_tru a list of true parameters
#' @param data_est a list of estimated parameters
#' @import ggplot2
#' @importFrom stats cor
#' @export
evaluate_3pl_estimation <- function(data_tru, data_est){
  data <- rbind(data.frame(param='t', tru=data_tru$t, est=data_est$t),
                data.frame(param='a', tru=data_tru$a, est=data_est$a),
                data.frame(param='b', tru=data_tru$b, est=data_est$b),
                data.frame(param='c', tru=data_tru$c, est=data_est$c))
  g <- ggplot(data, aes_string(x="tru", y="est", color="param")) +
    geom_point(alpha=.3) + geom_smooth(method='gam', se=FALSE) +
    facet_wrap(~param, nrow=1, scales='free') +
    xlab('True Parameter') + ylab('Est. Parameter') + theme_bw()
  print(g)
  for(p in unique(data$param)){
    x <- subset(data, data$param == p)
    cat('Parameter ', p, ': corr=', round(cor(x$tru, x$est), 2),
        ', rmse=', round(rmse(x$tru, x$est), 2), '\n', sep='')
  }
  invisible(NULL)
}


#' @rdname utils
#' @description \code{evaluate_gpcm_estimation} evaluates estimation results against true values
#' @import ggplot2
#' @importFrom stats cor
#' @export
evaluate_gpcm_estimation <- function(data_tru, data_est){
  num_category <- dim(data_tru$b)[2]
  data <- rbind(data.frame(param='t', tru=data_tru$t, est=data_est$t),
                data.frame(param='a', tru=data_tru$a, est=data_est$a))
  for(i in 2:num_category)
    data <- rbind(data, data.frame(param=paste('b', i, sep=''), tru=data_tru$b[,i], est=data_est$b[,i]))
  g <- ggplot(data, aes_string(x="tru", y="est", color="param")) +
    geom_point(alpha=.3) + geom_smooth(method='gam', se=FALSE) +
    facet_wrap(~param, scales='free') + 
    xlab('True Parameter') + ylab('Est. Parameter') + theme_bw()
  print(g)
  for(p in unique(data$param)){
    x <- subset(data, data$param == p)
    cat('Parameter ', p, ': corr=', round(cor(x$tru, x$est), 2),
        ', rmse=', round(rmse(x$tru, x$est), 2), '\n', sep='')
  }
  invisible(NULL)
}

