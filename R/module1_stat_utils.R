#' Root Mean Square Error
#' @description compute the root mean squared error of two numeric vectors/matrices
#' @param x a vector/matrix of numeric values
#' @param y a vector/matrix of numeric values
#' @export
rmse <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(dim(x) != dim(y))) stop("x and y have different dimensions.")
  rmse <- sqrt(colMeans((x - y)^2))
  return(rmse)
}


#' Frequency
#' @description computes the frequency and percentage of given values
#' @param x a vector of raw data
#' @param val a vector of valid values, \code{NULL} for all values
#' @examples
#' freq(sample(1:4, 150, replace=TRUE))
#' freq(sample(1:4, 150, replace=TRUE), 5:1)
#' freq(sample(1:4, 150, replace=TRUE), 1:3)
#' @export
freq <- function(x, val=NULL){
  if(is.null(val)) val <- sort(unique(x))
  out <- table(factor(x, levels=val, labels=val))
  out <- data.frame(out)
  colnames(out) <- c("value", "freq")
  out$perc <- out$freq / length(x) * 100
  out$cum.freq <- cumsum(out$freq)
  out$cum.perc <- cumsum(out$perc)
  out$value <- val
  return(out)
}
