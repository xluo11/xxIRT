#' Compute Root Mean Square Error
#' @description Compute the root mean squared error (rsme) of two numeric vectors/matrices
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


#' Count Frequency
#' @description Count (cumulative) frequency and percentage using user-defined values
#' @param x a vector being counted
#' @param val a vector of valid counting values
#' @examples
#' freq(sample(1:4, 150, replace=TRUE), 5:1)
#' freq(sample(1:4, 150, replace=TRUE), 1:3)
#' @export
freq <- function(x, val){
  out <- table(factor(x, levels=val, labels=val))
  out <- data.frame(out)
  colnames(out) <- c("value", "freq")
  out$perc <- out$freq / length(x) * 100
  out$cum.freq <- cumsum(out$freq)
  out$cum.perc <- cumsum(out$perc)
  out$value <- val
  return(out)
}
