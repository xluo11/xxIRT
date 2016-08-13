#' Utility Functions
#' @description \code{rmse} computes the root mean squared error (rsme) of two numeric vectors
#' @param x a vector/matrix of numeric values
#' @param y a vector/matrix of numeric values
#' @examples
#' rmse(1:10+rnorm(10), 1:10+rnorm(10))
#' rmse(matrix(1:100+rnorm(100), ncol=5), matrix(1:100+rnorm(100), ncol=5))
#' @family utility
#' @export
rmse <- function(x, y){
  x <- as.matrix(x)
  y <- as.matrix(y)
  if(any(dim(x) != dim(y))) stop("x and y have different dimensions.")
  sqrt(colMeans((x - y)^2))
}

#' @rdname rmse
#' @description \code{freq} counts the frequency using user-defined categories
#' @param categories a vector of valid counting values
#' @examples
#' freq(sample(1:4, 100, replace=TRUE), 5:1)
#' freq(sample(1:4, 100, replace=TRUE), 1:5)
#' @family utility
#' @export
freq <- function(x, categories){
  x <- as.vector(unlist(x))
  rs <- data.frame(table(x))
  rs <- rs$Freq[match(categories, rs$x)]
  rs[is.na(rs)] <- 0
  rs <- data.frame(x=categories, n=rs) 
  return(rs)
}

#' @rdname rmse
#' @description \code{hermite.gauss} gives a abscissae and weights of hermite-gauss quadrature (n=12)
#' @examples
#' hermite.gauss()
#' @family utility
#' @export
hermite.gauss <- function(){
  list(x=c(-3.889724897869781919272, -3.020637025120889771711, -2.279507080501059900188, -1.59768263515260479671, -0.947788391240163743705, -0.3142403762543591112766, 0.3142403762543591112766, 0.947788391240163743705, 1.59768263515260479671, 2.279507080501059900188, 3.020637025120889771711, 3.889724897869781919272), 
       wgt=c(2.65855168435630160602E-7, 8.5736870435878586546E-5, 0.003905390584629061859994, 0.0516079856158839299919, 0.2604923102641611292334, 0.570135236262479578347, 0.5701352362624795783471, 0.2604923102641611292334, 0.05160798561588392999187, 0.00390539058462906185999, 8.5736870435878586546E-5, 2.65855168435630160602E-7)
       )
}
