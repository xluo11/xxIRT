#' Helper functions of Model Estimation
#' @description miscellaneous helper functions for estimating IRT models
#' @name estimate_helpers
NULL

#' @rdname estimate_helpers
#' @description \code{estimate_nr_iteration} updates the parameters using the newton-raphson method
#' @param param the parameter being estimated
#' @param free TRUE to freely estimate specific parameters
#' @param dv the first and second derivatives
#' @param h_max the maximum value of h
#' @param lr the learning rate
#' @param bound the lower and upper bounds of the parameter
#' @keywords internal
estimate_nr_iteration <- function(param, free, dv, h_max, lr, bound){
  h <- dv$dv1 / dv$dv2
  h[is.na(h)] <- 0
  h <- ifelse(abs(h) > h_max, sign(h) * h_max, h) * lr
  h[!free] <- 0
  param <- param - h
  param[param < bound[1]] <- bound[1]
  param[param > bound[2]] <- bound[2]
  list(param=param, h=h)
}


#' @rdname estimate_helpers
#' @param u the observed response, 2d matrix, values start from 0
#' @keywords internal
model_polytomous_3dindex <- function(u){
  n_p <- dim(u)[1]
  n_i <- dim(u)[2]
  n_c <- max(u) + 1
  cbind(rep(1:n_p, n_i), rep(1:n_i, each=n_p), as.vector(u+1))
}

#' @rdname estimate_helpers
#' @keywords internal
model_polytomous_3dresponse <- function(u){
  n_c <- max(u) + 1
  x <- array(0, dim=c(dim(u), n_c))
  x[model_polytomous_3dindex(u)] <- 1
  x
}