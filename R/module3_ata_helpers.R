#' ATA Helper Functions
#' @name ataHelper
NULL

#' @rdname ataHelper
#' @description \code{ata.get.forms} converts input forms into actual form indeces in LP
#' @details 
#' \code{forms} input can be \code{NULL} (all forms) or a vector of form indicies. 
#' When \code{collapse=TRUE}, the function returns a row vector; otherwise, a column vector.
ata_get_forms <- function(x, forms, collapse){
  if(is.null(forms)) {
    forms <- 1:x$nform 
  } else if(!all(forms %in% 1:x$nform)) {
    stop("invalid form indices.")
  }
  forms <- matrix(forms, ncol=1)
  if(collapse) forms <- t(forms)
  return(forms)
}

#' @rdname ataHelper
#' @description \code{ata.obj.coefficients} processes input coefficients for setting objective functions
#' @details
#' \code{coef} can be a variable name, a vector of theta points, or a n-element vector. 
#' When \code{compensate=TRUE}, add coefficients up. The function rounds results to 2 decimal places and returns a matrix.
ata_obj_coef <- function(x, coef, compensate){
  if(length(coef) == x$nitem){
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) {
    coef <- irt_stats(irt_model("3pl", theta=coef, items=x$pool), "info")
  } else if(is.character(coef) && all(coef %in% colnames(x$pool))) {
    coef <- t(x$pool[, coef])
  } else {
    stop("invalid coefficients")
  }
  if(compensate) coef <- matrix(colSums(coef), nrow=1)
  coef <- round(coef, 2)
  return(coef)
}

#' @rdname ataHelper
#' @description \code{ata_constraint_coef} processes input coefficients for adding constraints
#' @details
#' \code{coef} can be a variable name, a constant, or a n-element vector. 
#' When \code{level=NULL}, assume it's quantitaive variable; otherwise, a categorical variable.
#' Results are rounded to 2 decimal places.
ata_constraint_coef <- function(x, coef){
  if(is.numeric(coef)){
    if(length(coef) == 1){
      coef <- rep(coef, x$nitem)
    } else if (length(coef) == x$nitem) {
      coef <- coef
    } else {
      stop("invalid numeric coefficients. the length needs to be 1 or the number of items.")
    }
  } else {
    if(!coef %in% colnames(x$pool)){
      stop("Cannot find constraint variable in the pool.")
    } else if(is.null(level) || is.na(level)) {
      coef <- x$pool[,coef]
    } else {
      coef <- (x$pool[,coef] == level) * 1.0
    }
  }
  
  return(round(coef, 2))
}