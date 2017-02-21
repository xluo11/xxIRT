#' ATA Helper Functions
#' @name ataHelpers
NULL


#' @rdname ataHelpers
#' @description \code{ata_append} adds constraint data into ata
#' @param x the ata object
#' @param mat the coefficient
#' @param dir direction
#' @param rhs right-hand-side value
ata_append <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}

#' @rdname ataHelpers
#' @description \code{ata_form_index} converts input forms into actual form indeces in LP
#' @param forms the forms indices
#' @param collapse \code{TRUE} to collapse forms
ata_form_index <- function(x, forms, collapse){
  if(is.null(forms)) {
    forms <- 1:x$nform 
  } else if(!all(forms %in% 1:x$nform)) {
    stop("invalid form indices.")
  }
  forms <- matrix(forms, ncol=1)
  if(collapse) forms <- t(forms)
  return(forms)
}


#' @rdname ataHelpers
#' @description \code{ata_obj_coef} processes input coefficients for setting objective functions
#' @param coef the coefficients
#' @param compensate \code{TRUE} to combine coefficients
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

