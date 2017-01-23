#' MST Helper Functions
#' @name mstHelpers
NULL


#' @rdname mstHelpers
#' @description \code{mst_reindex_routes} sorts an reindexes the routes map
#' @param x a \code{mst} object
mst_reindex_routes <- function(x) {
  index <- apply(x$route[, 1:x$nstage], 1, function(r) {
    sum(r * 10^(x$nstage - 1:x$nstage))
  })
  x$route <- x$route[order(index), ]
  x$route$index <- 1:nrow(x$route)
  x
}


#' @rdname mstHelpers
#' @description \code{mst_indices_input} converts indices input to indices of routes in topdown method or modules in bottomup method
#' @param indices the route or module indices
mst_indices_input <- function(x, indices){
  if(x$method == 'topdown'){
    if(is.null(indices)){
      indices <- x$route[, 1:x$nstage]
    } else {
      indices <- subset(x$route, x$route$index %in% indices)[, 1:x$nstage]
    }
  } else if(x$method == 'bottomup') {
    if(is.null(indices)){
      indices <- data.frame(module=1:x$nmodule)
    } else {
      indices <- data.frame(module=indices)
    }
  }
  return(indices)
}

