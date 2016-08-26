#' Automated Test Assembly
#' @description \code{ata} creates a autmoated test assemlby (ATA) object
#' @param pool an item pool (data.frame)
#' @param nform the number of forms to assemble
#' @return \code{ata} returns an \code{ata} object
#' @details
#' An \code{ata} object contains \code{pool} (item pool), \code{nitem} (number of items in the pool),
#' \code{nform} (number of forms), \code{nlpitem} (number of item-related decision variables in LP), 
#' \code{nlp} (number of all decision variables), \code{lp} (the LP object), 
#' \code{rs.index} (indices of assembled items, if LP is solved), \code{rs.items} (assembled items, if LP is solved)
#' @examples 
#' pool <- gen.irt(1, 100)$items
#' pool$content <- sample(1:3, nrow(pool), replace=TRUE)
#' pool$time <- round(exp(rnorm(nrow(pool), log(20), .1)))
#' # assemble two 10-item forms to maximize information at theta=(-.5, .5)
#' x <- ata(pool, 2)
#' x <- ata.maxselect(x, 1)
#' x <- ata.constraint(x, "len", NA, 10, 10)
#' x <- ata.obj.rel(x, -0.5, "max")
#' x <- ata.obj.rel(x,  0.5, "max")
#' x <- ata.solve(x)
#' y1 <- x$rs.items[[1]]
#' y2 <- x$rs.items[[2]]
#' y1
#' y2
#' with(y1, plot(irt(c(-.5, .5), a, b, c), type="information"))
#' with(y2, plot(irt(c(-.5, .5), a, b, c), type="information"))
#' # assemble two 10-item forms to maximize difficulty without any items from content 1
#' x <- ata(pool, 2)
#' x <- ata.maxselect(x, 1)
#' x <- ata.constraint(x, "len", NA, 10, 10)
#' x <- ata.constraint(x, "content", 1, 0, 0)
#' x <- ata.obj.rel(x, "b", "max")
#' x <- ata.solve(x)
#' y1 <- x$rs.items[[1]]
#' y2 <- x$rs.items[[2]]
#' with(y1, plot(irt(0, a, b, c), type="information"))
#' with(y2, plot(irt(0, a, b, c), type="information"))
#' # assemble two 10-item forms to have have information (3.5, 3.5, 3.5) at (-.5, 0, .5)
#' x <- ata(pool, 2)
#' x <- ata.maxselect(x, 1)
#' x <- ata.constraint(x, "len", NA, 10, 10)
#' x <- ata.obj.abs(x, -0.5, 3.5)
#' x <- ata.obj.abs(x,  0.0, 3.5)
#' x <- ata.obj.abs(x,  0.5, 3.5)
#' x <- ata.solve(x)
#' y1 <- x$rs.items[[1]]
#' y2 <- x$rs.items[[2]]
#' with(y1, plot(irt(c(-.5,0,.5),a,b,c), type="information"))
#' with(y2, plot(irt(c(-.5,0,.5),a,b,c), type="information"))
#' # assemble two 10-item forms in which mean(b)=1, sd(b)=1
#' x <- ata(pool, 2)
#' x <- ata.maxselect(x, 1)
#' x <- ata.constraint(x, "len", NA, 10, 10)
#' x <- ata.obj.abs(x, "b", 1.0 * 10)
#' x <- ata.obj.abs(x, (pool$b-1.0)^2, 1.0 * 10)
#' x <- ata.solve(x)
#' y1 <- x$rs.items[[1]]
#' mean(y1$b)
#' sd(y1$b)
#' y2 <- x$rs.items[[2]]
#' mean(y2$b)
#' sd(y2$b)
#' x$rs.items
#' with(y1, plot(irt(0, a, b, c), type="information"))
#' with(y2, plot(irt(0, a, b, c), type="information"))
#' @family ata
#' @export
#' @import lpSolveAPI
ata <- function(pool, nform){
  # validate pool
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(any(!c("a","b","c") %in% colnames(pool))) stop("cannot find a/b/c parameters in the pool.")
  
  # constants
  nitem <- nrow(pool)
  nlpitem <- nitem * nform
  nlp <- nlpitem + 1
  
  # create lp: x's (binary) + y (positive)
  lp <- make.lp(0, nlp)
  # obj fn: min/max y
  set.objfn(lp, 1, nlp) 
  set.type(lp, 1:nlpitem, "binary")
  dimnames(lp)[[2]][1:nlpitem] <- paste("f",rep(1:nform,each=nitem),".x",rep(1:nitem,nform),sep="")
  dimnames(lp)[[2]][nlp] <- "y"  
  
  x <- list(pool=pool, nitem=nitem, nform=nform, nlpitem=nlpitem, nlp=nlp, lp=lp)
  class(x) <- "ata"
  return(x)
}

#' @rdname ata
#' @param x the ata object
#' @param ... further arguments
#' @export
print.ata <- function(x, ...){
  cat("assemble", x$nform, "forms from", x$nitem, "items.\n")
  if(is.null(x$lp)) cat("the lp object is not found.")
  else print(x$lp)
  if(is.null(x$rs.items)) cat("the lp has not been solved yet.")
  else cat("the lp has been solved. See results in rs.items and rs.index.")
}

#' @rdname ata
#' @description \code{ata.obj.rel} adds an relative (maximize/minimize) objective function to LP
#' @param value a variable name, a theta point, or a vector of numeric values for all items
#' @param mode min for minimization and max for maximization
#' @param negative.obj TRUE if the objective function is epected to have a negative value
#' @param forms a vector of form where objective function is added
#' @details 
#' \code{ata.obj.rel}: For maximization problem, if obj.fn > 0, maximize y while obj.fn - y >= 0.
#' If obj.fn < 0, minimize y while obj.fn + y >= 0. For minimization problem, if obj.fn > 0,
#' minimize y while obj.fn - y <= 0. If obj.fn < 0, maximize y while obj.fn + y <= 0. \cr
#' @family ata
#' @export
ata.obj.rel <- function(x, value, mode, negative.obj=FALSE, forms=NULL){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(forms)) forms <- 1:x$nform else if(any(!forms %in% 1:x$nform)) stop("invalid form index.")
  
  # values
  if(length(value) == x$nitem) {
    value <- round(value, 3)
  } else if(length(value) == 1) {
    if(is.character(value) && value %in% colnames(x$pool))
      value <- round(x$pool[,value], 3)
    else if(is.numeric(value))
      value <- round(info(irt(value, x$pool$a, x$pool$b, x$pool$c)), 3)
    else
      stop("invalid objective function values.")
  } else {
    stop("invalid objective function values.")
  }
    
  # objectives
  for(i in forms){
    index <- 1:x$nitem + (i - 1) * x$nitem
    if(mode == "max"){
      if(!negative.obj){
        add.constraint(x$lp, c(value, -1), ">=", 0, c(index, x$nlp))
        lp.control(x$lp, sense="max")        
      } else {
        add.constraint(x$lp, c(value, +1), ">=", 0, c(index, x$nlp))
        lp.control(x$lp, sense="min")
      }
    } else if(mode == "min"){
      if(!negative.obj){
        add.constraint(x$lp, c(value, -1), "<=", 0, c(index, x$nlp))
        lp.control(x$lp, sense="min") 
      } else {
        add.constraint(x$lp, c(value, +1), "<=", 0, c(index, x$nlp))
        lp.control(x$lp, sense="max") 
      }
    }
  }
  
  return(x)    
}

#' @rdname ata
#' @description \code{ata.obj.abs} adds an absolute objective function to LP
#' @param target the target value of the objective function
#' @details 
#' \code{ata.obj.abs} minimizes y while obj.fn + y >= target and obj.fn - y <= target. \cr
#' @family ata
#' @export
ata.obj.abs <- function(x, value, target, forms=NULL){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(forms)) forms <- 1:x$nform else if(any(!forms %in% 1:x$nform)) stop("invalid form index.")
  
  # values
  if(length(value) == x$nitem) {
    value <- round(value, 3)
  } else if(length(value) == 1) {
    if(is.character(value) && value %in% colnames(x$pool))
      value <- round(x$pool[,value], 3)
    else if(is.numeric(value))
      value <- round(info(irt(value, x$pool$a, x$pool$b, x$pool$c)), 3)
    else
      stop("invalid objective function values.")
  } else {
    stop("invalid objective function values.")
  }
  
  # objectives
  lp.control(x$lp, sense="min")
  for(i in forms){
    index <- 1:x$nitem + (i - 1) * x$nitem
    add.constraint(x$lp, c(value,  1), ">=", target, c(index, x$nlp))
    add.constraint(x$lp, c(value, -1), "<=", target, c(index, x$nlp))
  }
  
  return(x)    
}

#' @rdname ata
#' @param var the variable used to create constraint, and use "len" to add test length constraint
#' @param level a level value for categorical variable, and \code{NA} or \code{NULL} for continuous varialbe
#' @param min the minimum value
#' @param max the maximum value
#' @family ata
#' @export
ata.constraint <- function(x, var, level, min, max, forms=NULL){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(var != "len" && !var %in% colnames(x$pool)) stop("cannot find constraint variable in the pool.")
  if(min > max) stop("min is greater than max.")
  if(is.null(forms)) forms <- 1:x$nform else if(any(!forms %in% 1:x$nform)) stop("invalid form index.")
  
  # value
  if(var == "len")
    value <- rep(1.0, x$nitem)
  else if(is.null(level) || is.na(level))
    value <- round(x$pool[, var], 3)
  else
    value <- (x$pool[, var] == level) * 1.0
  
  # index
  for(i in forms){
    index <- 1:x$nitem + (i - 1) * x$nitem
    if(min == max)
      add.constraint(x$lp, value, "=", max, index)
    else{
      add.constraint(x$lp, value, "<=", max, index)
      add.constraint(x$lp, value, ">=", min, index)
    }
  }
  
  return(x)  
}

#' @rdname ata
#' @description \code{ata.maxselect} sets the maximum selection for items
#' @param maxselect the maximum times of selection
#' @param items a vector of item index
#' @family ata
#' @export
ata.maxselect <- function(x, maxselect, items=NULL){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(items)) items <- 1:x$nitem else if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  
  for(i in items){
    index <- (1:x$nform - 1) * x$nitem + i
    value <- rep(1, x$nform)
    add.constraint(x$lp, value, "<=", maxselect, index)
  }
  return(x)
}

#' @rdname ata
#' @description \code{ata.fixitem} set a fixed value for items, e.g, 1 for selection and 0 for no selection
#' @family ata
#' @export
ata.fixitem <- function(x, value, items, forms=NULL){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  if(is.null(forms)) forms <- 1:x$nform
  if(length(value) == 1) value <- rep(value, length(items))
  
  for(i in forms){
    index <- (i - 1) * x$nitem + items
    set.bounds(x$lp, lower=value, upper=value, index)
  }
  return(x)
}

#' @rdname ata
#' @description \code{ata.solve} solves the LP and returns results
#' @details 
#' In \code{ata.solve}, the \code{...} are additional \code{lp.control.options}
#' @family ata
#' @export
ata.solve <- function(x, ...){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$lp) || nrow(x$lp) == 0) stop("invalid lp object.")
  
  lp.control(x$lp, mip.gap=c(.10, .10), epsint=.10, presolve="lindep", timeout=15*60)
  if(length(list(...)) != 0) lp.control(x$lp, ...)
  if(solve(x$lp) != 0){
    cat("No solution.")
  } else {
    x$rs.index <- matrix(get.variables(x$lp)[1:x$nlpitem], ncol=x$nform)
    x$rs.items <- list()
    for(i in 1:x$nform)
      x$rs.items[[paste("f",i,sep="")]] <- x$pool[x$rs.index[,i]==1,]
  }
  return(x)
}

#' @rdname ata
#' @description \code{ata.collapse} combine assmebly results from list to one data frame
#' @family ata
#' @export
ata.collapse.rs <- function(x){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$rs.items)) stop("not solved yet.")
  
  items <- NULL
  for(i in 1:length(x$rs.items))
    items <- rbind(items, cbind(Form=i, x$rs.items[[i]]))
  x$rs.items <- items
  x
}
