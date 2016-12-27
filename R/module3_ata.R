#' Automated Test Assembly (ATA)
#' @description \code{ata} creates an \code{ata} object
#' @param pool a data frame of items
#' @param nform the number of forms to be assembled
#' @param len the test length
#' @param maxselect the maximal times of selection of an item
#' @param debug \code{TRUE} to turn on debug mode
#' @return \code{ata} returns an \code{ata} object
#' @details
#' An \code{ata} object contains an item pool (\code{pool}), a LP object (\code{lp}),
#' and several constants such as the number of forms (\code{nform}), the size of item pool(\code{nitem}), 
#' the size of LP object (\code{nlpitem}), etc. It would contain assembly results (\code{result}) 
#' if LP has been solved. The result is a data frame of binary code with 1 indicating selected 
#' (items in rows and forms in columns). Use \code{ata.get.items} to extract actual items from results. \cr
#' @examples
#' \dontrun{
#' # ex. 1: 2 forms, 10 items, maximize b parmaters
#' items <- irt.model(model="3pl")$gen.data(1, 100)$items
#' items$content <- sample(1:3, nrow(items), replace=TRUE)
#' items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
#' x <- ata(items, 2, debug=TRUE)
#' x <- ata.obj.relative(x, "b", "max")
#' x <- ata.constraint(x, 1, 10, 10)
#' x <- ata.item.maxselect(x, 1)
#' x <- ata.solve(x)
#' plot(x)
#' y <- ata.get.items(x, as.list=TRUE)
#' mean(y[[1]]$b)
#' mean(y[[2]]$b)
#' # ex. 2: 2 forms, 10 items, minimize b parmaeters
#' x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
#' x <- ata.obj.relative(x, "b", "min", negative=TRUE)
#' x <- ata.solve(x)
#' plot(x)
#' y <- ata.get.items(x, as.list=TRUE)
#' mean(y[[1]]$b)
#' mean(y[[2]]$b)
#' # ex. 3: 2 forms, 10 items, maximize information at -0.5 and 0.5
#' # content distribution: 3, 3, 4; response time: avg. 55--65s
#' x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
#' x <- ata.obj.relative(x, c(-0.5, 0.5), "max")
#' x <- ata.constraint(x, "content", 3, 3, 1)
#' x <- ata.constraint(x, "content", 3, 3, 2)
#' x <- ata.constraint(x, "content", 4, 4, 3)
#' x <- ata.constraint(x, "time", 55*10, 65*10)
#' x <- ata.solve(x)
#' plot(x)
#' y <- ata.get.items(x, TRUE)
#' freq(y[[1]]$content, 1:3)$n
#' mean(y[[1]]$time)
#' freq(y[[2]]$content, 1:3)$n
#' mean(y[[2]]$time)
#' # ex. 4: 2 forms, 10 items, mean(b) = 0.5, sd(b) = 1.0, content = (3, 3, 4)
#' x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
#' x <- ata.obj.absolute(x, "b", 0.5 * 10)
#' x <- ata.obj.absolute(x, (x$pool$b - 0.5)^2, 1.0 * 10)
#' x <- ata.constraint(x, "content", 3, 3, 1)
#' x <- ata.constraint(x, "content", 3, 3, 2)
#' x <- ata.constraint(x, "content", 4, 4, 3)
#' x <- ata.solve(x)
#' plot(x)
#' y <- ata.get.items(x, TRUE)
#' c(mean(y[[1]]$b), sd(y[[1]]$b))
#' freq(y[[1]]$content, 1:3)$n
#' c(mean(y[[2]]$b), sd(y[[2]]$b))
#' freq(y[[2]]$content, 1:3)$n
#' # ex. 5: 2 forms, 10 items, flat TIF over [-1, 1]
#' x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
#' x <- ata.obj.relative(x, seq(-1, 1, .5), "max", negative=FALSE, flatten=.1)
#' x <- ata.solve(x)
#' y <- ata.get.items(x, TRUE)
#' plot(irt.model.3pl(items=y[[1]]), stats="information", total=TRUE)
#' plot(irt.model.3pl(items=y[[2]]), stats="information", total=TRUE)
#' }
#' @import lpSolveAPI
#' @export
ata <- function(pool, nform=1, len=NULL, maxselect=NULL, debug=FALSE){
  # validate pool
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(any(!c("a","b","c") %in% colnames(pool))) stop("a, b, or c parameters are not found in the pool.")
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
  # return the ata object
  x <- list(pool=pool, nitem=nitem, nform=nform, nlpitem=nlpitem, nlp=nlp, lp=lp, debug=debug)
  class(x) <- "ata"
  # length
  if(!is.null(len)){
    if(length(len) == 1){
      x <- ata.constraint(x, 1, len, len)
    } else if(length(len) == 2){
      x <- ata.constraint(x, 1, len[1], len[2])
    } else {
      stop("invalid test length. use one or two numbers.")
    }
  }
  # maxselect
  if(!is.null(maxselect)){
    x <- ata.item.maxselect(x, maxselect)
  }
  return(x)
}

#' @rdname ata
#' @param x the ata object
#' @param ... further arguments
#' @export
print.ata <- function(x, ...){
  cat("Assemble", x$nform, "forms from", x$nitem, "items.\n")
  if(is.null(x$lp)) {
    cat("the lp object is not found.")
  } else {
    print(x$lp)
  }
  if(is.null(x$result)) {
    cat("the lp has not been solved yet.")
  } else {
    cat("the lp has been solved.")
  }
  invisible(x)
}

#' @rdname ata
#' @import ggplot2
#' @export
plot.ata <- function(x, ...){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$result)) stop("the lp has not been solved yet.")
  items <- ata.get.items(x)
  theta <- round(seq(-3, 3, .1), 1)
  data <- NULL
  for(i in 1:x$nform){
    info <- irt.stats(irt.model.3pl(data.frame(theta=theta), subset(items, items$form == i)), "information")
    info <- rowSums(info)
    data <- rbind(data, data.frame(t=theta, info=info, form=i))
  }
  data$form <- factor(paste("Form", data$form))
  ggplot(data, aes_string(x="t", y="info", color="form")) + 
    geom_line() + xlab(expression(theta)) + ylab("Information") + 
    theme_bw() + theme(legend.key=element_blank()) +
    guides(color=guide_legend("Forms"))
}

#' @rdname ata
#' @description \code{ata.get.forms} is a helper function for converting input forms into actual form indeces in LP
ata.get.forms <- function(x, forms, collapse){
  if(is.null(forms)) {
    forms <- 1:x$nform 
  } else if(any(!forms %in% 1:x$nform)) {
    stop("invalid form indices.")
  }
  forms <- matrix(forms, ncol=1)
  if(collapse) forms <- t(forms)
  return(forms)
}

#' @rdname ata
#' @description \code{ata.obj.coefficients} is a helper function for processesing the input coefficients
ata.obj.coefficients <- function(x, coef, compensate){
  if(length(coef) == x$nitem){
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) {
    coef <- irt.stats(irt.model(data.frame(theta=coef), x$pool, NULL, model="3pl"), "information")
  } else if(is.character(coef) && all(coef %in% colnames(x$pool))) {
    coef <- t(x$pool[, coef])
  } else {
    stop("invalid coefficients")
  }
  if(compensate) coef <- matrix(colSums(coef), nrow=1)
  coef <- round(coef, 3)
  return(coef)
}

#' @rdname ata
#' @description \code{ata.obj.relative} adds relative (maximize/minimize) objectives to LP
#' @param coef the coefficients added to the LP
#' @param mode the optimzation mode (i.e. 'max' for maximization or 'min' for minimization)
#' @param negative \code{TRUE} when the expected value of the objective function is negative
#' @param compensate \code{TRUE} when objective functions are compensatory to one another
#' @param flatten the flatten parameter
#' @param forms the forms to which objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse all forms into one objective
#' @details 
#' For the function \code{ata.obj.relative} and \code{ata.obj.absolute}, 
#' when \code{coef} is a pool-size numeric vector, coefficients are used directly.
#' When \code{coef} is a variable name in the pool, values of that variable are used as coefficients.
#' When \code{coef} is a numeric vector which is unequal to pool size, information at those theta points 
#' are used as coefficients.\cr
#' When the expected value of the objective function is negative, set the \code{negative} argument to \code{TRUE}.\cr
#' The \code{compensate} argument controls whether objective functions are compensatory. 
#' For example, the ATA job wants to maximize information at -0.5 and 0.5.
#' When \code{compensate} is \code{TRUE}, the LP assembles a test maximizing the sum of information at -0.5 and 0.5.
#' When \code{compensate} is \code{FALSE}, the LP assembles a test maximizing information at each point, but not necessarily a maxmized total.\cr
#' \code{ata.obj.relative} is to maximize or minimize the objectives. There are four scenarios.
#' (1) For a maximization job with postive expected value, maximize y while sum(x) - y >= 0 and <= F (flatten).
#' (2) For a maximization job with negative expected value, minimize y while sum(x) + y >= 0 and <= F.
#' (3) For a minimization job with postive expected value, minimize y while sum(x) + y <= 0 and >= F.
#' (4) For a minimization job with negative expected value, maximize y while sum(x) - y <= 0 and >= F.
#' @import lpSolveAPI
#' @export
ata.obj.relative <- function(x, coef, mode, negative=FALSE, flatten=NULL, compensate=FALSE, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  forms <- ata.get.forms(x, forms, collapse)
  coef <- ata.obj.coefficients(x, coef, compensate)
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    index <- as.vector(outer(1:x$nitem, (f - 1) * x$nitem, "+"))
    if(mode == "max"){
      for(j in 1:nrow(coef)){
        val <- c(rep(coef[j,], length(f)), ifelse(negative, 1, -1))
        add.constraint(x$lp, val, ">=", 0, c(index, x$nlp))
        lp.control(x$lp, sense=ifelse(negative,"min", "max"))
        if(!is.null(flatten)) add.constraint(x$lp, val, "<=", flatten, c(index, x$nlp))
      }
    } else if(mode == "min"){
      for(j in 1:nrow(coef)){
        val <- c(rep(coef[j,], length(f)), ifelse(negative, 1, -1))
        add.constraint(x$lp, val, "<=", 0, c(index, x$nlp))
        lp.control(x$lp, sense=ifelse(negative,"max", "min"))
        if(!is.null(flatten)) add.constraint(x$lp, val, ">=", flatten, c(index, x$nlp))
      }
    } else {
      stop("invalid mode. use 'max' or 'min'")
    }    
  }
  if(x$debug) 
    cat(nrow(coef) * nrow(forms), "contraints were added to", mode, "the objectives.\n")
  return(x)
}

#' @rdname ata
#' @description \code{ata.obj.absolute} adds absolute objectives to LP
#' @param target the targeted value of the objective function
#' @details 
#' \code{ata.obj.absolute} minimizes y while sum(x) + y >= target and sum(x) - y <= target. \cr
#' @import lpSolveAPI
#' @export
ata.obj.absolute <- function(x, coef, target, compensate=FALSE, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  forms <- ata.get.forms(x, forms, collapse)
  coef <- ata.obj.coefficients(x, coef, compensate)
  if(length(target) == 1) target <- rep(target, nrow(coef))
  if(length(target) != nrow(coef)) stop("invalid target length.")
  lp.control(x$lp, sense="min")
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    index <- as.vector(outer(1:x$nitem, (f - 1) * x$nitem, "+"))
    for(j in 1:nrow(coef)){
      val <- c(rep(coef[j,], length(f)))
      add.constraint(x$lp, c(val,  1), ">=", target[j], c(index, x$nlp))
      add.constraint(x$lp, c(val, -1), "<=", target[j], c(index, x$nlp))
    }
  }
  if(x$debug) 
    cat(nrow(coef) * nrow(forms) * 2, "contraints were added to the objectives.\n")
  return(x)    
}

#' @rdname ata
#' @description \code{ata.constraint} adds a constraint to LP
#' @param level the level value for categorical variable
#' @param min the minimum value of the constraint
#' @param max the maximum value of the constraint
#' @details 
#' For \code{ata.constraint}, set \code{coef} to a variable name 
#' in the pool and \code{level} a level value of that variable to 
#' add a categorical constraint. Set \code{coef} to a variable name and
#' leave \code{level} to default value (\code{NULL} or \code{NA}) to add
#' a quantitative constraint. Set \code{coef} to a number or a vector to 
#' directly add a constraint.\cr
#' @import lpSolveAPI
#' @export
ata.constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(!is.na(min) && !is.na(max) && min > max) stop("min is greater than max.")
  # forms
  forms <- ata.get.forms(x, forms, collapse)
  # coefficients
  if(is.numeric(coef)){
    if(length(coef) == 1){
      coef <- round(rep(coef, x$nitem), 3)
    } else if (length(coef) == x$nitem) {
      coef <- round(coef, 3)
    } else {
      stop("invalid numeric coefficients")
    }
  } else {
    if(!coef %in% colnames(x$pool)){
      stop("cannot find constraint variable in the pool.")
    } else if(is.null(level) || is.na(level)) {
      coef <- round(x$pool[,coef], 3)
    } else {
      coef <- (x$pool[,coef] == level) * 1.0
    }
  }
  # constraints
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    index <- as.vector(outer(1:x$nitem, (f - 1) * x$nitem, "+"))
    val <- rep(coef, length(f))
    if(is.na(min) && !is.na(max)){
      add.constraint(x$lp, val, "<=", max, index)
    } else if(!is.na(min) && is.na(max)){
      add.constraint(x$lp, val, ">=", min, index)
    } else if(min == max){
      add.constraint(x$lp, val, "=", max, index)
    } else{
      add.constraint(x$lp, val, "<=", max, index)
      add.constraint(x$lp, val, ">=", min, index)
    }
  }
  if(x$debug) 
    cat(ifelse(min == max, 1, 2), 
        ifelse(is.null(level) || is.na(level), "quantitative", "categorical"), 
        "constraints were added.\n")
  return(x)  
}

#' @rdname ata
#' @description \code{ata.item.maxselect} sets the maximal times of selection for items
#' @param items a vector of item index
#' @import lpSolveAPI
#' @export
ata.item.maxselect <- function(x, maxselect, items=NULL){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(items)) {
    items <- 1:x$nitem 
  } else if(any(!items %in% 1:x$nitem)) {
    stop("invalid items input.")
  }
  for(i in items){
    index <- (1:x$nform - 1) * x$nitem + i
    value <- rep(1, x$nform)
    add.constraint(x$lp, value, "<=", maxselect, index)
  }
  if(x$debug) cat("maximum selection constraint was added for", length(items),"items.\n")
  return(x)
}

#' @rdname ata
#' @description \code{ata.item.enemy} adds enemy item relationship to LP
#' @import lpSolveAPI
#' @export
ata.item.enemy <- function(x, items){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(any(!items %in% 1:x$nitem)) stop("invalid item index")
  for(i in 1:x$nform){
    index <- items + (i - 1) * x$nitem
    value <- rep(1, length(items))
    add.constraint(x$lp, value, "<=", 1, index)
  }
  if(x$debug) cat("enemy item relationship was added for", length(items), "items.\n")
  return(x)
}

#' @rdname ata
#' @description \code{ata.fixitem} set a fixed value for items, e.g, 1 for selection and 0 for no selection
#' @import lpSolveAPI
#' @export
ata.item.fix <- function(x, items, min, max, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  forms <- ata.get.forms(x, forms, collapse)
  n <- length(items)
  if(length(min) == 1) min <- rep(min, n)
  if(length(max) == 1) max <- rep(max, n)
  if(length(min) != n || length(max) != n) stop("invalid min or max length")
  for(i in 1:nrow(forms)){
    for(j in items){
      index <- (forms[i,] - 1) * x$nitem + items[j]
      set.bounds(x$lp, lower=min[j], upper=max[j], index)
    }
  }
  if(x$debug) cat("fix values for", n, "items.\n")
  return(x)
}

#' @rdname ata
#' @description \code{ata.solve} solves the LP
#' @details 
#' In \code{ata.solve}, the \code{...} are additional \code{lp.control.options}.
#' The result (\code{x$result}) is a data frame of binary code with 1 indicating selected 
#' (items in rows and forms in columns). Use \code{ata.get.items} to extract actual items.
#' @import lpSolveAPI
#' @export
ata.solve <- function(x, ...){
  # validate
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$lp) || nrow(x$lp) == 0) stop("invalid lp object.")
  # control parameters
  lp.control(x$lp, mip.gap=c(.10, .10), epsint=.10, presolve="lindep", timeout=3*60)
  if(length(list(...)) != 0) lp.control(x$lp, ...)
  # solve
  if(solve(x$lp) != 0){
    cat("No solution.")
  } else {
    x$result <- matrix(get.variables(x$lp)[1:x$nlpitem], ncol=x$nform)
  }
  return(x)
}

#' @rdname ata
#' @description \code{ata.get.items} extracts items using assembly results
#' @param as.list \code{TRUE} to return a list, \code{FALSE} to return a data frame
#' @export
ata.get.items <- function(x, as.list=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$result)) stop("not solved yet.")
  if(as.list) {
    items <- list()
    for(i in 1:x$nform)
      items[[paste("form",i,sep="")]] <- x$pool[x$result[,i]==1,]
  } else {
    items <- NULL
    for(i in 1:x$nform){
      tmp <- x$pool[x$result[,i]==1,]
      if(nrow(tmp) > 0) {
        tmp$form <- i
        items <- rbind(items, tmp)
      }
    }
  }
  return(items)
}

