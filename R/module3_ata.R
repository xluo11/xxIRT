#' Automated Test Assembly (ATA)
#' @description \code{ata} creates an \code{ata} object
#' @param pool a data frame of item parameters
#' @param nform the number of forms to be assembled
#' @param len the test length
#' @param maxselect the maximum selection of each item
#' @param debug \code{TRUE} to turn on debugging mode
#' @details
#' An \code{ata} object contains an item pool (\code{pool}), a LP object (\code{lp}),
#' and several constants such as the number of forms (\code{nform}), the size of item pool(\code{nitem}), 
#' the size of LP object (\code{nlpitem}), etc. It would also contain assembly results (\code{result}) 
#' if LP has been solved. The result is a data frame of binary code with 1 meaning an item being selected 
#' (items in rows and forms in columns). Use \code{ata_get_items} to extract selected items from results. \cr
#' @examples
#' \dontrun{
#' # generate a 100-item pool
#' # library(magrittr)
#' # library(dplyr)
#' items <- irt_model("3pl")$gendata(1, 100)$items
#' items$content <- sample(1:3, nrow(items), replace=TRUE)
#' items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
#' # ex. 1: 4 forms, 10 items, maximize b parmaters
#' x <- ata(items, 4, len=10, maxselect=1)
#' x <- ata_obj_relative(x, "b", "max")
#' x <- ata_solve(x)
#' plot(x)
#' ata_get_items(x) %>% group_by(form) %>% 
#'   summarize(a=mean(a), b=mean(b), c=mean(c))
#' # ex. 2: 4 forms, 10 items, minimize b parmaeters
#' x <- ata(items, 4, len=10, maxselect=1)
#' x <- ata_obj_relative(x, "b", "min", negative=TRUE)
#' x <- ata_solve(x)
#' plot(x)
#' ata_get_items(x) %>% group_by(form) %>% 
#'   summarize(a=mean(a), b=mean(b), c=mean(c))
#' # ex. 3: 4 forms, 10 items, maximize information at -1 and 1
#' # content distribution: 3, 3, 4; response time: avg. 55--65s
#' x <- ata(items, 4, len=10, maxselect=1) %>%
#'      ata_obj_relative(c(-1, 1), "max") %>%
#'      ata_constraint("content", min=3, max=3, level=1) %>%
#'      ata_constraint("content", min=3, max=3, level=2) %>%
#'      ata_constraint("content", min=4, max=4, level=3) %>%
#'      ata_constraint("time", min=55*10, max=65*10) %>%
#'      ata_solve()
#' plot(x)
#' ata_get_items(x) %>% group_by(form) %>% 
#'   summarize(a=mean(a), b=mean(b), c=mean(c),
#'   cont1=sum(content==1), cont2=sum(content==2), cont3=sum(content==3))
#' # ex. 4: 2 forms, 10 items, mean(b) = 0, sd(b) = 1.0, content = (3, 3, 4)
#' x <- ata(items, 2, len=10, maxselect=1) %>%
#'      ata_obj_absolute(x$pool$b, 0 * 10) %>%
#'      ata_obj_absolute((x$pool$b - 0)^2, 1 * 10) %>%
#'      ata_constraint("content", min=3, max=3, level=1) %>%
#'      ata_constraint("content", min=3, max=3, level=2) %>%
#'      ata_constraint("content", min=4, max=4, level=3) %>%
#'      ata_solve(timeout=30)
#' plot(x)
#' ata_get_items(x) %>% group_by(form) %>% 
#'   summarize(b_mean=mean(b), b_sd=sd(b),
#'   cont1=sum(content==1), cont2=sum(content==2), cont3=sum(content==3))
#' # ex. 5: 2 forms, 10 items, flat TIF over [-1, 1]
#' x <- ata(items, 2, len=10, maxselect=1) %>%
#'      ata_obj_relative(seq(-1, 1, .5), "max", flatten=0.1) %>%
#'      ata_solve(timeout=30)
#' plot(x)
#' }
#' @import lpSolveAPI
#' @export
ata <- function(pool, nform=1, len=NULL, maxselect=NULL, debug=FALSE){
  # Validate pool
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a","b","c") %in% colnames(pool))) stop("a, b, or c parameters are not found in the pool.")
  
  # Constants
  nitem <- nrow(pool)
  nlpitem <- nitem * nform
  nlp <- nlpitem + 1
  
  # Create LP: x's (binary) + y (positive)
  lp <- lpSolveAPI::make.lp(0, nlp)
  # obj fn: min/max y
  set.objfn(lp, 1, nlp) 
  set.type(lp, 1:nlpitem, "binary")
  dimnames(lp)[[2]][1:nlpitem] <- paste("f", rep(1:nform, each=nitem), ".x", rep(1:nitem, nform), sep="")
  dimnames(lp)[[2]][nlp] <- "y"  
  
  # Create the ata object
  x <- list(pool=pool, nitem=nitem, nform=nform, nlpitem=nlpitem, nlp=nlp, lp=lp, debug=debug)
  class(x) <- "ata"
  
  # Constraint: length
  if(!is.null(len)){
    if(length(len) == 1){
      x <- ata_constraint(x, 1, len, len)
    } else if(length(len) == 2){
      x <- ata_constraint(x, 1, len[1], len[2])
    } else {
      stop("invalid test length. use one or two numbers.")
    }
  }
  
  # Constraint: maxselect
  if(!is.null(maxselect)){
    x <- ata_item_maxselect(x, maxselect)
  }
  
  return(x)
}


#' @rdname ata
#' @param x the ata object
#' @param ... further arguments
#' @export
print.ata <- function(x, ...){
  cat("Assemble", x$nform, "forms from", x$nitem, "items.\n")
  if(!is.null(x$lp)) print(x$lp)
  cat("Results are ", ifelse(is.null(x$result), "not", ""), " available.")
  invisible(x)
}


#' @rdname ata
#' @import ggplot2
#' @export
plot.ata <- function(x, ...){
  # Validate inputs
  if(class(x) != "ata") stop("Not an 'ata' object: ", class(x))
  if(is.null(x$result)) stop("LP has not been solved yet")
  opts <- list(...)
  if(is.null(opts$theta)) opts$theta <- round(seq(-3, 3, .1), 1)
  t <- opts$theta
  
  # Data
  items <- ata_get_items(x)
  data <- matrix(nrow=length(t)*x$nform, ncol=3)
  for(i in 1:x$nform){
    form <- subset(items, items$form == i)
    info <- irt_stats(irt_model("3pl", theta=t, items=form), "info", summary="people", fun=sum)
    data[1:length(t) + (i - 1) * length(t), ] <- cbind(t, info, i) 
  }
  data <- data.frame(data)
  colnames(data) <- c("t", "info", "form")
  
  # Drawing
  data$form <- factor(paste("Form", data$form))
  ggplot(data, aes_string(x="t", y="info", color="form")) + 
    geom_line() + xlab(expression(theta)) + ylab("Information") + 
    theme_bw() + theme(legend.key=element_blank()) +
    guides(color=guide_legend("Forms"))
}


#' @rdname ata
#' @description \code{ata_obj_relative} adds relative (maximize/minimize) objectives to LP
#' @param coef the coefficients added to the LP
#' @param mode the optimzation mode. 'max' for maximization and 'min' for minimization
#' @param negative \code{TRUE} when the expected value of the objective function is negative
#' @param compensate \code{TRUE} when objective functions are compensatory to one another
#' @param flatten the flatten parameter to make the objective function smooth
#' @param forms the forms to which objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse all forms into one objective
#' @details 
#' When \code{coef} is a pool-size numeric vector, coefficients are used directly.
#' When \code{coef} is a variable name, variable values are used as coefficients.
#' When \code{coef} is a numeric vector unequal to pool size, information at those points are used as coefficients.\cr
#' 
#' When the expected value of the objective function is negative, set the \code{negative=TRUE}.\cr
#' 
#' The \code{compensate} argument controls whether objective functions are compensatory. 
#' For example, the ATA job wants to maximize information at -0.5 and 0.5.
#' When \code{compensate=TRUE}, the LP assembles a test maximizing the sum of information at -0.5 and 0.5.
#' When \code{compensate=FALSE}, the LP assembles a test maximizing information at each point, but not necessarily a maxmized total.\cr
#' 
#' \code{ata_obj_relative} is to maximize or minimize the objectives. There are four scenarios.
#' (1) For a maximization job with postive expected value, maximize y while sum(x) - y >= 0 and <= F (flatten).
#' (2) For a maximization job with negative expected value, minimize y while sum(x) + y >= 0 and <= F.
#' (3) For a minimization job with postive expected value, minimize y while sum(x) + y <= 0 and >= F.
#' (4) For a minimization job with negative expected value, maximize y while sum(x) - y <= 0 and >= F.
#' @import lpSolveAPI
#' @export
ata_obj_relative <- function(x, coef, mode, negative=FALSE, flatten=NULL, compensate=FALSE, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("Not an 'ata' object: ", class(x))
  forms <- ata_get_forms(x, forms, collapse)
  coef <- ata_obj_coef(x, coef, compensate)
  
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
  
  if(x$debug) cat(nrow(coef), "contraints were added to", mode, 
                  "the objectives for each of the", nrow(forms), "forms.\n")
  return(x)
}


#' @rdname ata
#' @description \code{ata_obj_absolute} adds absolute objectives to LP
#' @param target the targeted value of the objective function
#' @details 
#' \code{ata_obj_absolute} minimizes y while sum(x) + y >= target and sum(x) - y <= target. \cr
#' @import lpSolveAPI
#' @export
ata_obj_absolute <- function(x, coef, target, compensate=FALSE, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  forms <- ata_get_forms(x, forms, collapse)
  coef <- ata_obj_coef(x, coef, compensate)
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
  
  if(x$debug) cat(nrow(coef) * 2, "contraints were added to approach the 
                  targets for each of the", nrow(forms),  "forms.\n")
  return(x)    
}


#' @rdname ata
#' @description \code{ata_constraint} adds a constraint to LP
#' @param level the level value for categorical variable
#' @param min the minimum value of the constraint
#' @param max the maximum value of the constraint
#' @details 
#' For \code{ata_constraint}, set \code{coef} to a variable name 
#' in the pool and \code{level} a level value of that variable to 
#' add a categorical constraint. Set \code{coef} to a variable name and
#' leave \code{level} to default value (\code{NULL} or \code{NA}) to add
#' a quantitative constraint. Set \code{coef} to a number or a vector to 
#' add a constraint directly.\cr
#' @import lpSolveAPI
#' @export
ata_constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(!is.na(min) && !is.na(max) && min > max) stop("min is greater than max.")
  forms <- ata_get_forms(x, forms, collapse)
  coef <- ata_constraint_coef(x, coef, level)

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
  
  if(x$debug) cat(ifelse(!is.null(min) && !is.null(max) && min != max, 2, 1), 
                  "constraints were added to each of the", nrow(forms), "forms.\n")
  return(x)  
}


#' @rdname ata
#' @description \code{ata_item_maxselect} sets the maximum selection for items
#' @param items a vector of item index
#' @import lpSolveAPI
#' @export
ata_item_maxselect <- function(x, maxselect, items=NULL){
  if(class(x) != "ata") stop("Not an 'ata' object: ", class(x))
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
  
  if(x$debug) cat("maximum selection constraint was added for", 
                  length(items),"items.\n")
  return(x)
}


#' @rdname ata
#' @description \code{ata_item_enemy} adds enemy item relationship to LP
#' @import lpSolveAPI
#' @export
ata_item_enemy <- function(x, items){
  if(class(x) != "ata") stop("Not an 'ata' object: ", class(x))
  if(any(!items %in% 1:x$nitem)) stop("invalid item index")
  
  for(i in 1:x$nform){
    index <- items + (i - 1) * x$nitem
    value <- rep(1, length(items))
    add.constraint(x$lp, value, "<=", 1, index)
  }
  
  if(x$debug) cat("enemy item relationship was added for", 
                  length(items), "items.\n")
  return(x)
}


#' @rdname ata
#' @description \code{ata_item_fixedvalue} sets a fixed value range for items
#' @import lpSolveAPI
#' @export
ata_item_fixedvalue <- function(x, items, min, max, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("Not an 'ata' object: ", class(x))
  if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  forms <- ata_get_forms(x, forms, collapse)
  
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
  
  if(x$debug) cat("fix values for", n, "items to", min, "--", "max", 
                  "in forms: [", paste(forms, collapse=","), "].\n")
  return(x)
}


#' @rdname ata
#' @description \code{ata_solve} solves the LP
#' @details 
#' In \code{ata_solve}, the \code{...} are additional \code{lp.control.options}.
#' @import lpSolveAPI
#' @export
ata_solve <- function(x, ...){
  # Validate input
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$lp) || nrow(x$lp) == 0) stop("invalid lp object.")
  
  # Control parameters
  lp.control(x$lp, mip.gap=c(.10, .10), epsint=.10, presolve="lindep", timeout=3*60)
  if(length(list(...)) != 0) lp.control(x$lp, ...)
  
  # Solve
  if(solve(x$lp) != 0){
    cat("No solution.")
  } else {
    x$result <- matrix(get.variables(x$lp)[1:x$nlpitem], ncol=x$nform)
  }
  
  return(x)
}


#' @rdname ata
#' @description \code{ata_get_items} extracts items using assembly results
#' @param as.list \code{TRUE} to return a list, \code{FALSE} to return a data frame
#' @export
ata_get_items <- function(x, as.list=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object: ", class(x))
  if(is.null(x$result)) stop("not solved yet.")
    
  if(as.list) {
    items <- list()
    for(i in 1:x$nform)
      items[[paste("form", i, sep="")]] <- x$pool[x$result[, i] == 1, ]
  } else {
    items <- NULL
    for(i in 1:x$nform){
      form <- x$pool[x$result[, i] == 1, ]
      if(nrow(form) > 0) {
        form$form <- i
        items <- rbind(items, form)
      }
    }
  }
  return(items)
}

