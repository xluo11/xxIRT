#' Automated Test Assembly (ATA)
#' @name ata
#' @examples
#' \dontrun{
#' ## generate a 100-item pool
#' nitems <- 100
#' pool <- with(model_3pl_gendata(1, nitems), data.frame(id=1:nitems, a=a, b=b, c=c))
#' pool$id <- 1:nitems
#' pool$content <- sample(1:3, nitems, replace=TRUE)
#' pool$time <- round(rlnorm(nitems, log(60), .2))
#' pool$group <- sort(sample(1:round(nitems/3), nitems, replace=TRUE))
#' 
#' ## ex. 1: 6 forms, 10 items, maximize b parameter
#' x <- ata(pool, 6, len=10, max_use=1)
#' x <- ata_obj_relative(x, "b", "max")
#' x <- ata_solve(x, timeout=5)
#' sapply(x$items, function(x) c(mean=mean(x$b), std=sd(x$b)))
#' 
#' ## ex. 2: 3 forms, 10 items, minimize b parameter
#' x <- ata(pool, 3, len=10, max_use=1)
#' x <- ata_obj_relative(x, "b", "min", negative=TRUE)
#' x <- ata_solve(x, as.list=FALSE, timeout=5)
#' with(x$items, aggregate(b, by=list(form=form), mean))
#'
#' ## ex. 3: 2 forms, 10 items, mean(b) = 0, sd(b) = 1.0, content = (3, 3, 4)
#' x <- ata(pool, 2, len=10, max_use=1)
#' x <- ata_obj_absolute(x, pool$b, 0 * 10)
#' x <- ata_obj_absolute(x, (pool$b - 0)^2, 1 * 10)
#' x <- ata_constraint(x, "content", min=3, max=3, level=1)
#' x <- ata_constraint(x, "content", min=3, max=3, level=2)
#' x <- ata_constraint(x, "content", min=4, max=4, level=3)
#' x <- ata_solve(x, timeout=5)
#' sapply(x$items, function(x) c(mean=mean(x$b), sd=sd(x$b)))
#' 
#' ## ex. 4: same with ex. 3, but group-based
#' x <- ata(pool, 2, len=10, max_use=1, group="group")
#' x <- ata_obj_absolute(x, pool$b, 0 * 10)
#' x <- ata_obj_absolute(x, (pool$b - 0)^2, 1 * 10)
#' x <- ata_constraint(x, "content", min=3, max=3, level=1)
#' x <- ata_constraint(x, "content", min=3, max=3, level=2)
#' x <- ata_constraint(x, "content", min=4, max=4, level=3)
#' x <- ata_solve(x, timeout=5)
#' sapply(x$items, function(x) c(mean=mean(x$b), sd=sd(x$b), 
#'        n_items=length(unique(x$id)), n_groups=length(unique(x$group))))
#' 
#' # ex. 5: 2 forms, 10 items, flat TIF over [-1, 1]
#' x <- ata(pool, 2, len=10, max_use=1)
#' x <- ata_obj_relative(x, seq(-1, 1, .5), "max")
#' x <- ata_solve(x)
#' plot(x)
#' }
NULL

#' @rdname ata
#' @description \code{ata} creates an \code{ata} object
#' @param pool item pool (must include a, b, c parameters), a data.frame
#' @param num_form the number of forms to be assembled
#' @param len the length of each form
#' @param max_use the maximum use of each item
#' @param ... options, e.g. group, common_items, overlap_items
#' @details
#' The \code{ata} object stores the definition of a LP model. \code{ata_solve} 
#' converts definitions to the real LP object and attempts to solve it. When
#' successfully solved, \code{result} (a matrix of binary values) and \code{items}
#' (a list or data.frame of items) are added to the \code{ata} object.
#' @export
ata <- function(pool, num_form=1, len=NULL, max_use=NULL, ...){
  if(!is.data.frame(pool)) pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!'b' %in% colnames(pool)) stop('b parameters are not found in the pool')
  if(!'a' %in% colnames(pool)) warning('a parameters are not found in the pool and it is assumed a=1')
  if(!'c' %in% colnames(pool)) warning('c parameters are not found in the pool and it is assumed c=0')
  
  opts <- list(...)
  if(is.null(opts$D)) opts$D <- 1.702
  
  # break down forms for common items and overlap items
  if(!is.null(opts$common_items)){
    form_map <- cbind(1:num_form, num_form + 1)
  } else if(!is.null(opts$overlap_items)){
    form_map <- cbind(1:num_form, 1:num_form+num_form, c(num_form*2, 1:(num_form-1)+num_form))
  } else if(!is.null(opts$form_map)) {
    form_map <- opts$form_map
  } else {
    form_map <- matrix(1:num_form, ncol=1)
  }
  num_form <- max(form_map)
  
  # group item sets
  if(is.null(opts$group)) {
    group <- 1:nrow(pool)
  } else if(is.character(opts$group)) {
    if(opts$group %in% colnames(pool)) group <- pool[, opts$group] else stop('the item-set grouping variable is not found in the pool')
  } else if(length(group) != nrow(pool)) {
    stop('the item-set grouping variable has a different length from the number of items')
  } else {
    group <- opts$group
  }
  group <- as.numeric(factor(group))
  
  num_item <- length(unique(group))
  num_lpvar <- num_item * num_form + 2
  
  # LP: x's (binary) + y_lb (continuous) + y_ub (continuous)
  obj <- c(rep(0, num_lpvar - 2), 1, 1)
  names(obj) <- c(paste("f", rep(1:num_form, each=num_item), "v", rep(1:num_item, num_form), sep=""), "y_lb", "y_ub")
  # x's are binary and y is continuous
  types <- c(rep("B", num_lpvar - 2), "C", "C")
  # placehoders for fixing values
  bounds <- list(idx=c(), lb=c(), ub=c())
  # TRUE to maximize, FALSE to minimize
  max <- TRUE
  # TRUE if the y is expected to be negative
  negative <- FALSE
  # constraints: coefficient matrix, directions, and right-hand-side values
  mat <- matrix(nrow=0, ncol=num_lpvar, dimnames=list(NULL, names(obj)))
  dir <- rhs <- NULL
  
  x <- list(num_item=num_item, num_form=num_form, num_lpvar=num_lpvar, pool=pool, group=group, form_map=form_map, 
            obj=obj, mat=mat, dir=dir, rhs=rhs, types=types, bounds=bounds, max=max, negative=negative, opts=opts)
  class(x) <- "ata"
  
  # add constraint: test length
  if(!is.null(len) && length(len) == 1) x <- ata_constraint(x, 1, min=len, max=len)
  if(!is.null(len) && length(len) == 2) x <- ata_constraint(x, 1, min=len[1], max=len[2])
  if(!is.null(len) && length(len) > 2) stop("invalid length.")
  if(!is.null(opts$common_items))
    x <- ata_constraint(x, 1, min=opts$common_items, max=opts$common_items, forms=num_form, internal_index=TRUE)
  if(!is.null(opts$overlap_items))
    x <- ata_constraint(x, 1, min=opts$overlap_items, max=opts$overlap_items, forms=unique(as.vector(form_map[,-1])), internal_index=TRUE)
  # add constraint: max_use
  if(!is.null(max_use)) x <- ata_item_use(x, max=max_use)
  
  x
}


#' @rdname ata
#' @param x the ata object
#' @export
print.ata <- function(x, ...){
  cat("Assemble", x$num_form, "forms from", x$num_item, "items.\n")
  if(is.null(x$items)) {
    cat("The LP hasn't been solved yet.\n")
  } else {
    cat("The LP has been solve:\n")
    items <- x$items
    if(!is.data.frame(items))
      items <- Reduce(rbind, items, NULL)
    if(nrow(items) <= 10) {
      print(items)
    } else {
      print(items[1:5,])
      cat("...\n")
      print(items[-4:0 + nrow(items),])
    }
    cat("See more results in 'x$result' or 'x$items' (x is the ata object).")
  }
  invisible(x)
}


#' @rdname ata
#' @import ggplot2
#' @export
plot.ata <- function(x, ...){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.null(x$items)) stop("lp hasn't been solved yet")

  opts <- list(...)
  if(is.null(opts$theta)) opts$theta <- round(seq(-3, 3, .1), 1)
  num_theta <- length(opts$theta)
  
  items <- x$items
  if(is.data.frame(items)) items <- split(items, f=items$form)
  data <- lapply(items, function(item) {
    info <- rowSums(model_3pl_info(opts$theta, item$a, item$b, item$c, D=x$opts$D))
    cbind(t=opts$theta, info=info, form=item$form[1])
  })
  data <- Reduce(rbind, data, NULL)
  data <- as.data.frame(data)
  
  # Drawing
  data$form <- factor(paste("Form", data$form))
  ggplot(data, aes_string(x="t", y="info", color="form")) + 
    geom_line() + xlab(expression(theta)) + ylab("Information") + 
    theme_bw() + theme(legend.key=element_blank()) +
    guides(color=guide_legend("Forms"))
}


#' @rdname helpers
#' @description \code{ata_append_constraints} appends constraint definitions to the ata object
#' @param mat the coefficient matrix
#' @param dir direction
#' @param rhs right-hand-side value
#' @keywords internal
ata_append_constraints <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}

#' @rdname helpers
#' @description \code{ata_form_index} converts input forms into actual form indices in the LP model
#' @keywords internal
ata_form_index <- function(x, forms, collapse, internal_index){
  if(internal_index){
    if(is.null(forms))
      forms <- 1:x$num_form
    if(any(!forms %in% 1:x$num_form))
      stop('invalid form indices')
    forms <- as.matrix(forms)
  } else {
    if(is.null(forms))
      forms <- 1:nrow(x$form_map)
    if(any(!forms %in% 1:nrow(x$form_map)))
      stop('invalid form indices')
    forms <- x$form_map[forms, , drop=FALSE]
  }
  if(collapse) forms <- matrix(unique(as.vector(forms)), nrow=1)
  forms
}


#' @rdname helpers
#' @description \code{ata_obj_coef} processes input coefficients of the objective functions
#' @param compensate \code{TRUE} to combine coefficients
#' @importFrom stats aggregate
#' @keywords internal
ata_obj_coef <- function(x, coef, compensate){
  if(length(coef) == x$num_item){ 
    # if a vector of given values (item-group-level), then convert to matrix
    coef <- matrix(coef, nrow=1)
  } else if(length(coef) == nrow(x$pool)) { 
    # if a vector of given values (item-level), then aggregate and conver to matrix
    coef <- aggregate(coef, by=list(x$group), sum)[,-1]
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) { 
    # if a vector of theta's, then compute infomation and aggregate
    coef <- with(x$pool, model_3pl_info(coef, a, b, c, D=x$opts$D))
    coef <- aggregate(t(coef), by=list(group=x$group), sum)[,-1]
    coef <- t(as.matrix(coef))
  } else if(is.character(coef) && all(coef %in% colnames(x$pool))) { 
    # if a variable name, then retrieve value and aggregate
    coef <- aggregate(x$pool[,coef], by=list(group=x$group), sum)[,-1]
    coef <- t(as.matrix(coef))
  } else {
    stop("invalid coefficients")
  }
  if(compensate) coef <- matrix(colSums(coef), nrow=1)
  round(coef, 2)
}


#' @rdname ata
#' @description \code{ata_obj_relative} adds relative (maximize/minimize) objectives to LP
#' @param coef the coefficients of the objective function or the constraint
#' @param mode the optimization mode: 'max' for maximization and 'min' for minimization
#' @param negative \code{TRUE} when the expected value of the objective function is negative
#' @param forms the indices of forms where objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse all forms into one objective function
#' @param internal_index \code{TRUE} to use internal form indices
#' @details 
#' \code{ata_obj_relative}, maximize (y - beta) subject to y <= sum(x) <= y + beta, or
#' minimize (y + beta) subject to y - beta <= subject sum(x) <= y.
#' When \code{negative=TRUE}, y and beta are negative; otherwise positive.  
#' When \code{coef} is a numeric vector of the same length with the pool or forms, the given coefficients 
#' are used directly; when a variable name, variable values are used; when a numeric vector with different 
#' length with the pool, informations at those theta points are used.
#' @export
ata_obj_relative <- function(x, coef, mode=c('max', 'min'), negative=FALSE, forms=NULL, collapse=FALSE, internal_index=FALSE, ...){
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  forms <- ata_form_index(x, forms, collapse, internal_index)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  x$negative <- negative
  
  x$max <- switch(match.arg(mode), "max"=TRUE, "min"=FALSE)
  if(x$max){
    x$obj[(x$num_lpvar-1):x$num_lpvar] <- c(1, -1)
  } else {
    x$obj[(x$num_lpvar-1):x$num_lpvar] <- c(1,  1)
  }
  
  if(!is.null(opts$no_cap) && opts$no_cap)
    x$obj[x$num_lpvar] <- 0
  
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef) * 2, ncol=x$num_lpvar)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef) * 2)
  for(i in 1:nrow(forms)) {
    f <- forms[i, ]
    ind <- outer(1:x$num_item, (f - 1) * x$num_item, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)) {
      row <- (j - 1) * 2 + (i - 1) * nrow(coef) * 2
      mat[row + 1, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      if(x$max){
        mat[row + 1, (x$num_lpvar-1):x$num_lpvar] <- c(-1, 0)
        mat[row + 2, (x$num_lpvar-1):x$num_lpvar] <- c(-1, -1)
      } else {
        mat[row + 1, (x$num_lpvar-1):x$num_lpvar] <- c(-1, 1)
        mat[row + 2, (x$num_lpvar-1):x$num_lpvar] <- c(-1, 0)
      }
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- 0
    }
  }
  
  if(!is.null(opts$beta_max)) {
    if(negative){
      x$bounds$idx <- c(x$bounds$idx, x$num_lpvar)
      x$bounds$lb <- c(x$bounds$lb, -opts$beta_max)
      x$bounds$ub <- c(x$bounds$ub, NA)
    } else {
      x$bounds$idx <- c(x$bounds$idx, x$num_lpvar)
      x$bounds$lb <- c(x$bounds$lb, NA)
      x$bounds$ub <- c(x$bounds$ub, opts$beta_max)
    }
  }

  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_obj_absolute} adds absolute objectives to the LP model
#' @param target the target values of the objective function
#' @details 
#' \code{ata_obj_absolute} minimizes y0+y1 subject to t-y0 <= sum(x) <= t+y1. 
#' @export
ata_obj_absolute <- function(x, coef, target, forms=NULL, collapse=FALSE, internal_index=FALSE, ...){
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  forms <- ata_form_index(x, forms, collapse, internal_index)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  if(length(target) == 1) target <- rep(target, nrow(coef))
  if(length(target) != nrow(coef)) stop("invalid target length.")
  x$max <- FALSE
  
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef) * 2, ncol=x$num_lpvar)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef) * 2)
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    ind <- outer(1:x$num_item, (f - 1) * x$num_item, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)){
      row <- (j - 1) * 2 + (i - 1) * nrow(coef) * 2
      mat[row + 1, ind] <- rep(coef[j, ], length(f))
      mat[row + 1, x$num_lpvar - 1] <- 1
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, x$num_lpvar - 0] <- -1
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- target[j]
    }
  }
  
  if(!is.null(opts$equal_band) && opts$equal_band){
    mat <- rbind(mat, c(rep(0, x$num_lpvar - 2), 1, -1))
    dir <- c(dir, '=')
    rhs <- c(rhs, 0)
  }
  if(!is.null(opts$lb_max)){
    x$bounds$idx <- c(x$bounds$idx, x$num_lpvar - 1)
    x$bounds$lb <- c(x$bounds$lb, NA)
    x$bounds$ub <- c(x$bounds$ub, opts$lb_max)
  }
  if(!is.null(opts$ub_max)){
    x$bounds$idx <- c(x$bounds$idx, x$num_lpvar - 0)
    x$bounds$lb <- c(x$bounds$lb, NA)
    x$bounds$ub <- c(x$bounds$ub, opts$ub_max)
  }
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_constraint} adds a constraint to the LP model
#' @param level the level of the categorical variable to be constrained
#' @param min the lower bound of the constraint
#' @param max the upper bound of the constraint
#' @details 
#' When \code{level} is NA, it is assumed to impose constraints on a quantitative item property;
#' otherwise, a categorical item property. \code{coef} can be a variable name, a constant, or 
#' a numeric vector of the same length with the pool.
#' @importFrom stats aggregate
#' @export
ata_constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE, internal_index=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.na(min) && is.na(max)) return(x)
  if(!is.na(min) && !is.na(max) && min > max) stop("min is greater than max.")
  forms <- ata_form_index(x, forms, collapse, internal_index)
  
  if(length(coef) == 1 && is.character(coef) && coef %in% colnames(x$pool)) {
    # if a variable name, then retrieve and aggregate
    coef <- x$pool[, coef]
    if(!is.null(level) && !is.na(level)) coef <- as.integer(coef == level)
    coef <- aggregate(coef, by=list(group=x$group), sum)[,-1]
  } else if(length(coef) == 1 && is.numeric(coef)) {
    # if a numeric value, then replicate values and aggregate
    coef <- aggregate(rep(coef, nrow(x$pool)), by=list(group=x$group), sum)[,-1]
  } else if(length(coef) == x$num_item * ncol(forms)) {
    # if a numeric vector (item-group-level), then do nothing 
    coef <- coef
  } else if(length(coef) == nrow(x$pool)) {
    # if a numeric vector (item-level), then aggregate
    coef <- aggregate(coef, by=list(group=x$group), sum)[,-1]
  } else {
    stop("invalid coef")
  }
  
  n <- ifelse(!is.na(min) && !is.na(max) && min != max, nrow(forms) * 2, nrow(forms))
  mat <- matrix(0, nrow=n, ncol=x$num_lpvar)
  dir <- rhs <- rep(NA, n)
  for(i in 1:nrow(forms)) {
    f <- forms[i,]
    ind <- outer(1:x$num_item, (f - 1) * x$num_item, "+")
    ind <- as.vector(ind)
    if(!is.na(min) && is.na(max)) {
      mat[i, ind] <- coef
      dir[i] <- ">="
      rhs[i] <- min
    } else if(is.na(min) && !is.na(max)) {
      mat[i, ind] <- coef
      dir[i] <- "<="
      rhs[i] <- max
    } else if(min == max) {
      mat[i, ind] <- coef
      dir[i] <- "="
      rhs[i] <- min
    } else {
      mat[(i - 1) * 2 + 1, ind] <- coef
      mat[(i - 1) * 2 + 2, ind] <- coef
      dir[(i - 1) * 2 + 1:2] <- c(">=", "<=")
      rhs[(i - 1) * 2 + 1:2] <- c(min, max)
    }
  }
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_item_use} sets the minimum and maximum usage for items
#' @param items a vector of item indices
#' @export
ata_item_use <- function(x, min=NA, max=NA, items=NULL){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.na(min) && is.na(max)) stop('min and max are both NA')
  if(is.null(items)) items <- 1:x$num_item 
  if(any(!items %in% 1:x$num_item)) stop("invalid items input.")
  nitems <- length(items)
  
  n <- sum(!is.na(min), !is.na(max))
  mat <- matrix(0, nrow=nitems * n, ncol=x$num_lpvar)
  for(i in 1:length(items)) {
    ind <- items[i] + (1:x$num_form - 1) * x$num_item
    mat[(i - 1) * n + 1:n, ind] <- 1
  }
  if(!is.na(min) && is.na(max)){
    dir <- rep(">=", nitems)
    rhs <- rep(min, nitems)
  } else if(is.na(min) && !is.na(max)){
    dir <- rep("<=", nitems)
    rhs <- rep(max, nitems)
  } else {
    dir <- rep(c(">=", "<="), nitems)
    rhs <- rep(c(min, max), nitems)
  }
  
  ata_append_constraints(x, mat, dir, rhs)
}

#' @rdname ata
#' @description \code{ata_item_enemy} adds enemy item relationships to the LP model
#' @export
ata_item_enemy <- function(x, items){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$num_item)) stop("invalid item index")
  
  mat <- matrix(0, nrow=nrow(x$form_map), ncol=x$num_lpvar)
  for(i in 1:nrow(x$form_map)){
    f <- x$form_map[i, ]
    ind <- items + (f - 1) * x$num_item
    mat[i, ind] <- 1
  }
  dir <- rep("<=", x$num_form)
  rhs <- rep(1, x$num_form)
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_item_fixedvalue} sets a fixed value range for items
#' @export
ata_item_fixedvalue <- function(x, items, min=NA, max=NA, forms){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$num_item)) stop("invalid items input.")
  if(length(forms) > 1) stop('fix values in one form at each time.')
  
  n <- length(items)
  if(length(min) == 1) min <- rep(min, n)
  if(length(max) == 1) max <- rep(max, n)
  if(length(min) != n || length(max) != n) stop("invalid min or max length.")
  x$bounds$idx <- c(x$bounds$idx, items+(forms-1)*x$num_item)
  x$bounds$lb <- c(x$bounds$lb, min)
  x$bounds$ub <- c(x$bounds$ub, max)
  
  x
}


#' @rdname ata
#' @description \code{ata_solve} solves the LP model
#' @param solver use 'lpsolve' or 'glpk' solver
#' @param as.list \code{TRUE} to return results in a list; otherwise, data frame
#' @param details \code{TRUE} to print detailed information
#' @param time_limit the time limit in seconds passed along to solvers
#' @param message \code{TRUE} to print messages from solvers
#' @details 
#' \code{ata_solve} takes control options in \code{...}. 
#' For lpsolve, see \code{lpSolveAPI::lp.control.options}.
#' For glpk, see \code{glpkAPI::glpkConstants}
#' @export
ata_solve <- function(x, solver=c('lpsolve', 'glpk'), as.list=TRUE, details=TRUE, time_limit=10, message=FALSE, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  rs <- switch(match.arg(solver, solver), 'lpsolve'=ata_solve_lpsolve(x, time_limit, message, ...), 'glpk'=ata_solve_glpk(x, time_limit, message, ...))

  x$code <- rs$code
  x$status <- rs$status
  x$optimum <- rs$optimum
  x$obj_vars <- rs$obj_vars
  if(all(rs$result == 0)) {
    x$result <- x$items <- NULL
    if(details) warning("No solution for the LP model.\n")
  } else {
    if(details) cat(rs$status, ', optimum: ', round(rs$optimum, 3), ' (', paste(round(rs$obj_vars, 3), collapse=', '), ')\n', sep='')
    x$result <- rs$result
    items <- list()
    for(i in 1:nrow(x$form_map)){
      f <- x$form_map[i, ]
      f <- f[!is.na(f)]
      selection <- apply(x$result[, f, drop=FALSE] == 1, 1, any)
      selection <- seq(x$num_item)[selection]
      items[[i]] <- cbind(x$pool[x$group %in% selection, ], form=i)
    } 
    if(!as.list) items <- Reduce(rbind, items, NULL)
    x$items <- items
  }
  
  x
}

#' @rdname helpers
#' @description \code{ata_solve_lpsolve} solves the the LP model using lp_solve
#' @import lpSolveAPI
#' @keywords internal
ata_solve_lpsolve <- function(x, time_limit, message, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  lp <- make.lp(0, x$num_lpvar)
  # (max): direction
  lp.control(lp, sense=ifelse(x$max, "max", "min"))
  # set bound for y: positive = (lb=0); negative = (ub = 0)
  if(x$negative) set.bounds(lp, lower=rep(-Inf, 2), upper=rep(0, 2), (x$num_lpvar-1):x$num_lpvar)
  # (obj): objective function
  set.objfn(lp, x$obj, seq_along(x$obj))
  # (type): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "B"="binary", "I"="integer", "C"="real"))
  for(i in seq_along(types))
    set.type(lp, i, types[i])
  # (bounds): column bounds
  if(!is.null(x$bounds$idx))
    with(x$bounds, for(i in 1:length(idx)) {
      set.bounds(lp, if(!is.na(lb[i])) lower=lb[i], if(!is.na(ub[i])) upper=ub[i], columns=idx[i])
    })
  # (mat): constraints
  for(i in 1:nrow(x$mat))
    add.constraint(lp, x$mat[i,], x$dir[i], x$rhs[i])
  # solve
  lp.control(lp, mip.gap=c(.01, .01), epsint=.10, presolve="lindep", timeout=time_limit)
  lp.control(lp, verbose=ifelse(message, 'normal', 'neutral'))
  lp.control(lp, ...)
  code <- solve(lp)
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'="the model is sub-optimal",
                   '2'="the model is infeasible",
                   '3'="the model is unbounded",
                   '4'="the model is degenerate",
                   '5'="numerical failure encountered",
                   '6'="process aborted",
                   '7'="timeout",
                   '9'="the model was solved by presolve",
                   '10'="the branch and bound routine failed",
                   '11'="the branch and bound was stopped because of a break-at-first or break-at-value",
                   '12'="a feasible branch and bound solution was found",
                   '13'="no feasible branch and bound solution was found")
  optimum <- get.objective(lp)
  result <- matrix(get.variables(lp)[1:(x$num_lpvar-2)], ncol=x$num_form, byrow=FALSE)
  obj_vars <- get.variables(lp)[(x$num_lpvar-1):x$num_lpvar]
  
  if(!code %in% c(0, 1, 9)) result <- matrix(0, nrow=nrow(result), ncol=ncol(result))
  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars)
}

#' @rdname helpers
#' @description \code{ata_solve_glpk} solves the the LP model using GLPK
#' @import glpkAPI
#' @keywords internal
ata_solve_glpk <- function(x, time_limit, message, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  # set up the problem
  lp <- initProbGLPK()
  addRowsGLPK(lp, nrow(x$mat))
  addColsGLPK(lp, ncol(x$mat))
  # (max): optimization direction
  setObjDirGLPK(lp, ifelse(x$max, GLP_MAX, GLP_MIN))
  # (obj): obj functions
  setObjCoefsGLPK(lp, seq(x$num_lpvar), x$obj)
  # (types): x's = binary, y's = continuous
  for(j in seq(x$num_lpvar)[x$types == 'B'])
    setColKindGLPK(lp, j, GLP_BV)
  for(j in seq(x$num_lpvar)[x$types == 'C'])
    setColBndGLPK(lp, j, ifelse(x$negative, GLP_UP, GLP_LO), 0, 0)
  ## fixed values
  if(!is.null(x$bounds$idx))
    for(j in 1:length(x$bounds$idx))
      if(is.na(x$bound$lb[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_UP, 0, x$bounds$ub[j])
      } else if(is.na(x$bound$ub[j])){
        setColBndGLPK(lp, x$bounds$idx[j], GLP_LO, x$bounds$lb[j], 0)
      } else {
        setColBndGLPK(lp, x$bounds$idx[j], GLP_DB, x$bounds$lb[j], x$bounds$ub[j])
      }
  # # check column bounds
  # cbind(getColsLowBndsGLPK(lp, 1:x$num_lpvar), getColsUppBndsGLPK(lp, 1:x$num_lpvar))
  
  # (mat)
  ind <- x$mat != 0
  ia <- rep(1:nrow(x$mat), ncol(x$mat))[ind]
  ja <- rep(1:ncol(x$mat), each=nrow(x$mat))[ind]
  ar <- x$mat[ind]
  loadMatrixGLPK(lp, length(ar), ia, ja, ar)
  # (dir & rhs): row bounds
  dir <- sapply(x$dir, function(x) switch(x, '>='=GLP_LO, '<='=GLP_UP, '='=GLP_FX))
  setRowsBndsGLPK(lp, 1:nrow(x$mat), x$rhs, x$rhs, dir)
  # # check row bounds
  # cbind(getRowsLowBndsGLPK(lp, 1:nrow(x$mat)), getRowsUppBndsGLPK(lp, 1:nrow(x$mat)))
  
  # solve
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  setMIPParmGLPK(MIP_GAP, 0.01)
  setMIPParmGLPK(TM_LIM, 1000 * time_limit)
  setMIPParmGLPK(MSG_LEV, ifelse(message, GLP_MSG_ON, GLP_MSG_OFF))
  for(i in seq_along(opts)) setMIPParmGLPK(get(names(opts)[i]), opts[[i]])
  code <- solveMIPGLPK(lp)
  status <- switch(as.character(code),
                   '0'="optimal solution found",
                   '1'='invalid basis',
                   '2'='singular matrix',
                   '3'='ill-conditioned matrix',
                   '4'='invalid bounds',
                   '5'='solver failed',
                   '6'='objective lower limit reached',
                   '7'='objective upper limit reached',
                   '8'='iteration limit exceeded',
                   '9'='time limit exceeded',
                   '10'='no primal feasible solution',
                   '11'='no dual feasible solution',
                   '12'='root LP optimum not provided',
                   '13'='search terminated by application',
                   '14'='relative mip gap tolerance reached',
                   '15'='no primal/dual feasible solution',
                   '16'='no convergence',
                   '17'='numerical instability',
                   '18'='invalid data',
                   '19'='result out of range')
  optimum <- mipObjValGLPK(lp)
  result <- matrix(mipColsValGLPK(lp)[1:(x$num_lpvar-2)], ncol=x$num_form, byrow=FALSE)
  obj_vars <- mipColsValGLPK(lp)[(x$num_lpvar-1):x$num_lpvar]
  
  list(code=code, status=status, optimum=optimum, result=result, obj_vars=obj_vars)
}

