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
#' sapply(x$items, function(x) summary(x$b))
#' 
#' ## ex. 2: 3 forms, 10 items, minimize b parameter
#' x <- ata(pool, 3, len=10, max_use=1)
#' x <- ata_obj_relative(x, "b", "min", negative=TRUE)
#' x <- ata_solve(x, as.list=FALSE, timeout=5)
#' aggregate(x$items$b, by=list(form=x$items$form), summary)
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
#' x <- ata_obj_relative(x, seq(-1, 1, .5), "max", flatten=0.05)
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
#' @param group the grouping indices
#' @details
#' The \code{ata} object stores LP definitions in \code{obj}, \code{mat}, 
#' \code{dir}, \code{rhs}, \code{types}, \code{bounds}, \code{max}. 
#' When calling \code{ata_solve}, it converts these definitions to the 
#' real LP object. If solved successfully, two results are added to the object:
#' \code{result} (a matrix of binary selection results) and \code{items} (a list 
#' or data frame of selected items).
#' @export
ata <- function(pool, num_form=1, len=NULL, max_use=NULL, group=NULL, ...){
  if(!is.data.frame(pool)) pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a","b","c") %in% colnames(pool))) stop("a, b, or c parameters are not found in the pool")
  opts <- list(...)
  if(is.null(opts$D)) opts$D <- 1.702
  
  if(is.null(group)) group <- 1:nrow(pool)
  if(is.character(group) && (group %in% colnames(pool))) group <- pool[, group]
  group <- as.numeric(factor(group))
  num_item <- length(unique(group))
  num_lpvar <- num_item * num_form + 1
  
  # LP: x's (binary) + y (continuous)
  obj <- matrix(c(rep(0, num_lpvar - 1), 1), nrow=1)
  colnames(obj) <- c(paste("f", rep(1:num_form, each=num_item), "v", rep(1:num_item, num_form), sep=""), "y")
  # x's are binary and y is continuous
  types <- c(rep("B", num_lpvar - 1), "C")
  # placehoders for fixing values
  bounds <- list(idx=c(), lb=c(), ub=c())
  # TRUE to maximize, FALSE to minimize
  max <- TRUE
  # TRUE if the y is expected to be negative
  negative <- FALSE
  # constraints: coefficient matrix, directions, and right-hand-side values
  mat <- matrix(nrow=0, ncol=num_lpvar, dimnames=list(NULL, colnames(obj)))
  dir <- rhs <- NULL
  
  x <- list(num_item=num_item, num_form=num_form, num_lpvar=num_lpvar, pool=pool, group=group, obj=obj,
            mat=mat, dir=dir, rhs=rhs, types=types, bounds=bounds, max=max, negative=negative, opts=opts)
  class(x) <- "ata"
  
  # add constraint: test length
  if(!is.null(len) && length(len) == 1) x <- ata_constraint(x, 1, min=len, max=len)
  if(!is.null(len) && length(len) == 2) x <- ata_constraint(x, 1, min=len[1], max=len[2])
  if(!is.null(len) && length(len) > 2) stop("invalid length.")
  # add constraint: max_use
  if(!is.null(max_use)) x <- ata_item_use(x, max=max_use)
  
  x
}


#' @rdname ata
#' @param x the ata object
#' @param ... further arguments
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


#' @rdname ata
#' @description \code{ata_append_constraints} appends constraint definitions to the ata object
#' @param mat the coefficient matrix
#' @param dir direction
#' @param rhs right-hand-side value
ata_append_constraints <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}

#' @rdname ata
#' @description \code{ata_form_index} converts input forms into actual form indices in LP
ata_form_index <- function(x, forms, collapse){
  if(is.null(forms)) {
    forms <- 1:x$num_form 
  } else if(!all(forms %in% 1:x$num_form)) {
    stop("invalid form indices.")
  }
  forms <- matrix(forms, ncol=1)
  if(collapse) forms <- t(forms)
  forms
}


#' @rdname ata
#' @description \code{ata_obj_coef} processes input coefficients of the objective functions
#' @param compensate \code{TRUE} to combine coefficients
#' @importFrom stats aggregate
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
#' @param flatten the flatten parameter to make the objective function flat
#' @param forms the indices of forms where objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse all forms into one objective function
#' @details 
#' To maximize the LP, it is to maximize y while subject to sum(x) - y >= 0 and <= F (flatten parameter).
#' To minimize the LP, it is to minimize y while subject to sum(x) - y <= 0 and >= F.
#' By default, y is non-negative. When \code{negative=TRUE}, y is set to be negative. \cr 
#' When \code{coef} is a pool-size or form-size numeric vector, coefficients are used directly.
#' When \code{coef} is a variable name, variable values are used as coefficients.
#' When \code{coef} is a numeric vector unequal to pool size, information at those points are used as coefficients.
#' @export
ata_obj_relative <- function(x, coef, mode=c('max', 'min'), negative=FALSE, flatten=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  forms <- ata_form_index(x, forms, collapse)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  x$negative <- negative
  x$max <- switch(match.arg(mode), "max"=TRUE, "min"=FALSE)
  
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef), ncol=x$num_lpvar)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef))
  for(i in 1:nrow(forms)) {
    f <- forms[i, ]
    ind <- outer(1:x$num_item, (f - 1) * x$num_item, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)) {
      row <- j + (i - 1) * nrow(coef)
      mat[row, ind] <- rep(coef[j, ], length(f))
      mat[row, x$num_lpvar] <- -1
      dir[row] <- ifelse(x$max, ">=", "<=")
      rhs[row] <- 0
    }
  }
  
  if(!is.null(flatten)) {
    mat <- rbind(mat, mat)
    dir <- c(dir, rep(ifelse(x$max, "<=", ">="), nrow(forms) * nrow(coef)))
    rhs <- c(rhs, rep(flatten, nrow(forms) * nrow(coef)))
  }
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_obj_absolute} adds absolute objectives to LP
#' @param target the target of the objective function
#' @details 
#' \code{ata_obj_absolute} is to minimize y while subject to sum(x) + y >= T and sum(x) - y <= T. 
#' @export
ata_obj_absolute <- function(x, coef, target, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  forms <- ata_form_index(x, forms, collapse)
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
      mat[row + 1, x$num_lpvar] <- 1
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, x$num_lpvar] <- -1
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- target[j]
    }
  }
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_constraint} adds a constraint to LP
#' @param level the level value for categorical variable
#' @param min the lower bound of the constraint
#' @param max the upper bound of the constraint
#' @details 
#' For \code{ata_constraint}, set \code{coef} to a variable name and \code{level} a level 
#' of that variable to add a categorical constraint. Set \code{coef} to a variable name and
#' leave \code{level} to default value (\code{NULL} or \code{NA}) to add a quantitative constraint. 
#' Set \code{coef} to a constant or a vector to add a constraint directly.
#' @importFrom stats aggregate
#' @export
ata_constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.na(min) && is.na(max)) return(x)
  if(!is.na(min) && !is.na(max) && min > max) stop("min is greater than max.")
  forms <- ata_form_index(x, forms, collapse)
  
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
      dir[i] <- "=="
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
#' @description \code{ata_item_use} sets the minimum and maximum use for items
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
#' @description \code{ata_item_enemy} adds enemy item relationship to LP
#' @export
ata_item_enemy <- function(x, items){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$num_item)) stop("invalid item index")
  
  mat <- matrix(0, nrow=x$num_form, ncol=x$num_lpvar)
  for(i in 1:x$num_form){
    ind <- items + (i - 1) * x$num_item
    mat[i, ind] <- 1
  }
  dir <- rep("<=", x$num_form)
  rhs <- rep(1, x$num_form)
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_item_fixedvalue} sets a fixed value range for items
#' @export
ata_item_fixedvalue <- function(x, items, min=NA, max=NA, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$num_item)) stop("invalid items input.")
  forms <- ata_form_index(x, forms, collapse)
  n <- length(items)
  if(length(min) == 1) min <- rep(min, n)
  if(length(max) == 1) max <- rep(max, n)
  if(length(min) != n || length(max) != n) stop("invalid min or max length.")
  idx <- as.vector(outer(items, (forms - 1) * x$num_item, "+"))
  x$bounds$idx <- c(x$bounds$idx, idx)
  x$bounds$lb <- c(x$bounds$lb, rep(min, length(forms)))
  x$bounds$ub <- c(x$bounds$ub, rep(max, length(forms)))
  x
}


#' @rdname ata
#' @description \code{ata_solve} solves the LP object
#' @param as.list \code{TRUE} to return results in a list; otherwise, data frame
#' @param timeout the time limit in seconds
#' @param mip_gap the mip gap parameter
#' @param verbose the message parameter
#' @param warning \code{TRUE} to output warning message when LP is not solved
#' @export
ata_solve <- function(x, as.list=TRUE, timeout=10, mip_gap=0.1, verbose="neutral", warning=FALSE, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  rs <- ata_solve_lpsolve(x, timeout=timeout, mip.gap=mip_gap, verbose=verbose, ...)
  if(!rs$status %in% c(0, 1)) rs$result <- array(0L, dim(rs$result))
  
  x$status <- rs$status
  x$optimum <- rs$optimum
  if(all(rs$result == 0)) {
    x$result <- x$items <- NULL
    if(warning) cat("No solution for LP.\n")
  } else {
    x$result <- rs$result
    items <- list()
    for(i in 1:x$num_form){
      # items[[i]] <- cbind(x$pool[x$result[,i] == 1, ], form=i)
      selected_groups <- (1:x$num_item)[x$result[,i] == 1]
      items[[i]] <- cbind(x$pool[x$group %in% selected_groups, ], form=i)
    } 
    if(!as.list) items <- Reduce(rbind, items, NULL)
    x$items <- items
  }
  
  x
}

#' @rdname ata
#' @description \code{ata_solve_lpsolve} solves the LP using LpSolve
#' @import lpSolveAPI
ata_solve_lpsolve <- function(x, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  lp <- make.lp(0, x$num_lpvar)
  # (max): direction
  lp.control(lp, sense=ifelse(x$max, "max", "min"))
  # set bound for y: positive = (lb=0); negative = (ub = 0)
  if(x$negative) set.bounds(lp, lower=-Inf, upper=0, x$num_lpvar)
  # (obj): objective function
  for(i in seq_along(x$obj))
    if(x$obj[i] != 0)
      set.objfn(lp, x$obj[i], i) 
  # (type): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "B"="binary", "I"="integer", "C"="real"))
  for(i in seq_along(types))
    set.type(lp, i, types[i])
  # (bounds): column bounds
  with(x$bounds, for(i in seq_along(idx)) {
    set.bounds(lp, if(!is.na(lb[i])) lower=lb[i], if(!is.na(ub[i])) upper=ub[i], columns=idx[i])
  })
  # (mat): constraints
  dir <- gsub("==", "=", x$dir)
  for(i in 1:nrow(x$mat))
    add.constraint(lp, x$mat[i,], dir[i], x$rhs[i])
  # solve
  lp.control(lp, mip.gap=c(.01, .10), epsint=.10, presolve="lindep", timeout=10)
  lp.control(lp, ...)
  status <- solve(lp)
  optimum <- get.objective(lp)
  result <- matrix(get.variables(lp)[2:x$num_lpvar - 1], ncol=x$num_form, byrow=FALSE)
  list(status=status, optimum=optimum, result=result)
}

# ata_solve_glpk <- function(x, ...) {
#   if(class(x) != "ata") stop("not an 'ata' object")
#   lp <- initProbGLPK()
#   addRowsGLPK(lp, nrow(x$mat))
#   addColsGLPK(lp, ncol(x$mat))
#   # (max): optimization direction
#   setObjDirGLPK(lp, ifelse(x$max, GLP_MAX, GLP_MIN))
#   # (types): x's = binary, y = continuous
#   types <- sapply(x$types, function(x) switch(x, "C"=GLP_CV, "I"=GLP_IV, "B"=GLP_BV))
#   for(i in seq_along(types)) setColKindGLPK(lp, i, types[i])
#   # (obj): omit coef=0
#   for(i in seq_along(x$obj))
#     if(x$obj[i] != 0) setObjCoefGLPK(lp, i, x$obj[i])
#   # (dir & rhs): row bounds
#   dir <- sapply(x$dir, function(x) switch(x, "<="=GLP_UP, ">="=GLP_LO, "=="=GLP_FX))
#   for(i in 1:nrow(x$mat)) setRowBndGLPK(lp, i, dir[i], lb=x$rhs[i], ub=x$rhs[i])
#   # (bounds): column bounds
#   bounds.lb <- sapply(x$types, function(x) switch(x, "C"=0, "I"=0, "B"=0))
#   bounds.ub <- sapply(x$types, function(x) switch(x, "C"=Inf, "I"=Inf, "B"=1))
#   with(x$bounds, for(i in seq_along(ind)) {
#     if(!is.na(lb[i])) bounds.lb[ind[i]] <- lb[i]
#     if(!is.na(ub[i])) bounds.ub[ind[i]] <- ub[i]
#   })
#   # (mat)
#   ind <- x$mat != 0
#   ia <- rep(1:nrow(x$mat), ncol(x$mat))[ind]
#   ja <- rep(1:ncol(x$mat), each=nrow(x$mat))[ind]
#   ar <- x$mat[ind]
#   loadMatrixGLPK(lp, length(ar), ia, ja, ar)
#   # mip control parameters:
#   setMIPParmGLPK(PRESOLVE, GLP_ON)
#   setMIPParmGLPK(MIP_GAP, 0.01)
#   setMIPParmGLPK(TM_LIM, 1000 * 10)
#   opts <- list(...)
#   for(i in seq_along(opts)) setMIPParmGLPK(get(names(opts)[i]), opts[[i]])
#   # set bound for y: positive = (lb=0); negative = (ub = 0)
#   setColBndGLPK(lp, x$nlp, ifelse(x$negative, GLP_UP, GLP_LO), 0, 0)
#   # solve
#   status <- solveMIPGLPK(lp)
#   optimum <- mipObjValGLPK(lp)
#   result <- matrix(mipColsValGLPK(lp)[2:x$nlp - 1], ncol=x$nforms, byrow=FALSE)
#   list(status=status, optimum=optimum, result=result)
# }
