#' Automated Test Assembly (ATA)
#' @name ata
#' @examples
#' \dontrun{
#' ## generate a pool of 100 items
#' n_items <- 100
#' pool <- with(model_3pl_gendata(1, nitems), data.frame(id=1:n_items, a=a, b=b, c=c))
#' pool$content <- sample(1:3, n_items, replace=TRUE)
#' pool$time <- round(rlnorm(n_items, log(60), .2))
#' pool$group <- sort(sample(1:round(n_items/3), n_items, replace=TRUE))
#' 
#' ## ex. 1: four 10-item forms, maximize b parameter
#' x <- ata(pool, 4, len=10, max_use=1)
#' x <- ata_obj_relative(x, "b", "max")
#' x <- ata_solve(x, timeout=5)
#' data.frame(form=1:4, b=sapply(x$items, function(x) mean(x$b)))
#' 
#' ## ex. 2: four 10-item forms, minimize b parameter
#' x <- ata(pool, 4, len=10, max_use=1)
#' x <- ata_obj_relative(x, "b", "min", negative=TRUE)
#' x <- ata_solve(x, as.list=FALSE, timeout=5)
#' with(x$items, aggregate(b, by=list(form=form), mean))
#'
#' ## ex. 3: two 10-item forms, mean(b)=0, sd(b)=1
#' ## content = (3, 3, 4), avg. time = 58--62 seconds
#' constr <- data.frame(name='content',level=1:3, min=c(3,3,4), max=c(3,3,4), stringsAsFactors=F)
#' constr <- rbind(constr, c('time', NA, 58*10, 62*10))
#' x <- ata(pool, 2, len=10, max_use=1)
#' x <- ata_obj_absolute(x, pool$b, 0*10)
#' x <- ata_obj_absolute(x, (pool$b-0)^2, 1*10)
#' for(i in 1:nrow(constr))
#'   x <- with(constr, ata_constraint(x, name[i], min[i], max[i], level=level[i]))
#' x <- ata_solve(x, timeout=5)
#' sapply(x$items, function(x) c(mean=mean(x$b), sd=sd(x$b)))
#' 
#' ## ex. 4: two 10-item forms, max TIF over (-1, 1), consider item sets
#' x <- ata(pool, 2, len=10, max_use=1, group="group")
#' x <- ata_obj_relative(x, seq(-1, 1, .5), 'max')
#' x <- ata_solve(x, timeout=5)
#' plot(x)
#' }
NULL

#' @rdname ata
#' @description \code{ata} initiates an ATA model
#' @param pool item pool, a data.frame
#' @param num_form number of forms to be assembled
#' @param len test length of each form
#' @param max_use maximum use of each item
#' @param ... options, e.g. group, common_items, overlap_items
#' @details
#' The ATA model stores the definition of a MIP model. \code{ata_solve} 
#' converts the model definition to a real MIP object and attempts to solve it.
#' @export
ata <- function(pool, num_form=1, len=NULL, max_use=NULL, ...){
  if(!is.data.frame(pool)) pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!'b' %in% colnames(pool)) warning('b parameters are not found in the pool')
  if(!'a' %in% colnames(pool)) warning('a parameters are not found in the pool')
  if(!'c' %in% colnames(pool)) warning('c parameters are not found in the pool')
  
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
  
  # the number of items and 
  num_lpvar <- num_item * num_form + 2
  # LP: x's (binary) + y1 (continuous) + y2 (continuous)
  obj <- c(rep(0, num_lpvar - 2), 1, 1)
  names(obj) <- c(paste("f", rep(1:num_form, each=num_item), "v", rep(1:num_item, num_form), sep=""), "y1", "y2")
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
#' @param x an ATA object
#' @export
print.ata <- function(x, ...){
  cat("Assemble", x$num_form, "forms from", x$num_item, "items/sets.\n")
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
  
  data$form <- factor(paste("Form", data$form))
  ggplot(data, aes_string(x="t", y="info", color="form")) + 
    geom_line() + xlab(expression(theta)) + ylab("Information") + 
    theme_bw() + theme(legend.key=element_blank()) +
    guides(color=guide_legend("Forms"))
}


#' @rdname ata
#' @description \code{ata_obj_relative} adds a relative objective to the model
#' @param coef coefficients of the objective function
#' @param mode optimization mode: 'max' for maximization and 'min' for minimization
#' @param tol the tolerance paraemter
#' @param negative \code{TRUE} when the objective function is expected to be negative
#' @param forms forms where objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse into one objective function
#' @param internal_index \code{TRUE} to use internal form indices
#' @details 
#' \code{ata_obj_relative}: 
#' when mode='max', maximize (y-tol), subject to y <= sum(x) <= y+tol;
#' when mode='min', minimize (y+tol), subject to y-tol <= sum(x) <= y.
#' When \code{negative} is \code{TRUE}, y < 0, tol > 0.
#' \code{coef} can be a numeric vector that has the same length with the pool or forms, 
#' or a variable name in the pool, or a numeric vector of theta points.
#' When \code{tol} is \code{NULL}, it is optimized; when \code{FALSE}, ignored; 
#' when a number, fixed; when a range, constrained with lower and upper bounds.
#' @export
ata_obj_relative <- function(x, coef, mode=c('max', 'min'), tol=NULL, negative=FALSE, forms=NULL, collapse=FALSE, internal_index=FALSE, ...){
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  forms <- ata_form_index(x, forms, collapse, internal_index)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  x$negative <- negative

  # optimization direction
  x$max <- switch(match.arg(mode), "max"=TRUE, "min"=FALSE)
  if(x$max){
    x$obj[(x$num_lpvar-1):x$num_lpvar] <- c(1, -1)
  } else {
    x$obj[(x$num_lpvar-1):x$num_lpvar] <- c(1,  1)
  }
  
  # tolerance parameter
  if(!is.null(tol))
    if(length(tol) == 2) {
      x$bounds$idx <- c(x$bounds$idx, x$num_lpvar)
      x$bounds$lb <- c(x$bounds$lb, tol[1])
      x$bounds$ub <- c(x$bounds$ub, tol[2])
    } else if(is.numeric(tol)){
      x$bounds$idx <- c(x$bounds$idx, x$num_lpvar)
      x$bounds$lb <- c(x$bounds$lb, tol)
      x$bounds$ub <- c(x$bounds$ub, tol)
    } else if(!tol) {
      x$obj[x$num_lpvar] <- 0
    }
  
  # objective for each form
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
  
  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_obj_absolute} adds an absolute objective to the model
#' @param target the target values of the objective function
#' @param equal_tol \code{TRUE} to force upward and downward tolerance to be equal
#' @param tol_up the range of upward tolerance
#' @param tol_down the range of downward tolerance
#' @details 
#' \code{ata_obj_absolute} minimizes y0+y1 subject to t-y0 <= sum(x) <= t+y1.
#' @export
ata_obj_absolute <- function(x, coef, target, equal_tol=FALSE, tol_up=NULL, tol_down=NULL, forms=NULL, collapse=FALSE, internal_index=FALSE, ...){
  if(class(x) != "ata") stop("not an 'ata' object")
  opts <- list(...)
  
  forms <- ata_form_index(x, forms, collapse, internal_index)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  if(length(target) == 1) target <- rep(target, nrow(coef))
  if(length(target) != nrow(coef)) stop("invalid target length.")
  
  # optimization direction
  x$max <- FALSE
  x$obj[x$num_lpvar+(-1:0)] <- 1
  
  # objective for each form
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
  
  # constrains on tolerance parameters
  if(equal_tol){
    mat <- rbind(mat, c(rep(0, x$num_lpvar - 2), 1, -1))
    dir <- c(dir, '=')
    rhs <- c(rhs, 0)
  }
  if(!is.null(tol_down)){
    if(length(tol_down) != 2) stop('invalid tol_down input. expect a vector of 2 elements.')
    x$bounds$idx <- c(x$bounds$idx, x$num_lpvar-1)
    x$bounds$lb <- c(x$bounds$lb, tol_down[1])
    x$bounds$ub <- c(x$bounds$ub, tol_down[2]) 
  }
  if(!is.null(tol_up)){
    if(length(tol_up) != 2) stop('invalid tol_up input. expect a vector of 2 elements.')
    x$bounds$idx <- c(x$bounds$idx, x$num_lpvar)
    x$bounds$lb <- c(x$bounds$lb, tol_up[1])
    x$bounds$ub <- c(x$bounds$ub, tol_up[2]) 
  }

  ata_append_constraints(x, mat, dir, rhs)
}


#' @rdname ata
#' @description \code{ata_constraint} adds a constraint to the model
#' @param level the level of a categorical variable to be constrained
#' @param min the lower bound of the constraint
#' @param max the upper bound of the constraint
#' @details 
#' When \code{level} is \code{NA}, it is assumed that the constraint is on
#' a quantitative item property; otherwise, a categorical item property. 
#' \code{coef} can be a variable name, a constant, or a numeric vector that has
#' the same size as the pool.
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
#' @description \code{ata_item_use} limits the minimum and maximum usage for items
#' @param items a vector of item indices, \code{NULL} for all items
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
#' @description \code{ata_item_enemy} adds an enemy-item constraint to the model
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
#' @description \code{ata_item_fixedvalue} forces an item to be selected or not selected
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
#' @description \code{ata_solve} solves the MIP model
#' @param solver use 'lpsolve' for lp_solve 5.5 or 'glpk' for GLPK
#' @param as.list \code{TRUE} to return results in a list; otherwise, a data frame
#' @param details \code{TRUE} to print detailed information
#' @param time_limit the time limit in seconds passed along to solvers
#' @param message \code{TRUE} to print messages from solvers
#' @details 
#' \code{ata_solve} takes control options in \code{...}. 
#' For lpsolve, see \code{lpSolveAPI::lp.control.options}.
#' For glpk, see \code{glpkAPI::glpkConstants}\cr
#' Once the model is solved, additional data are added to the model.
#' \code{status} shows the status of the solution, \code{optimum} 
#' the optimal value of the objective fucntion found in the solution, 
#' \code{obj_vars} the values of two critical variables in the objective
#' function, \code{result} the assembly results in a binary matrix, and 
#' \code{items} the assembled items
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


