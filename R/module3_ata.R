#' Automated Test Assembly (ATA)
#' @description \code{ata} creates an \code{ata} object
#' @param pool item pool
#' @param nforms the number of forms
#' @param len the test length
#' @param maxselect the maximum selection of each item
#' @details
#' The \code{ata} object stores LP information in \code{obj}, \code{mat}, \code{dir}, 
#' \code{rhs}, \code{types}, \code{bounds}, \code{max}. When the \code{ata_solve} is
#' called, this information will be converted to real LP object for the selected solver.
#' When the LP is solved successfully, two results will be added to the \code{ata} object:
#' (i) \code{result} is a matrix of binary selection results (items in rows and forms in columns)
#' and (ii) \code{items} is a list or data frame of selected items.\cr
#' @examples
#' \dontrun{
#' library(dplyr)
#' # generate a 100-item pool
#' items <- irt_model("3pl")$gendata(1, 100)$items
#' items$id <- 1:nrow(items)
#' items$content <- sample(1:3, nrow(items), replace=TRUE)
#' items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
#' 
#' ## ex. 1: 6 forms, 10 items, maximize b parmaters
#' ## solved by GLPK and LpSolve respectively
#' x <- ata(items, 6, len=10, maxselect=1)
#' x <- ata_obj_relative(x, "b", "max")
#' glpk <- ata_solve(x, solver="glpk")
#' glpk$optimum
#' sapply(glpk$items, function(x)
#'   c(mean=mean(x$b), sd=sd(x$b), min=min(x$b), max=max(x$b)))
#' lpsolve <- ata_solve(x, solver="lpsolve")
#' lpsolve$optimum
#' sapply(lpsolve$items, function(x)
#'   c(mean=mean(x$b), sd=sd(x$b), min=min(x$b), max=max(x$b)))
#' 
#' ## ex. 2: 4 forms, 10 items, minimize b parmaeters
#' x <- ata(items, 3, len=10, maxselect=1)
#' x <- ata_obj_relative(x, "b", "min", negative=TRUE)
#' glpk <- ata_solve(x, solver="glpk", as.list=FALSE, timeout=5)
#' group_by(glpk$items, form) %>% 
#'   summarise(mean=mean(b), sd=sd(b), min=min(b), max=max(b))
#' lpsolve <- ata_solve(x, solver="lpsolve", as.list=FALSE, timeout=5)
#' group_by(lpsolve$items, form) %>% 
#'   summarise(mean=mean(b), sd=sd(b), min=min(b), max=max(b))
#'   
#' ## ex. 3: 3 forms, 10 items, maximize information at -1 and 1
#' ## content distribution: 3, 3, 4
#' ## response time: avg. 55-65 seconds
#' x <- ata(items, 3, len=10, maxselect=1)
#' x <- ata_obj_relative(x, c(-1, 1), "max")
#' x <- ata_constraint(x, "content", min=3, max=3, level=1)
#' x <- ata_constraint(x, "content", min=3, max=3, level=2)
#' x <- ata_constraint(x, "content", min=4, max=4, level=3)
#' x <- ata_constraint(x, "time", min=55*10, max=65*10)
#' lpsolve <- ata_solve(x, solver="lpsolve")
#' lpsolve$optimum
#' plot(lpsolve)
#' sapply(lpsolve$items, function(x) 
#'   c(freq(x$content, 1:3)$freq, mean(x$time)))
#'   
#' ## ex. 4: 2 forms, 10 items, mean(b) = 0, sd(b) = 1.0, content = (3, 3, 4)
#' x <- ata(items, 2, len=10, maxselect=1) %>%
#'      ata_obj_absolute(items$b, 0 * 10) %>%
#'      ata_obj_absolute((items$b - 0)^2, 1 * 10) %>%
#'      ata_constraint("content", min=3, max=3, level=1) %>%
#'      ata_constraint("content", min=3, max=3, level=2) %>%
#'      ata_constraint("content", min=4, max=4, level=3)
#' lpsolve <- ata_solve(x, "lpsolve", verbose="normal", timeout=5)
#' sapply(lpsolve$items, function(x) c(mean=mean(x$b), sd=sd(x$b)))
#' 
#' # ex. 5: 2 forms, 10 items, flat TIF over [-1, 1]
#' x <- ata(items, 2, len=10, maxselect=1) %>%
#'      ata_obj_relative(seq(-1, 1, .5), "max", flatten=0.05)
#' x <- ata_solve(x, "lpsolve")
#' plot(x)
#' }
#' @export
ata <- function(pool, nforms=1, len=NULL, maxselect=NULL){
  # vaalidate item pool
  pool <- as.data.frame(pool, stringsAsFactors=FALSE)
  if(!all(c("a","b","c") %in% colnames(pool))) stop("a, b, or c parameters are not found in the pool.")
  
  # constants
  nitems <- nrow(pool)
  nlp <- nitems * nforms + 1
  
  # LP: x's (binary) + y (continuous)
  obj <- matrix(c(rep(0, nlp-1), 1), nrow=1)
  colnames(obj) <- c(paste("f", rep(1:nforms, each=nitems), "x", rep(1:nitems, nforms), sep=""), "y")
  types <- c(rep("B", nlp-1), "C")
  bounds <- list(ind=c(), lb=c(), ub=c())
  max <- TRUE
  mat <- matrix(nrow=0, ncol=nlp, dimnames=list(NULL, colnames(obj)))
  dir <- rhs <- NULL
  negative <- FALSE
  x <- list(nitems=nitems, nforms=nforms, nlp=nlp, pool=pool, obj=obj, mat=mat, dir=dir, rhs=rhs, 
            types=types, bounds=bounds, max=max, negative=negative)
  class(x) <- "ata"
  
  # add constraint: test length
  if(!is.null(len) && length(len) == 1) x <- ata_constraint(x, 1, min=len, max=len)
  if(!is.null(len) && length(len) == 2) x <- ata_constraint(x, 1, min=len[1], max=len[2])
  if(!is.null(len) && length(len) > 2) stop("invalid length.")
  
  # add constraint: maxselect
  if(!is.null(maxselect)) x <- ata_item_maxselect(x, maxselect)
  
  x
}


#' @rdname ata
#' @param x the ata object
#' @param ... further arguments
#' @export
print.ata <- function(x, ...){
  cat("Assemble", x$nforms, "forms from", x$nitems, "items.\n")
  if(is.null(x$items)) {
    cat("The LP hasn't been solved yet.\n")
  } else {
    cat("The LP has been solve:\n")
    items <- x$items
    if(!is.data.frame(items)) items <- Reduce(function(x, y) rbind(x, y), items, NULL)
    if(nrow(items) <= 10) {
      print(items)
    } else {
      print(items[1:5,])
      cat("...\n")
      print(items[-4:0 + nrow(items),])
    }
    cat("See more reulsts in 'result' or 'items'.")
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
  nt <- length(opts$theta)
  
  # Data
  items <- x$items
  if(is.data.frame(items)) items <- split(items, f=items$form)
  data <- lapply(items, function(item) {
    info <- irt_stats(model_3pl(theta=opts$theta, items=item), "info", summary="people", fun=sum)
    cbind(t=opts$theta, info=info, form=item$form[1])
  })
  data <- Reduce(function(x, y) rbind(x, y), data, NULL)
  data <- as.data.frame(data)
  
  # Drawing
  data$form <- factor(paste("Form", data$form))
  ggplot(data, aes_string(x="t", y="info", color="form")) + 
    geom_line() + xlab(expression(theta)) + ylab("Information") + 
    theme_bw() + theme(legend.key=element_blank()) +
    guides(color=guide_legend("Forms"))
}


#' @rdname ata
#' @description \code{ata_obj_relative} adds relative (maximize/minimize) objectives to LP
#' @param coef the coefficients of the objective function or the constraint
#' @param mode the optimzation mode: 'max' for maximization and 'min' for minimization
#' @param negative \code{TRUE} when the expected value of the objective function is negative
#' @param flatten the flatten parameter to make the objective function flat
#' @param forms the indcies of forms where objectives are added. \code{NULL} for all forms
#' @param collapse \code{TRUE} to collapse all forms into one objective function
#' @details 
#' To maximize the LP, it is to maximize y while subject to sum(x) - y >= 0 and <= F (flatten parameter).
#' To minimize teh LP, it is to minimize y while subject to sum(x) - y <= 0 and >= F.
#' By default, y is non-negative. When \code{negative=TRUE}, y is set to be negative. \cr 
#' When \code{coef} is a pool-size or form-size numeric vector, coefficients are used directly.
#' When \code{coef} is a variable name, variable values are used as coefficients.
#' When \code{coef} is a numeric vector unequal to pool size, information at those points are used as coefficients.\cr
#' @export
ata_obj_relative <- function(x, coef, mode=c('max', 'min'), negative=FALSE, flatten=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  forms <- ata_form_index(x, forms, collapse)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  x$negative <- negative
  x$max <- ifelse(match.arg(mode) == "max", TRUE, FALSE)
  
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef), ncol=x$nlp)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef))
  for(i in 1:nrow(forms)) {
    f <- forms[i,]
    ind <- outer(1:x$nitems, (f - 1) * x$nitems, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)) {
      row <- j + (i-1) * nrow(coef)
      mat[row, ind] <- rep(coef[j, ], length(f))
      mat[row, x$nlp] <- -1
      dir[row] <- ifelse(x$max, ">=", "<=")
      rhs[row] <- 0
    }
  }
  
  if(!is.null(flatten)) {
    mat <- rbind(mat, mat)
    dir <- c(dir, rep(ifelse(x$max, "<=", ">="), nrow(forms) * nrow(coef)))
    rhs <- c(rhs, rep(flatten, nrow(forms) * nrow(coef)))
  }
  
  x <- ata_append(x, mat, dir, rhs)
  x
}


#' @rdname ata
#' @description \code{ata_obj_absolute} adds absolute objectives to LP
#' @param target the target of the objective function
#' @details 
#' \code{ata_obj_absolute} is to minimize y while subject to sum(x) + y >= T and sum(x) - y <= T. \cr
#' @export
ata_obj_absolute <- function(x, coef, target, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  forms <- ata_form_index(x, forms, collapse)
  coef <- ata_obj_coef(x, coef, compensate=FALSE)
  if(length(target) == 1) target <- rep(target, nrow(coef))
  if(length(target) != nrow(coef)) stop("invalid target length.")
  x$max <- FALSE
  
  mat <- matrix(0, nrow=nrow(forms) * nrow(coef) * 2, ncol=x$nlp)
  dir <- rhs <- rep(NA, nrow(forms) * nrow(coef) * 2)
  for(i in 1:nrow(forms)){
    f <- forms[i,]
    ind <- outer(1:x$nitems, (f - 1) * x$nitems, "+")
    ind <- as.vector(ind)
    for(j in 1:nrow(coef)){
      row <- (j - 1) * 2 + (i - 1) * nrow(coef) * 2
      mat[row + 1, ind] <- rep(coef[j, ], length(f))
      mat[row + 1, x$nlp] <- 1
      mat[row + 2, ind] <- rep(coef[j, ], length(f))
      mat[row + 2, x$nlp] <- -1
      dir[row + 1:2] <- c(">=", "<=")
      rhs[row + 1:2] <- target[j]
    }
  }
  
  x <- ata_append(x, mat, dir, rhs)
  x    
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
#' Set \code{coef} to a constant or a vector to add a constraint directly.\cr
#' @export
ata_constraint <- function(x, coef, min=NA, max=NA, level=NULL, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.na(min) && is.na(max)) return(x)
  if(!is.na(min) && !is.na(max) && min > max) stop("min is greater than max.")
  forms <- ata_form_index(x, forms, collapse)
  
  # coef is a variable name
  if(length(coef) == 1 && is.character(coef) && coef %in% colnames(x$pool)) {
    coef <- x$pool[, coef]
    if(!is.null(level) && !is.na(level)) coef <- coef == level
  }
  # coef is a numeric constant or vector
  if(length(coef) == 1) {
    coef <- rep(coef, x$nitem * ncol(forms))
  } else if(length(coef) == x$nitems) {
    coef <- rep(coef, ncol(forms))
  }
  if(length(coef) != x$nitems * ncol(forms)) 
    stop("coef is not a cosntant, pool-size vector or form-size vector")
  
  n <- ifelse(!is.na(min) && !is.na(max) && min != max, nrow(forms) * 2, nrow(forms))
  mat <- matrix(0, nrow=n, ncol=x$nlp)
  dir <- rhs <- rep(NA, n)
  for(i in 1:nrow(forms)) {
    f <- forms[i,]
    ind <- outer(1:x$nitems, (f - 1) * x$nitems, "+")
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
  
  x <- ata_append(x, mat, dir, rhs)
  x
}


#' @rdname ata
#' @description \code{ata_item_maxselect} sets the maximum selection for items
#' @param items a vector of item indices
#' @export
ata_item_maxselect <- function(x, maxselect, items=NULL){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(is.null(items)) items <- 1:x$nitem 
  if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  nitems <- length(items)
  
  mat <- matrix(0, nrow=nitems, ncol=x$nlp)
  for(i in items) {
    ind <- i + (1:x$nforms - 1) * x$nitems
    mat[i, ind] <- 1
  }
  dir <- rep("<=", nitems)
  rhs <- rep(maxselect, nitems)
  x <- ata_append(x, mat, dir, rhs)
  x
}


#' @rdname ata
#' @description \code{ata_item_enemy} adds enemy item relationship to LP
#' @export
ata_item_enemy <- function(x, items){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$nitem)) stop("invalid item index")
  
  mat <- matrix(0, nrow=x$nforms, ncol=x$nlp)
  for(i in 1:x$nforms){
    ind <- items + (i - 1) * x$nitems
    mat[i, ind] <- 1
  }
  dir <- rep("<=", x$nforms)
  rhs <- rep(1, x$nforms)
  x <- ata_append(x, mat, dir, rhs)
  x
}


#' @rdname ata
#' @description \code{ata_item_fixedvalue} sets a fixed value range for items
#' @export
ata_item_fixedvalue <- function(x, items, min=NA, max=NA, forms=NULL, collapse=FALSE){
  if(class(x) != "ata") stop("not an 'ata' object")
  if(any(!items %in% 1:x$nitem)) stop("invalid items input.")
  forms <- ata_form_index(x, forms, collapse)
  n <- length(items)
  if(length(min) == 1) min <- rep(min, n)
  if(length(max) == 1) max <- rep(max, n)
  if(length(min) != n || length(max) != n) stop("invalid min or max length.")
  ind <- as.vector(outer(items, (forms - 1) * x$nitems, "+"))
  x$bounds$ind <- c(x$bounds$ind, ind)
  x$bounds$lb <- c(x$bounds$lb, rep(min, length(forms)))
  x$bounds$ub <- c(x$bounds$ub, rep(max, length(forms)))
  x
}


#' @rdname ata
#' @description \code{ata_solve} solves the LP
#' @param solver \code{"glpk"} for GLPK and \code{"lpsolve"} for lpSolve
#' @param as.list \code{TRUE} to return resutls in a list; otherwise, data frame
#' @param timeout the time limit in seconds
#' @param mip_gap the mip gap paraemter
#' @param verbose the message parameter
#' @details 
#' In \code{ata_solve}, additional control parameters will be passed into solvers.
#' When passing control parameters to the GLPK solver, use the correct parameter name
#' (see \code{?glpkAPI::glpkConstants}).
#' @export
ata_solve <- function(x, solver=c("lpsolve", "glpk"), as.list=TRUE, timeout=10, mip_gap=0.1, verbose=c("none", "normal", "full"), ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  solver <- match.arg(solver)
  verbose <- match.arg(verbose)
  
  if(solver == "glpk") {
    timeout <- timeout * 1000
    verbose <- switch(verbose, "full"=GLP_MSG_ALL, "normal"=GLP_MSG_ON, "none"=GLP_MSG_OFF)
    rs <- ata_solve_glpk(x, TM_LIM=timeout, MIP_GAP=mip_gap, MSG_LEV=verbose)
  } else if(solver == "lpsolve") {
    verbose <- switch(verbose, "full"="full", "normal"="normal", "none"="neutral")
    mipgap <- rep(mip_gap, 2)
    rs <- ata_solve_lpsolve(x, timeout=timeout, mip.gap=mip_gap, verbose=verbose)
    if(!rs$status %in% c(0, 1)) rs$result <- array(0L, dim(rs$result))
  }
  
  x$status <- rs$status
  x$optimum <- rs$optimum
  if(all(rs$result == 0)) {
    cat("No solution for LP.\n")
  } else {
    x$result <- rs$result
    items <- list()
    for(i in 1:x$nforms) items[[i]] <- cbind(x$pool[x$result[,i] == 1, ], form=i)
    if(!as.list) items <- Reduce(function(x, y) rbind(x, y), items, NULL)
    x$items <- items
  }
  x
}


#' @rdname ata
#' @description \code{ata_solve_glpk} solves the LP using  GLPK
#' @import glpkAPI
#' @export
ata_solve_glpk <- function(x, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  lp <- initProbGLPK()
  addRowsGLPK(lp, nrow(x$mat))
  addColsGLPK(lp, ncol(x$mat))
  # (max): optimization direction
  setObjDirGLPK(lp, ifelse(x$max, GLP_MAX, GLP_MIN))
  # (types): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "C"=GLP_CV, "I"=GLP_IV, "B"=GLP_BV))
  for(i in seq_along(types)) setColKindGLPK(lp, i, types[i])
  # (obj): omit coef=0
  for(i in seq_along(x$obj))
    if(x$obj[i] != 0) setObjCoefGLPK(lp, i, x$obj[i])
  # (dir & rhs): row bounds
  dir <- sapply(x$dir, function(x) switch(x, "<="=GLP_UP, ">="=GLP_LO, "=="=GLP_FX))
  for(i in 1:nrow(x$mat)) setRowBndGLPK(lp, i, dir[i], lb=x$rhs[i], ub=x$rhs[i])
  # (bounds): column bounds
  bounds.lb <- sapply(x$types, function(x) switch(x, "C"=0, "I"=0, "B"=0))
  bounds.ub <- sapply(x$types, function(x) switch(x, "C"=Inf, "I"=Inf, "B"=1))
  with(x$bounds, for(i in seq_along(ind)) {
    if(!is.na(lb[i])) bounds.lb[ind[i]] <- lb[i]
    if(!is.na(ub[i])) bounds.ub[ind[i]] <- ub[i]
  })
  # (mat)
  ind <- x$mat != 0
  ia <- rep(1:nrow(x$mat), ncol(x$mat))[ind]
  ja <- rep(1:ncol(x$mat), each=nrow(x$mat))[ind]
  ar <- x$mat[ind]
  loadMatrixGLPK(lp, length(ar), ia, ja, ar)
  # mip control parameters:
  setMIPParmGLPK(PRESOLVE, GLP_ON)
  setMIPParmGLPK(MIP_GAP, 0.01)
  setMIPParmGLPK(TM_LIM, 1000 * 10)
  opts <- list(...)
  for(i in seq_along(opts)) setMIPParmGLPK(get(names(opts)[i]), opts[[i]])
  # set bound for y: positive = (lb=0); negative = (ub = 0)
  setColBndGLPK(lp, x$nlp, ifelse(x$negative, GLP_UP, GLP_LO), 0, 0)
  # solve
  status <- solveMIPGLPK(lp)
  optimum <- mipObjValGLPK(lp)
  result <- matrix(mipColsValGLPK(lp)[2:x$nlp - 1], ncol=x$nforms, byrow=FALSE)
  list(status=status, optimum=optimum, result=result)
}


#' @rdname ata
#' @description \code{ata_solve_lpsolve} solves the LP using LpSolve
#' @import lpSolveAPI
#' @export
ata_solve_lpsolve <- function(x, ...) {
  if(class(x) != "ata") stop("not an 'ata' object")
  lp <- make.lp(0, x$nlp)
  # (max): direction
  lp.control(lp, sense=ifelse(x$max, "max", "min"))
  # set bound for y: positive = (lb=0); negative = (ub = 0)
  if(x$negative) set.bounds(lp, lower=-Inf, upper=0, x$nlp)
  # (obj): objective function
  for(i in seq_along(x$obj))
    if(x$obj[i] != 0) set.objfn(lp, x$obj[i], i) 
  # (type): x's = binary, y = continuous
  types <- sapply(x$types, function(x) switch(x, "B"="binary", "I"="integer", "C"="real"))
  for(i in seq_along(types)) set.type(lp, i, types[i])
  # (bounds): column bounds
  with(x$bounds, for(i in seq_along(ind)) {
    set.bounds(lp, if(!is.na(lb[i])) lower=lb[i], if(!is.na(ub[i])) upper=ub[i], columns=ind[i])
  })
  # (mat): constraints
  dir <- gsub("==", "=", x$dir)
  for(i in 1:nrow(x$mat)) add.constraint(lp, x$mat[i,], dir[i], x$rhs[i])
  # solve
  lp.control(lp, mip.gap=c(.01, .10), epsint=.10, presolve="lindep", timeout=10)
  lp.control(lp, ...)
  status <- solve(lp)
  optimum <- get.objective(lp)
  result <- matrix(get.variables(lp)[2:x$nlp - 1], ncol=x$nforms, byrow=FALSE)
  list(status=status, optimum=optimum, result=result)
}

