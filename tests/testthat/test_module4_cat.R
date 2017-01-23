library(testthat)
context("module4_cat.R")

test_that("SE/MI/CI stopping rule", {
  pool <- irt_model("3pl")$gendata(1,200)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
  
  opts <- list(min=10, max=30, stop.se=.3)
  x <- cat_sim(0.1, pool, opts)
  expect_true(x$admin$se[x$len] <= .3)
  
  opts <- list(min=10, max=30, stop.mi=.8)
  x <- cat_sim(0.1, pool, opts)
  items <- x$pool[-as.numeric(rownames(x$admin)), ]
  info <- irt_stats(irt_model("3pl", theta=x$est, items=items), "info")[1, ]
  expect_true(max(info) <= 0.8)
  
  opts <- list(min=10, max=30, stop.cut=-0.5)
  x <- cat_sim(0.1, pool, opts)
  lb <- x$est - 1.96 * x$admin$se[x$len]
  ub <- x$est + 1.96 * x$admin$se[x$len]
  expect_true(!xor(lb <= -0.5, ub <= -0.5))
})

test_that("c-cat selection rule", {
  pool <- irt_model("3pl")$gendata(1,200)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
  
  opts <- list(min=30, max=60, stop.cut=0, ccat.target=c(.50, .25, .25), ccat.random=5)
  x <- cat_sim(0.1, pool, opts, cat.select=cat_select_ccat)
  f <- freq(x$admin$content, 1:3)$perc / 100
  expect_true(all(f == c(.5, .25, .25)))
})

test_that("shadow test selection rule", {
  pool <- irt_model("3pl")$gendata(1,200)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
  
  con <- data.frame(name="content", level=c(1, 2, 3), min=c(10, 10, 10), max=c(10, 10,10))
  opts <- list(min=30, max=30, stop.cut=0, shadow.constraints=con)
  x <- cat_sim(0.1, pool, opts, cat.select=cat_select_shadow)
  f <- freq(x$admin$content, 1:3)$freq
  expect_true(all(f == c(10, 10, 10)))
})