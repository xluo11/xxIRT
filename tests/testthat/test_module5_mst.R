library(testthat)
library(dplyr)
library(magrittr)
context("module5_mst.R")

test_that("add and remove routes", {
  pool <- irt_model("3pl")$gendata(1,300)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
  
  x <- mst(pool, design=c(1, 2, 3), npanel=2, method='topdown')
  expect_true(x$nroute, 6)
  expect_true(nrow(x$route), 6)
  
  x <- mst_route(x, route=c(1, 2, 6), "-")
  expect_true(x$nroute, 5)
  expect_true(nrow(x$route), 5)
  expect_error(mst_route(x, route=c(1, 2, 6), "-"))
  
  x <- mst_route(x, route=c(1, 2, 6), "+")
  expect_true(x$nroute, 6)
  expect_true(nrow(x$route), 6)
  expect_error(mst_route(x, route=c(1, 2, 6), "+"))
})

test_that("add constraints", {
  pool <- irt_model("3pl")$gendata(1,300)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))

  x <- mst(pool, design=c(1, 2, 3), npanel=1, method='topdown')
  x <- mst_objective(x, theta= 0, indices=1:5)
  x <- mst_constraint(x, coef=1, min=10, max=10)
  x <- mst_constraint(x, coef="content", min=4, max=4, level=1)
  x <- mst_constraint(x, coef="content", min=3, max=3, level=2)
  x <- mst_constraint(x, coef="content", min=3, max=3, level=3)
  x <- mst_constraint(x, coef="time", min=58*10, max=62*10)
  x <- mst_assemble(x, timeout=60)
  y <- mst_get_items(x, 1)
  expect_true(all(freq(y$content, 1:3)$freq == c(4, 3, 3)))
  expect_true(mean(y$time) >= 58 & mean(y$time) <= 62)
})

test_that("set stage length", {
  pool <- irt_model("3pl")$gendata(1,300)$items
  pool$content <- sample(1:3, nrow(pool), replace=TRUE)
  pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
  
  x <- mst(pool, design=c(1, 2, 3), npanel=1, method='topdown')
  x <- mst_objective(x, theta= 0, indices=1:5)
  x <- mst_constraint(x, coef=1, min=10, max=10)
  x <- mst_stage_length(x, stages=1, min=3, max=3)
  x <- mst_stage_length(x, stages=2, min=3, max=10)
  x <- mst_stage_length(x, stages=3, min=1, max=3)
  x <- mst_assemble(x, timeout=60)
  y <- mst_get_items(x, 1)
  f <- y %>% group_by(stage, module) %>% summarise(n=n())
  expect_true(all(filter(f, stage==1)$n == 3))
  expect_true(all(filter(f, stage==2)$n >= 3))
  expect_true(all(filter(f, stage==3)$n <= 3))
})

