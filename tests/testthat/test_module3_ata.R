library(testthat)
library(magrittr)
library(dplyr)
context("module3_ata.R")
set.seed(880813)


# a helper function to create item pool
pool <- function(n){
  items <- irt_model("3pl")$gendata(1, n)$items
  items$id <- 1:nrow(items)
  items$content <- sample(1:3, nrow(items), replace=TRUE)
  items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
  items
}


test_that("assmeble multiple forms with single relative objective", {
  items <- pool(100)
  # 4 forms, 10 items, maximize difficulty
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) expect_equal(nrow(x), 10))
  b.avg <- mean(items$b[order(items$b, decreasing=TRUE)[1:50]])
  sapply(x$items, function(x) expect_gt(mean(x$b), b.avg))
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) expect_equal(nrow(x), 10))
  b.avg <- mean(items$b[order(items$b, decreasing=TRUE)[1:50]])
  sapply(x$items, function(x) expect_gt(mean(x$b), b.avg))
  
  # 4 forms, 10 items, minimize difficulty
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "min", negative=TRUE)
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) expect_equal(nrow(x), 10))
  b.avg <- mean(items$b[order(items$b, decreasing=FALSE)[1:50]])
  sapply(x$items, function(x) expect_lt(mean(x$b), b.avg))
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) expect_equal(nrow(x), 10))
  b.avg <- mean(items$b[order(items$b, decreasing=FALSE)[1:50]])
  sapply(x$items, function(x) expect_lt(mean(x$b), b.avg))
})


test_that("assmeble multiple forms with single absolute objective", {
  tol <- 0.2
  items <- pool(100)
  # 2 forms, 20 items, avg. difficulty = 0.5
  x <- ata(items, 2, len=20, maxselect=1)
  x <- ata_obj_absolute(x, items$b, 0.5 * 20)
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) expect_equal(nrow(x), 20))
  sapply(x$items, function(x) expect_equal(mean(x$b), 0.5, tolerance=tol, scale=1))
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) expect_equal(nrow(x), 20))
  sapply(x$items, function(x) expect_equal(mean(x$b), 0.5, tolerance=tol, scale=1))
})


test_that("assmeble multiple forms with multiple relative objective", {
  items <- pool(120)
  t <- seq(-1, 1, .5)
  t.avg <- irt_stats(model_3pl(theta=t, items=items), "info", summary="people", fun=mean)
  
  # 4 forms, 10 items, max. info. at seq(-1, 1, .5)
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, t, "max")
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) {
    info <- irt_stats(model_3pl(theta=t, items=x), "info", summary="people", fun=mean)
    expect_true(all(info > t.avg))
  })
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) {
    info <- irt_stats(model_3pl(theta=t, items=x), "info", summary="people", fun=mean)
    expect_true(all(info > t.avg))
  })
})


test_that("assmeble multiple forms with multiple absolute objective", {
  items <- pool(120)
  t <- seq(-1, 1, .5)
  t.avg <- irt_stats(model_3pl(theta=t, items=items), "info", summary="people", fun=mean)
  tol <- .2
  
  # 4 forms, 10 items, target avg. info. at seq(-1, 1, .5)
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_absolute(x, t, t.avg * 10)
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) {
    info <- irt_stats(model_3pl(theta=t, items=x), "info", summary="people", fun=mean)
    expect_equal(info, t.avg, tolerance=tol, scale=1)
  })
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) {
    info <- irt_stats(model_3pl(theta=t, items=x), "info", summary="people", fun=mean)
    expect_equal(info, t.avg, tolerance=tol, scale=1)
  })
})


test_that("assmeble multiple forms with multiple constraints", {
  items <- pool(100)
  
  # 4 forms, 10 items, max. difficulty
  # content: 3, 3, 4
  # time: avg. 58-62 seconds
  x <- ata(items, 4)
  x <- ata_obj_relative(x, "b", "max")
  # constraint: constant
  x <- ata_constraint(x, 1, min=10, max=10)
  # constraint: categorical
  x <- ata_constraint(x, "content", min=3, max=3, level=1)
  x <- ata_constraint(x, "content", min=3, max=3, level=2)
  x <- ata_constraint(x, "content", min=4, max=4, level=3)
  # constraint: quantitative
  x <- ata_constraint(x, "time", min=58*10, max=62*10)
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) expect_equal(freq(x$content, 1:3)$freq, c(3, 3, 4)))
  sapply(x$items, function(x) expect_true(mean(x$time) >= 58 && mean(x$time) <= 62))
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) expect_equal(freq(x$content, 1:3)$freq, c(3, 3, 4)))
  sapply(x$items, function(x) expect_true(mean(x$time) >= 58 && mean(x$time) <= 62))
})



test_that("assmeble multiple forms with item management", {
  items <- pool(6)
  
  # 3 forms, 2 items, max. difficulty
  # enemy items: I1, I3, I5
  # fix select: I1 in F1, I3 in F2, I5 in F3
  x <- ata(items, 3, len=2, maxselect=1)
  x <- ata_obj_relative(x, 0, "max")
  x <- ata_item_enemy(x, c(1, 3, 5))
  x <- ata_item_fixedvalue(x, 1, min=1, forms=1)
  x <- ata_item_fixedvalue(x, 3, min=1, forms=2)
  x <- ata_item_fixedvalue(x, 5, min=1, forms=3)
  # solver: lpsolve
  x <- ata_solve(x, "lpsolve")
  sapply(x$items, function(x) {
    expect_equal(nrow(x), 2)
    expect_equal(sum(x$id %in% c(1, 3, 5)), 1)
  })
  # solver: glpk
  x <- ata_solve(x, "glpk")
  sapply(x$items, function(x) {
    expect_equal(nrow(x), 2)
    expect_equal(sum(x$id %in% c(1, 3, 5)), 1)
  })
})


