library(testthat)
context("module0_model_3pl.R")

test_that("create a model using given data", {
  theta <- c(-1, 0, 1)
  a <- c(.5882, 1)
  b <- c(-1, 1)
  c <- c(0, .2)
  people <- data.frame(theta=theta)
  items <- data.frame(a=a, b=b, c=c)
  
  x <- model_3pl(people, items)
  expect_equal(x$people, people)
  expect_equal(x$items, items)
  expect_null(x$responses)
  
  x <- model_3pl(theta=theta, a=a, b=b, c=c)
  expect_equal(x$people, people)
  expect_equal(x$items, items)
  expect_null(x$responses)
  
  x <- model_3pl(items=items, theta=theta)
  expect_equal(x$people, people)
  expect_equal(x$items, items)
  expect_null(x$responses)
  
  x <- model_3pl(people=people, a=a, b=b, c=c)
  expect_equal(x$people, people)
  expect_equal(x$items, items)
  expect_null(x$responses)
  
  expect_error(model_3pl(people, a=rep(1, 3), b=b, c=c))
  expect_error(model_3pl(people, a=a, b=b, c=rep(0, 3)))
})

test_that("create a model using generate data", {
  tol <- 0.2
  x <- model_3pl()$gendata(1000, 200)

  expect_equal(mean(x$people$theta), 0, tolerance=tol, scale=1)
  expect_equal(sd(x$people$theta), 1, tolerance=tol, scale=1)
  expect_equal(mean(x$items$b), 0, tolerance=tol, scale=1)
  expect_equal(sd(x$items$b), 1, tolerance=tol, scale=1)
  expect_equal(mean(log(x$items$a)), 0, tolerance=tol, scale=1)
  expect_equal(sd(log(x$items$a)), 0.2, tolerance=tol, scale=1)
  expect_equal(mean(x$items$c), 5 / (5 + 46), tolerance=tol, scale=1)
  expect_equal(sd(x$items$c), sqrt(5 * 46 / (5  + 46)^2 / (5 + 46 + 1)), tolerance=tol, scale=1)
  expect_equal(dim(x$responses), c(1000, 200))
})

test_that("compute P, I, L", {
  theta <- c(-1, 0, 1)
  a <- c(.5882, 1)
  b <- c(-1, 1)
  c <- c(0, .2)
  u <- matrix(c(1, 0, 1, 0, 1, 0), nrow=3)
  x <- model_3pl(theta=theta, a=a, b=b, c=c, responses=u)
  P <- x$P(x)
  for(i in 1:length(theta))
    for(j in 1:length(b)){
      p_manual <- c[j] + (1 - c[j]) / (1 + exp(-1.7 * a[j] * (theta[i] - b[j])))
      expect_equal(P[i, j], p_manual)
    }
  I <- x$I(x)
  for(i in 1:length(theta))
    for(j in 1:length(b)){
      i_manual <- (1.7 * a[j] * (P[i, j] - c[j]) / (1 - c[j]))^2 * (1 - P[i, j]) / P[i, j]
      expect_equal(I[i, j], i_manual)
    }
  L <- x$L(x)
  for(i in 1:length(theta))
    for(j in 1:length(b)){
      l_manual <- ifelse(u[i, j] == 1, P[i, j], 1 - P[i, j])
      expect_equal(L[i, j], l_manual)
    }
})
