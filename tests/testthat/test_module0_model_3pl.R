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
  thresh <- 0.1
  x <- model_3pl()$gendata(1000, 200)
  theta.mean <- mean(x$people$theta)
  theta.sd <- sd(x$people$theta)
  a.mean <- mean(log(x$items$a))
  a.sd <- sd(log(x$items$a))
  b.mean <- mean(x$items$b)
  b.sd <- sd(x$items$b)
  c.mean <- mean(x$items$c)
  c.sd <- sd(x$items$c)
  expect_true(abs(theta.mean - 0) < thresh)
  expect_true(abs(theta.sd - 1) < thresh)
  expect_true(abs(a.mean - 0) < thresh)
  expect_true(abs(a.sd - 0.2) < thresh)
  expect_true((b.mean - 0) < thresh)
  expect_true(abs(b.sd - 1) < thresh)
  expect_true(abs(c.mean - 5 / (5 + 42)) < thresh)
  expect_true(abs(c.sd - sqrt(5 * 42 / (5  + 42)^2 / (5 + 42 + 1))) < thresh)
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
