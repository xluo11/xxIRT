library(testthat)
context("module1_irt_utils.R")
set.seed(880813)


test_that("create irt models", {
  x <- model_3pl()$gendata(10, 5)
  y <- irt_model("3pl", x$people, x$items, x$responses)
  expect_equal(x$people, y$people)
  expect_equal(x$items, y$items)
  expect_equal(x$responses, y$responses)
  
  y <- irt_model("3pl", theta=x$people$theta, a=x$items$a, b=x$items$b, c=x$items$c, responses=x$responses)
  expect_equal(x$people, y$people)
  expect_equal(x$items, y$items)
  expect_equal(x$responses, y$responses)
})

test_that("compute irt statistics", {
  x <- model_3pl()$gendata(10, 5)
  expect_equal(irt_stats(x, stats="prob"), x$P(x))
  expect_equal(irt_stats(x, stats="prob", summary="people", fun=sum), rowSums(x$P(x)))
  expect_equal(irt_stats(x, stats="info"), x$I(x))
  expect_equal(irt_stats(x, stats="info", summary="items", fun=sum), colSums(x$I(x)))
  expect_equal(irt_stats(x, stats="lik"), x$L(x))
  expect_equal(irt_stats(x, stats="lik", summary="people", fun=sum), rowSums(x$L(x)))
})

test_that("subset irt model", {
  x <- model_3pl()$gendata(20, 10)
  people.index <- c(1, 3, 5)
  items.index <- c(2, 4, 6)
  y <- irt_select(x, people.index, items.index)
  expect_equal(x$people$theta[people.index], y$people$theta)
  expect_equal(x$items[items.index, , drop=FALSE], y$items)
  expect_equal(x$responses[people.index, items.index], y$responses)

  y <- irt_select(x, people.index=people.index)
  expect_equal(x$people$theta[people.index], y$people$theta)
  expect_equal(x$items, y$items)
  expect_equal(x$responses[people.index, , drop=FALSE], y$responses)

  y <- irt_select(x, items.index=items.index)
  expect_equal(x$people, y$people)
  expect_equal(x$items[items.index, ], y$items)
  expect_equal(x$responses[, items.index], y$responses)
})

test_that("sample irt model", {
  x <- model_3pl()$gendata(20, 10)
  n.people <- 5
  n.items <- 3
  y <- irt_sample(x, n.people, n.items)
  expect_equal(nrow(y$people), n.people)
  expect_equal(nrow(y$items), n.items)
  expect_equal(dim(y$responses), c(n.people, n.items))
  
  y <- irt_sample(x, n.people = n.people)
  expect_equal(nrow(y$people), n.people)
  expect_equal(nrow(y$items), nrow(x$items))
  expect_equal(dim(y$responses), c(n.people, nrow(x$items)))

  y <- irt_sample(x, n.items = n.items)
  expect_equal(nrow(y$people), nrow(x$people))
  expect_equal(nrow(y$items), n.items)
  expect_equal(dim(y$responses), c(nrow(x$people), n.items))
})

test_that("rescale 3pl model", {
  x <- model_3pl()$gendata(20, 10)
  
  y <- irt_rescale_3pl(x, "theta", 0, 1)
  expect_equal(mean(y$people$theta), 0)
  expect_equal(sd(y$people$theta), 1)

  y <- irt_rescale_3pl(x, "b", 0, 1)
  expect_equal(mean(y$items$b), 0)
  expect_equal(sd(y$items$b), 1)
})