library(testthat)
context("module2_estimation.R")

test_that("estimate people parameters", {
  thresh.corr <- 0.9
  thresh.rmse <- 0.4
  data <- irt_model("3pl")$gendata(2000, 50)
  
  x <- estimate_people(data$responses, data$items, model="3pl", method="mle")
  expect_true(cor(data$people, x$people) > thresh.corr)
  expect_true(rmse(data$people, x$people) < thresh.rmse)
  
  x <- estimate_people(data$responses, data$items, model="3pl", method="map")
  expect_true(cor(data$people, x$people) > thresh.corr)
  expect_true(rmse(data$people, x$people) < thresh.rmse)
  
  x <- estimate_people(data$responses, data$items, model="3pl", method="eap")
  expect_true(cor(data$people, x$people) > thresh.corr)
  expect_true(rmse(data$people, x$people) < thresh.rmse)
})

test_that("estimate item parameters", {
  thresh <- 0.4
  data <- irt_model("3pl")$gendata(2000, 50)
  
  x <- estimate_items(data$responses, model="3pl", method="jmle", people=data$people)
  expect_true(all(rmse(data$items, x$items) < thresh))
  
  x <- estimate_items(data$responses, model="3pl", method="mmle")
  expect_true(all(rmse(data$items, x$items) < thresh))
  
  x <- estimate_items(data$responses, model="3pl", method="bme")
  expect_true(all(rmse(data$items, x$items) < thresh))
})
