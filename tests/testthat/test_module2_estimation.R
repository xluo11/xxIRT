library(testthat)
context("module2_estimation.R")
set.seed(880813)


test_that("estimate people parameters", {
  thresh <- 0.4
  data <- irt_model("3pl")$gendata(2000, 50)
  # MLE
  x <- estimate_people(data$responses, data$items, model="3pl", method="mle")
  expect_true(rmse(data$people, x$people) < thresh)
  # MAP
  x <- estimate_people(data$responses, data$items, model="3pl", method="map")
  expect_true(rmse(data$people, x$people) < thresh)
  # EAP
  x <- estimate_people(data$responses, data$items, model="3pl", method="eap")
  expect_true(rmse(data$people, x$people) < thresh)
})


test_that("estimate item parameters", {
  thresh <- 0.4
  data <- irt_model("3pl")$gendata(2000, 30)
  # JMLE
  x <- estimate_items(data$responses, model="3pl", method="jmle", people=data$people)
  expect_true(all(rmse(data$items, x$items) < thresh))
  # MMLE
  x <- estimate_items(data$responses, model="3pl", method="mmle")
  expect_true(all(rmse(data$items, x$items) < thresh))
  # BME
  x <- estimate_items(data$responses, model="3pl", method="bme")
  expect_true(all(rmse(data$items, x$items) < thresh))
})
