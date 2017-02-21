library(testthat)
library(dplyr)
context("module4_cat.R")

pool <- function(n) {
  x <- irt_model("3pl")$gendata(1, n)$items
  x$id <- 1:nrow(x)
  x$content <- sample(1:3, nrow(x), replace=TRUE)
  x$time <- round(exp(rnorm(nrow(x), log(60), .2)))
  x  
}


test_that("default rules", {
  items <- pool(200)
  theta <- seq(-3, 3, .5)
  iter <- 10
  rmse.tol <- 0.4
  
  # fixed length
  rs <- matrix(nrow=length(theta) * iter, ncol=3, dimnames=list(NULL, c("true", "est", "len")))
  for(i in 1:length(theta))
    for(j in 1:iter) {
      opts <- list(min=30, max=30, stop.se=.25)
      x <- cat_sim(theta[i], items, opts)
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$len)
      expect_equal(nrow(x$admin), x$len)
      expect_equal(nrow(x$items), x$len)
      expect_equal(nrow(x$pool), nrow(items) - x$len)
      expect_equal(x$true, theta[i], tolerance=0.01, scale=1)
    }
  rs <- as.data.frame(rs)
  expect_true(all(rs$len == 30))
  rs <- rs %>% group_by(true) %>% summarise(rmse=sqrt(mean((true-est)^2)))
  expect_true(all(rs$rmse <= rmse.tol))
  
  # variable length
  rs <- matrix(nrow=length(theta) * iter, ncol=3, dimnames=list(NULL, c("true", "est", "len")))
  for(i in 1:length(theta))
    for(j in 1:iter) {
      opts <- list(min=28, max=32, stop.se=.25)
      x <- cat_sim(theta[i], items, opts)
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$len)
      expect_equal(nrow(x$admin), x$len)
      expect_equal(nrow(x$items), x$len)
      expect_equal(nrow(x$pool), nrow(items) - x$len)
      expect_equal(x$true, theta[i], tolerance=0.01, scale=1)
    }
  rs <- as.data.frame(rs)
  expect_true(all(rs$len >= 28))
  expect_true(all(rs$len <= 32))
  expect_true(length(unique(rs$len)) > 1)
  rs <- rs %>% group_by(true) %>% summarise(rmse=sqrt(mean((true-est)^2)))
  expect_true(all(rs$rmse <= rmse.tol))
})


test_that("randomesque", {
  items <- pool(100)
  theta <- 0.1
  iter <- 100
  
  # randomesque = 1
  rs <- rep(NA, iter)
  for(k in 1:iter) {
    opts <- list(min=5, max=5, stop.se=0.3, randomesque=1)
    x <- cat_sim(theta, items, opts)
    rs[k] <- x$admin$id[1]
  }
  expect_true(length(unique(rs)) == 1)
  
  # randomesque = 5
  rs <- rep(NA, iter)
  for(k in 1:iter) {
    opts <- list(min=5, max=5, stop.se=0.3, randomesque=5)
    x <- cat_sim(theta, items, opts)
    rs[k] <- x$admin$id[1]
  }
  expect_true(length(unique(rs)) >= 1)
  expect_true(length(unique(rs)) <= 5)
  
  # randomesque = 10
  rs <- rep(NA, iter)
  for(k in 1:iter) {
    opts <- list(min=5, max=5, stop.se=0.3, randomesque=10)
    x <- cat_sim(theta, items, opts)
    rs[k] <- x$admin$id[1]
  }
  expect_true(length(unique(rs)) >= 1)
  expect_true(length(unique(rs)) <= 10)
})

test_that("default stopping rule", {
  items <- pool(300) 
  theta <- seq(-3, 3, .5)
  iter <- 10
  
  # standard error
  rs <- matrix(nrow=length(theta) * iter, ncol=4, dimnames=list(NULL, c("true", "est", "se", "len")))
  opts <- list(min=5, max=30, stop.se=.4)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts)
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$admin$se[x$len], x$len)
    }
  apply(rs, 1, function(x) expect_true(x["se"] <= 0.4 || x["len"] == 30))
  
  # minimum information
  rs <- matrix(nrow=length(theta) * iter, ncol=4, dimnames=list(NULL, c("true", "est", "len", "mi")))
  opts <- list(min=5, max=30, stop.mi=.8)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts)
      mi <- irt_stats(model_3pl(theta=x$est, items=x$pool), "info")
      mi <- max(mi)
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$len, mi)
    }
  apply(rs, 1, function(x) expect_true(x["mi"] <= 0.8 || x["len"] == 30))
  
  # cut score
  rs <- matrix(nrow=length(theta) * iter, ncol=4, dimnames=list(NULL, c("true", "est", "se", "len")))
  opts <- list(min=5, max=30, stop.cut=0)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts)
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$admin$se[x$len], x$len)
    }
  apply(rs, 1, function(x) expect_true(x["est"] - 1.96 * x["se"] >= 0 || x["est"] + 1.96 * x["se"] <= 0 || x["len"] == 30))
})


test_that("c-cat selection rule", {
  items <- pool(200)
  theta <- seq(-3, 3, .5)
  iter <- 10
  rmse.tol <- 0.4
  
  # no initial randomness
  rs <- matrix(nrow=length(theta) * iter, ncol=10 + 2)
  opts <- list(min=30, max=60, stop.se=.3, ccat.target=c(.5, .3, .2), ccat.random=0)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts, cat.select=cat_select_ccat)
      f <- freq(x$admin$content, 1:3)$perc / 100
      expect_equal(f, c(.5, .3, .2), tolerance=0.05, scale=1)
      rs[j + (i - 1) * iter, ] <- c(x$admin$content[1:10], x$true, x$est)
    }
  apply(rs[,1:10], 2, function(x) expect_equal(length(unique(x)), 1))
  rs2 <- data.frame(true=rs[,11], est=rs[,12]) %>%
    group_by(true) %>% summarise(rmse=sqrt(mean((true-est)^2)))
  expect_true(all(rs2$rmse <= rmse.tol))
  
  # has initial randomness
  rs <- matrix(nrow=length(theta) * iter, ncol=10 + 2)
  opts <- list(min=30, max=60, stop.se=0.3, ccat.target=c(.5, .3, .2), ccat.random=5)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts, cat.select=cat_select_ccat)
      f <- freq(x$admin$content, 1:3)$perc / 100
      expect_equal(f, c(.5, .3, .2), tolerance=0.05, scale=1)
      rs[j + (i - 1) * iter, ] <- c(x$admin$content[1:10], x$true, x$est)
    }
  apply(rs[, 1:5], 2, function(x) expect_gt(length(unique(x)), 1))
  rs2 <- data.frame(true=rs[,11], est=rs[,12]) %>%
    group_by(true) %>% summarise(rmse=sqrt(mean((true-est)^2)))
  expect_true(all(rs2$rmse <= rmse.tol))
})


test_that("shadow-test selection rule", {
  items <- pool(200)
  theta <- seq(-3, 3, .5)
  iter <- 10
  rmse.tol <- .4
  
  rs <- matrix(nrow=length(theta) * iter, ncol=3, dimnames=list(NULL, c("true", "est", "len")))
  con <- data.frame(name="content", level=c(1, 2, 3), min=c(10, 10, 10), max=c(10, 10,10))
  opts <- list(min=30, max=30, stop.cut=0, shadow.constraints=con)
  for(i in 1:length(theta))
    for(j in 1:iter) {
      x <- cat_sim(theta[i], items, opts, cat.select=cat_select_shadow, debug=F)
      f <- freq(x$admin$content, 1:3)$freq
      expect_equal(f, c(10, 10, 10))
      rs[j + (i - 1) * iter, ] <- c(x$true, x$est, x$len)
    }
  rs <- as.data.frame(rs) %>% group_by(true) %>% summarise(rmse=sqrt(mean((true-est)^2)))
  expect_true(all(rs$rmse <= rmse.tol))
})