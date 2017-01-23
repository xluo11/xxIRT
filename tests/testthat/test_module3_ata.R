library(testthat)
library(magrittr)
library(dplyr)

context("module3_ata.R")

test_that("create ata object", {
  items <- irt_model("3pl")$gendata(1, 100)$items
  items$content <- sample(1:3, nrow(items), replace=TRUE)
  items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
  
  # use 'len' and 'maxselect'
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_solve(x)
  y <- ata_get_items(x, as.list=TRUE)
  for(i in 1:x$nform) expect_equal(nrow(y[[i]]), 10)
  
  # not use 'len' and 'maxselect'
  x <- ata(items, 4)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, 1, min=10, max=10)
  x <- ata_item_maxselect(x, 1)
  x <- ata_solve(x)
  y <- ata_get_items(x, as.list=TRUE)
  for(i in 1:x$nform) expect_equal(nrow(y[[i]]), 10)
})

test_that("add absolute objective functions", {
  items <- irt_model("3pl")$gendata(1, 100)$items
  items$content <- sample(1:3, nrow(items), replace=TRUE)
  items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
  
  # absolute objective
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_absolute(x, "b", 0 * 10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  b <- y %>% group_by(form) %>% summarise(b=mean(b))
  expect_true(all(abs(b$b - 0) < 0.2))
  
  # absolute objective for differrent forms
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_absolute(x, "b",  0.5 * 10, forms=1:2)
  x <- ata_obj_absolute(x, "b", -0.5 * 10, forms=3:4)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  b <- y %>% group_by(form) %>% summarise(b=mean(b))
  expect_true(all(abs(b$b[1:2] - 0.5) < 0.2))
  expect_true(all(abs(b$b[3:4] + 0.5) < 0.2))

  # absolute objective for thetas
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_absolute(x, c(-0.2, 0.2), 0.5 * 10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  info <- irt_stats(irt_model("3pl", theta=c(-0.2, 0.2), items=y), "info")
  info <- stats::aggregate(t(info), list(y$form), sum)
  expect_true(all(abs(info[, -1] - 0.5 * 10) < 0.2))
  
  # absolute objective for vector values
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_absolute(x, items$a ^ 2 * items$b, 0.5 * 10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- aggregate(y$a ^ 2 * y$b, list(y$form), sum)
  expect_true(all(abs(val[, -1] - 0.5 * 10) < 0.2))
})

test_that("add relative objective functions", {
  items <- irt_model("3pl")$gendata(1, 100)$items
  items$content <- sample(1:3, nrow(items), replace=TRUE)
  items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
  
  # relative objective: maximization
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_solve(x)
  y <- ata_get_items(x)
  b <- y %>% group_by(form) %>% summarise(b=mean(b)) 
  thresh <- mean(items$b[order(items$b, decreasing=TRUE)][31:40])
  expect_true(all(b$b > thresh))
  
  # relative objective: minimization
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "min", negative=TRUE)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  b <- y %>% group_by(form) %>% summarise(b=mean(b)) 
  thresh <- mean(items$b[order(items$b, decreasing=FALSE)][31:40])
  expect_true(all(b$b < thresh))
  
  # relative objective for thetas
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, c(-0.2, 0.2), "max")
  x <- ata_solve(x)
  y <- ata_get_items(x)
  for(t in c(-0.2, 0.2)){
    items.info <- irt_stats(irt_model("3pl", theta=t, items=items), "info")
    items.info <- mean(items.info[order(items.info, decreasing=TRUE)][31:40])
    y.info <- irt_stats(irt_model("3pl", theta=t, items=y), "info")
    y.info <- stats::aggregate(as.vector(y.info), list(y$form), mean)
    expect_true(all(y.info[, -1] > items.info))
  }
  
  # relative objective for vector values
  value <-   items$a ^ 2 * items$b
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, value, "max")
  x <- ata_solve(x)
  y <- ata_get_items(x)
  expect_true(all(aggregate(y$a ^ 2 * y$b, list(y$form), sum)[, -1] > 
                    mean(value[order(value, decreasing=TRUE)][31:40])))
})

test_that("add constraints", {
  items <- irt_model("3pl")$gendata(1, 100)$items
  items$content <- sample(1:3, nrow(items), replace=TRUE)
  items$time <- round(rlnorm(nrow(items), log(60), .2), 0)

  # add categorical constraint for all forms with both min and max
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef="content", min=3, max=3, level=1)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(content=sum(content == 1))
  expect_true(all(val$content == 4))
  
  # add categorical constraint for all forms with min only
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef="content", min=3, level=1)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(content=sum(content == 1))
  expect_true(all(val$content >= 3))
  
  # add categorical constraint for all forms with max only
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef="content", max=3, level=1)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(content=sum(content == 1))
  expect_true(all(val$content <= 3))
  
  # add categorical constraint for 2 forms with min and max
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef="content", min=4, max=4, level=1, forms=1:2)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(content=sum(content == 1))
  expect_true(all(val$content[1:2] == 4))
  
  # add quantitaive constraint for all forms with both min and max
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef="time", min=59*10, max=61*10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(time=mean(time))
  expect_true(all(val$time >= 59 & val$time < 61))
  
  # add constant constraint for all forms with both min and max
  x <- ata(items, 4, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, 1, min=10, max=10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(len=n())
  expect_true(all(val$len == 10))
  
  # add vector value constraint for all forms with both min and max
  x <- ata(items, 4, len=10, maxselect=1)
  x <- ata_obj_relative(x, "b", "max")
  x <- ata_constraint(x, coef=items$a, min=0.8*10, max=1.0*10)
  x <- ata_solve(x)
  y <- ata_get_items(x)
  val <- y %>% group_by(form) %>% summarise(a=mean(a))
  expect_true(all(val$a >= 0.8 & val$a <= 1.1))
})

test_that("enemy items", {
  items <- irt_model("3pl")$gendata(1, 5)$items
  items$id <- 1:nrow(items)
  
  for(i in 2:5){
    x <- ata(items, 1, len=4, maxselect=1)
    x <- ata_obj_relative(x, "b", "max")
    x <- ata_item_enemy(x, c(1, i))
    x <- ata_solve(x)
    y <- ata_get_items(x)
    expect_false(all(c(1, i) %in% y$id))
  }
})

test_that("fixed selection", {
  items <- irt_model("3pl")$gendata(1, 5)$items
  items$id <- 1:nrow(items)
  
  for(i in 1:nrow(items)){
    x <- ata(items, 1, len=3, maxselect=1)
    x <- ata_obj_relative(x, "b", "max")
    x <- ata_item_fixedvalue(x, items=i, min=1, max=1)
    x <- ata_solve(x)
    y <- ata_get_items(x)
    expect_true(i %in% y$id)
  }
})

