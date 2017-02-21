library(testthat)
library(dplyr)
library(magrittr)
context("module5_mst.R")
set.seed(880813)

pool <- function(n) {
  x <- irt_model("3pl")$gendata(1, n)$items
  x$id <- 1:nrow(x)
  x$content <- sample(1:3, nrow(x), replace=TRUE)
  x$time <- round(exp(rnorm(nrow(x), log(60), .2)))
  x
}


test_that("add and remove routes", {
  items <- pool(300)
  x <- mst(items, design=c(1, 2, 3), npanel=2, method='topdown')
  expect_equal(x$nroute, 6)
  expect_equal(nrow(x$route), 6)

  # remove an existing route
  x <- mst_route(x, route=c(1, 2, 6), "-")
  expect_equal(x$nroute, 5)
  expect_equal(nrow(x$route), 5)
  expect_error(mst_route(x, route=c(1, 2, 6), "-"))
  
  # add a new route
  x <- mst_route(x, route=c(1, 2, 6), "+")
  expect_equal(x$nroute, 6)
  expect_equal(nrow(x$route), 6)
  expect_error(mst_route(x, route=c(1, 2, 6), "+"))
})


test_that("top-down MST", {
  items <- pool(300)
  
  # 1-2-3 MST, 2 panels, 30 items, 10 items each content
  # maximize information at single point
  x <- mst(items, design=c(1, 2, 3), npanel=2, method="topdown", len=30, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=-1, indices=1)
  x <- mst_objective(x, theta=0, indices=2:3)
  x <- mst_objective(x, theta=1, indices=4)
  x <- mst_constraint(x, "content", 10, 10, level=1)
  x <- mst_constraint(x, "content", 10, 10, level=2)
  x <- mst_constraint(x, "content", 10, 10, level=3)
  x <- mst_constraint(x, "time", 58*30, 62*30)
  x <- mst_stage_length(x, 1:3, min=5)
  # lpsolve
  x <- mst_assemble(x, "lpsolve", timeout=30)
  plot(x, byroute=T)
  for(p in 1:2)
    for(r in 1:4) {
      m <- mst_get_items(x, p, route=r)
      f <- freq(m$content, 1:3)$freq
      expect_equal(nrow(m), 30)
      expect_equal(f, c(10, 10, 10))
      expect_true(mean(m$time) >= 57 && mean(m$time) <= 63)
    }
  # glpk
  x <- mst_assemble(x, "glpk", timeout=30)
  plot(x, byroute=T)
  for(p in 1:2)
    for(r in 1:4) {
      m <- mst_get_items(x, p, route=r)
      f <- freq(m$content, 1:3)$freq
      expect_equal(nrow(m), 30)
      expect_equal(f, c(10, 10, 10))
      expect_true(mean(m$time) >= 57 && mean(m$time) <= 63)
    }
  
  # 1-2-3 MST, 2 panels, 30 items, 10 items each content
  # maximize information at multiple points
  x <- mst(items, design=c(1, 2, 3), npanel=1, method="topdown", len=30, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=c(-1.5, -.5), indices=1, flatten=.2, theta.step=3)
  x <- mst_objective(x, theta=c(-.5, .5), indices=2:3, flatten=.2, theta.step=3)
  x <- mst_objective(x, theta=c(.5, 1.5), indices=4, flatten=.2, theta.step=3)
  x <- mst_constraint(x, "content", 10, 10, level=1)
  x <- mst_constraint(x, "content", 10, 10, level=2)
  x <- mst_constraint(x, "content", 10, 10, level=3)
  x <- mst_constraint(x, "time", 58*30, 62*30)
  x <- mst_stage_length(x, 1:3, min=5)
  # lpsolve
  x <- mst_assemble(x, "lpsolve", timeout=30)
  plot(x, byroute=T)
  for(r in 1:4) {
    m <- mst_get_items(x, 1, route=r)
    f <- freq(m$content, 1:3)$freq
    expect_equal(nrow(m), 30)
    expect_equal(f, c(10, 10, 10))
    expect_true(mean(m$time) >= 57 && mean(m$time) <= 63)
  }
  
  # 1-2-3 MST, 2 panels, 30 items, 10 items each content
  # target information at multiple points
  x <- mst(items, design=c(1, 2, 3), npanel=1, method="topdown", len=30, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=c(-1.5, -.5), indices=1, target=14, theta.step=3)
  x <- mst_objective(x, theta=c(-.5, .5), indices=2:3, target=14, theta.step=3)
  x <- mst_objective(x, theta=c(.5, 1.5), indices=4, target=14, theta.step=3)
  x <- mst_constraint(x, "content", 10, 10, level=1)
  x <- mst_constraint(x, "content", 10, 10, level=2)
  x <- mst_constraint(x, "content", 10, 10, level=3)
  x <- mst_constraint(x, "time", 58*30, 62*30)
  x <- mst_stage_length(x, 1:3, min=5)
  # glpk
  x <- mst_assemble(x, "glpk", timeout=30)
  plot(x, byroute=T)
  for(r in 1:4) {
    m <- mst_get_items(x, 1, route=r)
    f <- freq(m$content, 1:3)$freq
    expect_equal(nrow(m), 30)
    expect_equal(f, c(10, 10, 10))
    expect_true(mean(m$time) >= 57 && mean(m$time) <= 63)
  }
})


test_that("bottom-up MST", {
  items <- pool(300)
  
  # 1-2-3 MST, 2 panels, 10 items each module, content=[4, 3, 3]
  # maximize information at single point
  x <- mst(items, design=c(1, 2, 3), npanel=2, method="bottomup", len=10, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=-1, indices=c(2, 4))
  x <- mst_objective(x, theta=0, indices=c(1, 5))
  x <- mst_objective(x, theta=1, indices=c(3, 6))
  x <- mst_constraint(x, "content", 4, 4, level=1)
  x <- mst_constraint(x, "content", 3, 3, level=2)
  x <- mst_constraint(x, "content", 3, 3, level=3)
  x <- mst_constraint(x, "time", 58*10, 62*10)
  # lpsolve
  x <- mst_assemble(x, "lpsolve", timeout=30)
  plot(x, byroute=T)
  sapply(split(x$items, list(x$items$panel, x$items$index)), function(x){
    expect_true(mean(x$time) >= 57 && mean(x$time) <= 63)
    expect_equal(freq(x$content, 1:3)$freq, c(4, 3, 3))
  })
  # glpk
  x <- mst_assemble(x, "glpk", timeout=30)
  plot(x, byroute=T)
  sapply(split(x$items, list(x$items$panel, x$items$index)), function(x){
    expect_true(mean(x$time) >= 57 && mean(x$time) <= 63)
    expect_equal(freq(x$content, 1:3)$freq, c(4, 3, 3))
  })
  
  # 1-2-3 MST, 2 panels, 30 items, 10 items each content
  # maximize information at multiple points
  x <- mst(items, design=c(1, 2, 3), npanel=1, method="bottomup", len=10, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=c(-1.5, -.5), indices=c(2, 4), flatten=.2, theta.step=3)
  x <- mst_objective(x, theta=c(-.5, .5), indices=c(1, 5), flatten=.2, theta.step=3)
  x <- mst_objective(x, theta=c(.5, 1.5), indices=c(3, 6), flatten=.2, theta.step=3)
  x <- mst_constraint(x, "content", 4, 4, level=1)
  x <- mst_constraint(x, "content", 3, 3, level=2)
  x <- mst_constraint(x, "content", 3, 3, level=3)
  # x <- mst_constraint(x, "time", 58*10, 62*10)
  # lpsolve
  x <- mst_assemble(x, "lpsolve", timeout=30)
  plot(x, byroute=T)
  sapply(split(x$items, list(x$items$panel, x$items$index)), function(x){
    # expect_true(mean(x$time) >= 57 && mean(x$time) <= 63)
    expect_equal(freq(x$content, 1:3)$freq, c(4, 3, 3))
  })

  # 1-2-3 MST, 2 panels, 30 items, 10 items each content
  # target information at multiple points
  x <- mst(items, design=c(1, 2, 3), npanel=1, method="bottomup", len=10, maxselect=1)
  x <- mst_route(x, c(1, 2, 6), "-")
  x <- mst_route(x, c(1, 3, 4), "-")
  x <- mst_objective(x, theta=c(-1.5, -.5), indices=c(2, 4), target=5, theta.step=3)
  x <- mst_objective(x, theta=c(-.5, .5), indices=c(1, 5), target=5, theta.step=3)
  x <- mst_objective(x, theta=c(.5, 1.5), indices=c(3, 6), target=5, theta.step=3)
  x <- mst_constraint(x, "content", 4, 4, level=1)
  x <- mst_constraint(x, "content", 3, 3, level=2)
  x <- mst_constraint(x, "content", 3, 3, level=3)
  # x <- mst_constraint(x, "time", 58*10, 62*10)
  # glpk
  x <- mst_assemble(x, "glpk", timeout=30)
  plot(x, byroute=T)
  sapply(split(x$items, list(x$items$panel, x$items$index)), function(x){
    # expect_true(mean(x$time) >= 57 && mean(x$time) <= 63)
    expect_equal(freq(x$content, 1:3)$freq, c(4, 3, 3))
  })
})


test_that("set rdp and minimal information", {
  items <- pool(300)
  
  # set rdp
  x <- mst(items, c(1, 2, 2), npanel=2, method="topdown", len=20, maxselect=1)
  x <- mst_objective(x, -1, 1:2)
  x <- mst_objective(x,  1, 3:4)
  x <- mst_stage_length(x, 1:3, min=3)
  x <- mst_set_rdp(x, 1, 2:3, tol=.5)
  x <- mst_assemble(x, "lpsolve", timeout=10)
  plot(x, byroute=F)
  rs <- x$items %>% model_3pl(theta=0, items=.) %>% irt_stats(., "info") %>%
    t() %>% aggregate(., list(x$items$panel, x$items$index), sum) %>%
    rename(panel=Group.1, index=Group.2, info=people.1)
  for(p in 1:2)
    expect_lte(abs(filter(rs, panel==p, index==2)$info - filter(rs, panel==p, index==3)$info), 0.6)

  # set minimum information
  x <- mst(items, c(1, 2, 2), npanel=2, method="topdown", len=20, maxselect=1)
  x <- mst_objective(x, -1, 1:2)
  x <- mst_objective(x,  1, 3:4)
  x <- mst_module_mininfo(x, 0, 3, indices=1:5)
  x <- mst_assemble(x, "lpsolve", timeout=10)
  plot(x, byroute=F)
  rs <- x$items %>% model_3pl(theta=0, items=.) %>% irt_stats(., "info") %>%
    t() %>% aggregate(., list(x$items$panel, x$items$index), sum) %>%
    rename(panel=Group.1, index=Group.2, info=people.1)
  expect_true(all(rs$info >= 3))
})


test_that("set stage length", {
  items <- pool(300)
  x <- mst(items, c(1, 2, 2), npanel=2, method="topdown", len=20, maxselect=1)
  x <- mst_objective(x, -1, 1:2)
  x <- mst_objective(x,  1, 3:4)
  x <- mst_stage_length(x, 1:3, min=5, max=7)
  x <- mst_assemble(x, "lpsolve", timeout=10)
  plot(x, byroute=F)
  rs <- x$items %>% group_by(panel, stage, module) %>% summarise(n=n())
  expect_true(all(rs$n >= 5 & rs$n <= 7))
})

