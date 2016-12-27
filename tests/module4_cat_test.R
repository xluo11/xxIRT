library(reshape2)
library(ggplot2)
library(lpSolveAPI)
library(gaussquad)

# generate an item pool
pool <- irt.model()$gen.data(1,200)$items
pool$content <- sample(1:3, nrow(pool), replace=TRUE)
pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))

# cat simulation: 10-30 items, default rules, stop criterion: se = .3
opts <- list(min=10, max=30, stop.se=.3)
x <- cat.sim(0.1, pool, opts)
x$admin
plot(x)
# cat simulation: 10-30 items, default rules, stop criterion: mi = .3
opts <- list(min=10, max=30, stop.mi=.8)
x <- cat.sim(0.1, pool, opts, debug=TRUE)
# cat simulation: 10-30 items, default rules, stop criterion: cut = -.5
opts <- list(min=10, max=30, stop.cut=-0.5)
x <- cat.sim(0.1, pool, opts, debug=TRUE)
# cat simulation: 10-30 items, default rules, stop criterion: se = .3, randomesque=10
opts <- list(min=10, max=30, stop.se=.3, randomesque)
x <- cat.sim(0.1, pool, opts)
x
plot(x)
# cat simulation: 10-30 items, c-cat selection rule, first 10 random
opts <- list(min=30, max=60, stop.cut=0, ccat.target=c(.5,.25,.25), ccat.random=10)
x <- cat.sim(0.1, pool, opts, cat.select=cat.select.ccat)
x
freq(x$admin$content, 1:3)
plot(x)

# cat simulation with shadow test
cons <- data.frame(name="content", level=1, min=10, max=10, stringsAsFactors=FALSE)
cons <- rbind(cons, c("content", 2, 10, 10))
cons <- rbind(cons, c("content", 3, 10, 10))
cons <- rbind(cons, c("time", NA, 55*30, 65*30))
opts <- list(min=30, max=30, stop.se=.03, shadow.constraints=cons)
x <- cat.sim(0.1, pool, opts, cat.select=cat.select.shadow, debug=TRUE)
x
freq(x$admin$content, 1:3)
sum(x$items$time)
plot(x)

# cat simulation using projection-based stopping rule
cons <- data.frame(name="content", level=1, min=10, max=10, stringsAsFactors=FALSE)
cons <- rbind(cons, c("content", 2, 10, 10))
cons <- rbind(cons, c("content", 3, 10, 10))
opts <- list(min=10, max=30, stop.cut=0, projection.constraints=cons, projection.method="information")
x <- cat.sim(0.1, pool, opts, cat.stop=cat.stop.projection, debug=TRUE)
x
