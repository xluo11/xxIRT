library(reshape2)
library(ggplot2)
library(lpSolveAPI)
library(gaussquad)

# create an ata object
items <- irt.model(model="3pl")$gen.data(1, 3)$items
items$content <- sample(1:3, nrow(items), replace=TRUE)
items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
x <- ata(items, debug=TRUE)

# add relative objectives using a vector
x <- ata(items, debug=TRUE)
ata.obj.relative(x, 2*x$pool$a+x$pool$b, "max")
2*x$pool$a+x$pool$b
# add relative objectives using a variable
x <- ata(items, debug=TRUE)
ata.obj.relative(x, c("a","b"), "min", negative=TRUE)
x$pool[, c("a", "b")]
# add relative objectives using thetas
x <- ata(items, debug=TRUE)
ata.obj.relative(x, c(-0.5, 0.5), "max")
irt.stats(irt.model.3pl(data.frame(theta=c(-0.5,0.5)), x$pool), "information")
# add relative objectives with flatten parameter
x <- ata(items, debug=TRUE)
ata.obj.relative(x, c(-0.5, 0.5), "max", flatten=0.1)
# add relative objectives that are compensatory
x <- ata(items, debug=TRUE)
ata.obj.relative(x, c(-0.5, 0.5), "max", compensate=TRUE)
# add relative objectives to form 2
x <- ata(items, 2, debug=TRUE)
ata.obj.relative(x, c(-0.5, 0.5), "max", forms=2)
# add relative objectives to collapse forms
x <- ata(items, 2, debug=TRUE)
ata.obj.relative(x, c(-0.5, 0.5), "max", forms=1:2, collapse=TRUE)

# add absolute objectives using a vector
x <- ata(items, debug=TRUE)
ata.obj.absolute(x, 2*x$pool$a+x$pool$b, 5)
2*x$pool$a+x$pool$b
# add absolute objectives using a variable
x <- ata(items, debug=TRUE)
ata.obj.absolute(x, c("a", "b"), 5)
x$pool[,c("a", "b")]
# add absolute objectives using thetas
x <- ata(items, debug=TRUE)
ata.obj.absolute(x, c(-0.5, 0.5), 5)
irt.stats(irt.model.3pl(data.frame(theta=c(-0.5,0.5)), x$pool), "information")
# add absolute objectives with different targets
x <- ata(items, debug=TRUE)
ata.obj.absolute(x, c(-0.5, 0.5), c(5, 10))
# add absolute objectives that are compensatory
x <- ata(items, debug=TRUE)
ata.obj.absolute(x, c(-0.5, 0.5), 5, TRUE)
# add absolute objectives to form 2
x <- ata(items, 2, debug=TRUE)
ata.obj.absolute(x, c(-0.5, 0.5), 5, forms=2)
# add absolute objectives with collapsed forms
x <- ata(items, 2, debug=TRUE)
ata.obj.absolute(x, c(-0.5, 0.5), 5, forms=1:2, collapse=TRUE)

# add a categorical constraint
x <- ata(items, debug=TRUE)
ata.constraint(x, "content", min=5, max=10, level=1)
# add a categorical constraint with same min and max
x <- ata(items, debug=TRUE)
ata.constraint(x, "content", min=5, max=5, level=1)
# add a quantitative constraint
x <- ata(items, debug=TRUE)
ata.constraint(x, "time", min=5, max=10)
# add constraint using a constant
x <- ata(items, debug=TRUE)
ata.constraint(x, 1, min=5, max=5)
# add constraint using a vector
x <- ata(items, debug=TRUE)
ata.constraint(x, 1:x$nitem, min=5, max=5)
# add constraint to form 2
x <- ata(items, 2, debug=TRUE)
ata.constraint(x, "content", min=5, max=5, level=1, form=2)
# add constraint with collapsed forms
x <- ata(items, 2, debug=TRUE)
ata.constraint(x, "content", min=5, max=5, level=1, form=1:2, collapse=TRUE)

# set maximal times of selections for items
x <- ata(items, 2, debug=TRUE)
ata.item.maxselect(x, maxselect=1)
x <- ata(items, 2, debug=TRUE)
ata.item.maxselect(x, maxselect=2, items=c(1,2))

# enemy relationship
x <- ata(items, 2, debug=TRUE)
ata.item.enemy(x, items=c(2,3))

# fix values
x <- ata(items, 2, debug=TRUE)
ata.item.fix(x, items=1:2, min=1, max=1)
x <- ata(items, 2, debug=TRUE)
ata.item.fix(x, itms=1:2, min=0, max=0)

# ex. 1: 2 forms, 10 items, maximize b parmaters
items <- irt.model(model="3pl")$gen.data(1, 100)$items
items$content <- sample(1:3, nrow(items), replace=TRUE)
items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
x <- ata(items, 2, debug=TRUE)
x <- ata.obj.relative(x, "b", "max")
x <- ata.constraint(x, 1, 10, 10)
x <- ata.item.maxselect(x, 1)
x <- ata.solve(x)
y <- ata.get.items(x, as.list=TRUE)
mean(y[[1]]$b)
plot(irt.model.3pl(items=y[[1]]), stats="information", total=TRUE)
mean(y[[2]]$b)
plot(irt.model.3pl(items=y[[2]]), stats="information", total=TRUE)
# ex. 2: 2 forms, 10 items, minimize b parmaeters
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.relative(x, "b", "min", negative=TRUE)
x <- ata.solve(x)
y <- ata.get.items(x, as.list=TRUE)
mean(y[[1]]$b)
plot(irt.model.3pl(items=y[[1]]), stats="information", total=TRUE)
mean(y[[2]]$b)
plot(irt.model.3pl(items=y[[2]]), stats="information", total=TRUE)
# ex. 3: 2 forms, 10 items, maximize information at -0.5 and 0.5
# content distribution: 3, 3, 4; response time: avg. 55--65s
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.relative(x, c(-0.5, 0.5), "max")
x <- ata.constraint(x, "content", 3, 3, 1)
x <- ata.constraint(x, "content", 3, 3, 2)
x <- ata.constraint(x, "content", 4, 4, 3)
x <- ata.constraint(x, "time", 55*10, 65*10)
x <- ata.solve(x)
y <- ata.get.items(x, TRUE)
freq(y[[1]]$content, 1:3)$n
mean(y[[1]]$time)
plot(irt.model(items=y[[1]]), stats="information", total=TRUE)
freq(y[[2]]$content, 1:3)$n
mean(y[[2]]$time)
plot(irt.model(items=y[[2]]), stats="information", total=TRUE)
# ex. 4: 2 forms, 10 items, mean(b) = 0.5, sd(b) = 1.0, content = (3, 3, 4)
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.absolute(x, "b", 0.5 * 10)
x <- ata.obj.absolute(x, (x$pool$b - 0.5)^2, 1.0 * 10)
x <- ata.constraint(x, "content", 3, 3, 1)
x <- ata.constraint(x, "content", 3, 3, 2)
x <- ata.constraint(x, "content", 4, 4, 3)
x <- ata.solve(x)
y <- ata.get.items(x, TRUE)
c(mean(y[[1]]$b), sd(y[[1]]$b))
freq(y[[1]]$content, 1:3)$n
c(mean(y[[2]]$b), sd(y[[2]]$b))
freq(y[[2]]$content, 1:3)$n
# ex. 5: 2 forms, 10 items, flat TIF over [-1, 1]
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.relative(x, seq(-1, 1, .5), "max", negative=FALSE, flatten=.1)
x <- ata.solve(x)
y <- ata.get.items(x, TRUE)
plot(irt.model.3pl(items=y[[1]]), stats="information", total=TRUE)
plot(irt.model.3pl(items=y[[2]]), stats="information", total=TRUE)
