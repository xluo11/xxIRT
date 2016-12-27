# generate an item pool
pool <- irt.model()$gen.data(1,2)$items
pool$content <- sample(1:2, 2)
pool$time <- round(rnorm(2, 60, 2))
x <- mst(pool, design=c(1,2,3), npanel=2, "topdown")

# remove a route using route vector
x$route
mst.route(x, c(1,2,6), "-")$route
# remove a route using route index
mst.route(x, 3, "-")$route
# add a route using route vector
x <- mst.route(x, c(1,2,6), "-")
mst.route(x, c(1,2,6), "+")$route


# add objective for topdown method
x <- mst(pool, design=c(1,2), npanel=1, method="topdown")
x <- mst.objective(x, theta=c(-1, 0.5), indices=1)
x <- mst.objective(x, theta=c(-0.5, 1), indices=2)
x$assembler
# add objective for bottomup method
x <- mst(pool, design=c(1,2), npanel=1, method="bottomup")
x <- mst.objective(x, theta=c(-1, 0.5), indices=1)
x <- mst.objective(x, theta=c(-0.5, 0.5), indices=2)
x <- mst.objective(x, theta=c(-0.5, 1), indices=3)
x$assembler
# add objective with flatten parameter
x <- mst(pool, design=c(1,2), npanel=1, method="topdown")
x <- mst.objective(x, theta=c(-1, 0.5), indices=1, flatten=0.1)
x <- mst.objective(x, theta=c(-0.5, 1), indices=2, flatten=0.1)
x$assembler
# add objective with target value
x <- mst(pool, design=c(1,2), npanel=1, method="bottomup")
x <- mst.objective(x, theta=c(-1, 0.5), indices=1, target=8)
x <- mst.objective(x, theta=c(-0.5, 0.5), indices=2, target=10)
x <- mst.objective(x, theta=c(-0.5, 1), indices=3, target=8)
x$assembler

# add constraint using a constant (test length)
x <- mst(pool, design=c(1,2), npanel=1, method="topdown")
x <- mst.constraint(x, 1, min=5, max=5, indices=1:2)
x$assembler
# add a categorical constraint
x <- mst(pool, design=c(1,2), npanel=1, method="topdown")
x <- mst.constraint(x, "content", min=5, max=5, level=1, indices=1:2)
x$assembler
# add a quantitative constraint
x <- mst(pool, design=c(1,2), npanel=1, method="bottomup")
x <- mst.constraint(x, "time", min=60, max=60, indices=1:3)
x$assembler

# add stage length
x <- mst(pool, design=c(1,2), npanel=1, method="bottomup")
x <- mst.stage.length(x, c(1,2), min=c(5,5), max=c(10,10))
x$assembler

# ex. 1: 1-2-2 MST, 2 panels, topdown, 20 items, content = c(10, 5, 5)
# maximize information at -1 for easy routes and 1 for hard routes
pool <- irt.model()$gen.data(1,300)$items
pool$content <- sample(1:3, nrow(pool), replace=TRUE)
pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))
x <- mst(pool, design=c(1,2,2), npanel=2, method='topdown')
x$route
x <- mst.objective(x, theta=-1, indices=1:2)
x <- mst.objective(x, theta=1, indices=3:4)
x <- mst.constraint(x, 1, 20, 20)
x <- mst.constraint(x, "content", 10, 10, level=1)
x <- mst.constraint(x, "content", 5, 5, level=2)
x <- mst.constraint(x, "content", 5, 5, level=3)
x <- mst.stage.length(x, c(1,2,3), min=5)
x <- mst.assemble(x)
freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
plot(x)
plot(x, byroute=TRUE)
mst.sim(x, 1)
mst.sim(x, -1)
# ex. 2: 1-2-2 MST, 2 panels, bottomup, 10 items per stage, content = c(4, 3, 3)
# maximize information at -1 for easy modules, 0 for medium modules and 1 for hard modules
x <- mst(pool, design=c(1,2,2), npanel=2, method='bottomup')
x$module
x <- mst.objective(x, theta=0, indices=1)
x <- mst.objective(x, theta=-1, indices=c(2,4))
x <- mst.objective(x, theta=1, indices=c(3,5))
x <- mst.constraint(x, 1, 10, 10)
x <- mst.constraint(x, "content", 4, 4, level=1)
x <- mst.constraint(x, "content", 3, 3, level=2)
x <- mst.constraint(x, "content", 3, 3, level=3)
x <- mst.assemble(x)
freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
plot(x)
plot(x, byroute=TRUE)
mst.sim(x, 1)
mst.sim(x, -1)
# ex. 3: 1-2-3 MST, 2 panels, topdown, 30 items, content = c(10, 10, 10), prohibit routes with radical changes
# maximize information at -1 for easy, 0 for medium and 1 for hard routes
x <- mst(pool, design=c(1,2,3), npanel=2, method='topdown')
x <- mst.route(x, c(1,2,6), "-")
x <- mst.route(x, c(1,3,4), "-")
x$route
x <- mst.objective(x, -1, indices=1)
x <- mst.objective(x,  0, indices=2:3)
x <- mst.objective(x,  1, indices=4)
x <- mst.constraint(x, 1, 30, 30)
x <- mst.constraint(x, "content", 10, 10, level=1)
x <- mst.constraint(x, "content", 10, 10, level=2)
x <- mst.constraint(x, "content", 10, 10, level=3)
x <- mst.stage.length(x, c(1,2,3), min=3)
x <- mst.assemble(x, timeout=5*60)
freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
plot(x)
plot(x, byroute=TRUE)
mst.sim(x, 1)
mst.sim(x, .2)
mst.sim(x, -.2)
mst.sim(x, -1)
# ex. 3: 1-2-3 MST, 2 panels, bottomup, 10 items per stage, content = c(4, 3, 3), prohibit routes with radical changes
# target information at 6 (theta=-1) for easy, 6 (theta=0) for medium and 6 (theta=1) for hard modules
x <- mst(pool, design=c(1,2,3), npanel=2, method='bottomup')
x <- mst.route(x, c(1,2,6), "-")
x <- mst.route(x, c(1,3,4), "-")
x$module
x <- mst.objective(x, -1, indices=c(2,4), target=6)
x <- mst.objective(x,  0, indices=c(1,5), target=6)
x <- mst.objective(x,  1, indices=c(3,6), target=6)
x <- mst.constraint(x, 1, 10, 10)
x <- mst.constraint(x, "content", 4, 4, level=1)
x <- mst.constraint(x, "content", 3, 3, level=2)
x <- mst.constraint(x, "content", 3, 3, level=3)
x <- mst.stage.length(x, c(1,2,3), min=3)
x <- mst.assemble(x, timeout=5*60)
freq(mst.get.items(x, panel=1,route=1)$content, 1:3)$n
freq(mst.get.items(x, panel=2,route=4)$content, 1:3)$n
plot(x)
plot(x, byroute=TRUE)
mst.sim(x, 1)
mst.sim(x, .2)
mst.sim(x, -.2)
mst.sim(x, -1)




