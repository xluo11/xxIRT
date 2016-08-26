# xxIRT: R Package for Item Response Theory
######Author: [Xiao Luo](mailto:xluo1986@gmail.com) || Last Edit: August 09, 2016 || Version: 1.0.2

######**Citation**: Luo, X. (2016). xxIRT: R Package for Item Response Theory (Version 2.0) [Software]. Retrieved from [https://github.com/xluo11/xxIRT](https://github.com/xluo11/xxIRT)


###Introduction
xxIRT is a R package to conduct item response theory (IRT) analysis and research. It comprises of five modules: (1) commons & utilities, (2) computerized adaptive testing (CAT), (3) estimation, (4) automated test assembly (ATA), and (5) multistage testing (MST).

This package is designed for both beginners and intermediate users to showcase IRT concepts and conduct research more efficiently. There are four GUIs (`catGUI()`, `estimationGUI()`, `ataGUI()`, `mstGUI()`) built on top of *Shiny* to give users a graphic interface to play round this package (with slightly reduced functionality). 

###Installation
In R console, call `install.packages("xxIRT")` to install the stable version of the **xxIRT** package from CRAN. Use command `devtools::install_github("xluo11/xxIRT")` to install the nightly build version from [github.com](https://github.com/xluo11/xxIRT) which incorporates new features and revisions in a timely fashion. To enable github installation, make sure the *devtools* package is installed (e.g., `install.packages("devtools")`). 

###Module I: Commons & Utiities
At the heart of this package is an **'irt'** object which contains **thetas** (a vector of ability parameters), **items** (a data.frame of item parameters), **rsp** (a optional response matrix). Use `irt(theta, a, b, c)` to create an 'irt' object with given values. To enjoy the laziness, use `gen.irt(n.peo, n.item)` to generate an 'irt' object with *n.peo* thetas and *n.item* items. Passing the IRT object to `gen.rsp(irt.object)` to return a 'irt' object with generate binary response matrix. 

Computing model-based correct response probability is a common practice in IRT. Use `prob(irt.object)` to compute a probability matrix with given *thetas* and *items* in the 'irt' object. The resulting matrix has *thetas* in rows and *items* in columns. Use `info(irt.object)` to compute information in the similar fashion. Use `likelihood(irt.object)` to compute likelihood. Notice that the likelihood calculation requires a response matrix. These three functions allow summarization of results, by passing `summary` (summary direction: 1 = by row, and 0 = by column) and `fun` (summary function) into the function.

`plot(irt.object)` draws item/test characteristic curves or item/test information functions, depending on the arguments `type` (information or probability) and `total` (total or individual values).

####Examples
```r
x <- gen.rsp(gen.irt(100, 20)) # generate an irt object
p <- prob(x) # compute probabilities
i <- info(x) # compute informations
l <- likelihood(x, summary=1, fun=sum, log=TRUE) # compute log-likelihood for people
plot(x, type="probability", total=FALSE) # draw ICCs
plot(x, type="information", total=TRUE) # draw TIF

x <- gen.rsp(gen.irt(1, 20)) # generate an irt object with one person
t <- round(seq(-3, 3, .1), 1) # thetas from -3 to 3
u.mat <- matrix(rep(x$rsp, length(t)), ncol=20, byrow=TRUE) # turn response vector to matrix
y <- with(x$items, irt(t, a, b, c, u.mat)) # log-likelihood
plot(t, likelihood(y, summary=1, fun=sum, log=TRUE), type="l", xlab=expression(theta), ylab="Log-likelihood") # visulization of maximum likelihood
```

###Module II: Estimation
Estimation of theta and item parameters from an observed response matrix is the most fun yet challenging task in IRT. Three estimators of theta parameters are provided in this module, which assumes *item parameters* are known or already calibrated. `estimate.theta.mle(u, a, b, c)` uses the maximum likelihood (ML) method, `estimate.theta.eap(u, a, b, c)` the expected a posteriori (EAP) method, and `estimate.theta.map(u, a, b, c)` uses the maximum a posteriori (MAP) method. 

Another three estimators of item parameters are provided too. If *thetas* are known, `estimate.item.jmle(u, theta)` calibrates the item parameters using the joint maximum likelihood (JML) method. If *thetas* are unknown, `estimate.item.mmle(u)` calibrates the item parameters using the marginal maximum likelihood (MML) method by integrating out *thetas* with an assumed distribution. Usually we assume *thetas* are from a standard normal distribution, and use Hermite-Gauss quadrature to approximate the integration of *thetas*. `estimate.item.bme(u)` brings this Bayesian techniques to not only *thetas* but also to item parameters. In the implementation, we assumes *a* is from a lognormal distribution, *b* from a normal distribution, and *c* from a beta distribution.

####Examples
```r
x <- gen.rsp(gen.irt(500, 50)) # generate data
color <- rgb(.8,.2,.2,.5) # drawing color

t <- with(x$items, estimate.theta.mle(x$rsp, a, b, c)) # ML estimation
rmse(x$thetas, t)
plot(x$thetas, t, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color, xlab="True", ylab="Estimate")
abline(0, 1, lty=2)

t <- with(x$items, estimate.theta.eap(x$rsp, a, b, c)) # EAP estimation
rmse(x$thetas, t)
plot(x$thetas, t, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color, xlab="True", ylab="Estimate")
abline(0, 1, lty=2)

t <- with(x$items, estimate.theta.map(x$rsp, a, b, c)) # MAP estimation
rmse(x$thetas, t)
plot(x$thetas, t, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color,  xlab="True", ylab="Estimate")
abline(0, 1, lty=2)


x <- gen.rsp(gen.irt(3000, 60, a.sig=.4)) # generate data

y <- estimate.item.jmle(x$rsp, x$thetas, model="3PL") # JML calibration
plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$a, y$parameters$a) # check a parameter
plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$b, y$parameters$b) # chekc b parameter
plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$c, y$parameters$c) # check c parameter

y <- estimate.item.mmle(x$rsp, model="3PL") # MMLE calibration
plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$a, y$parameters$a) # check a parameter
plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$b, y$parameters$b) # check b parameter
plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$c, y$parameters$c) # check c parameter

y <- estimate.item.bme(x$rsp, model="3PL", a.mu=0, a.sig=.4) # BME calibration
plot(x$items$a, y$parameters$a, xlim=c(0, 3), ylim=c(0, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$a, y$parameters$a) # check a parameters
plot(x$items$b, y$parameters$b, xlim=c(-3, 3), ylim=c(-3, 3), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$b, y$parameters$b) # check b parameters
plot(x$items$c, y$parameters$c, xlim=c(0, .5), ylim=c(0, .5), pch=16, col=color,  xlab="True", ylab="Estimate"); abline(0, 1, lty=2); rmse(x$items$c, y$parameters$c) # check c parameters
```

###Module III: CAT
Computerized adaptive testing (CAT) changes the landscape of educational testing with new technologies. In CAT, tests are administered dynamically and customized to fit each individual's characteristic in real time. Testing becomes, to some extent, a two-way communication as in an interview where the next question is based on the answer to the previous question. `cat.sim(theta, u, opts)` establishes a general framework to conduct CAT simulation, where users can pass in their own version of `cat.select` (item selection algorithm), `cat.estimate` (estimation method), and `cat.stop` (termination rule). Such flexible framework allows users to run CAT simulation with minimal modifications. 

Some out-of-box algorithms of those CAT components are of course provided. For instance, the default select method, `cat.select.default`, randomly selects an item from the *k* (k=1 by default) most informative items. Add an element `select.random` in `opts` passed to `cat.sim` to chang it. `cat.select.ccat` implements the contrained CAT item selection algorithm which handles content-balancing constraint. Remember to add an element `ccat.target` in `opts` to set targeted content distribution percentages, and add `ccat.random` in `opts` to add randomness in the first few selections.

The default estimation method, `cat.estimate.default`, uses `estimate.theta.eap` for a response vector of all 1s or all 0s and `estimate.theta.mle` otherwise. The default termination method, `cat.stop.default`, evalutes one of the three criteria after reaching the minimum length: (1) if the se threshold is reached when `stop.se` is found in `opts`; (2) if all items fail to provide minimum information when `stop.mi` is found in `opts`; (3) if a clear pass/fail decision can be made when `stop.cut` is found in `opts`.  

The `cat.sim(theta, u, opts)` runs the simulation and results in a **'cat'** object which contains list components like `admins` (administration history), `items` (item history), `stats` (response and theta history), `pool` (unused pool), `true` (true theta), `est` (estimated theta), `len` (test length). Use `print(cat.object)` and `plot(cat.object)` to print and plot the `cat` object.

####Examples
```r
pool <- gen.irt(1, 100)$items # generate item pool
pool$content <- sample(1:3, nrow(pool), replace=TRUE) # add content codes

opts <- list(min=10, max=30, stop.se=.3) # 10--30 items, stop when se below .3
x <- cat.sim(0.1, pool, opts) 
x
plot(x)

opts$select.random <- 10 # randomesque to control item exposure
x <- cat.sim(0.1, pool, opts)
x
plot(x)

opts$ccat.target <- c(.5, .3, .2) # content balancing
opts$random <- 5
x <- cat.sim(0.1, pool, opts, cat.select=cat.select.ccat)
x
plot(x)
freq(x$items$content, 1:3)
```

###Module IV: ATA
Automated test assembly (ATA) is a fairly useful technology in psychometrics, in which an advanced optimization algoirhtm is applied to select items from the item pool to optimize the objective function while satisfying a large quantity of complicated test constraints. The ATA module in this package is built upon the [lp_solve](http://lpsolve.sourceforge.net) library and [lpSolveAPI](https://cran.r-project.org/web/packages/lpSolveAPI/lpSolveAPI.pdf) package. 

Use `ata(pool, nform)` to initiate an **'ata'** object with given item pool and number of forms to assemble. Next, use either `ata.obj.rel(x, value, mode)` (maximize/minimize) or `ata.obj.asb(x, value, target)` (approach target values) to add objective functions. Both functions take in a *ata* object (the *x* argument) and returns an updated *ata* object. In `ata.obj.rel`, the *value* argument can be a variable name (e.g., `value="b"` to denote optimizing *b*-parameters), a vector of *theta* points at which information is optimized (e.g., `c(-0.5, 0, 0.5)` to denote optimizing information at thetas equal to -0.5, 0 and 0.5), a vector of values with length equal to the number of items (e.g., `abs(pool$b-1.0)` denote optimizing the absolute difference between *b*-paramter and 1.0). *mode* argument determines whether to *max* (maximize) or *min* (minimize) the problem. If the expected optimal value is negative, remember to pass in `negative.obj=TRUE`.

Use `ata.constraint(x, var, level, min, max)` to add constraints. When imposing a constraint on a categorical variable, use *var* to denote the variable name, and set *level* to be the level of the variable to be contrained. *min* and *max* are the minimum and maximum values. For instance, `ata.constraint(x, "b", 1, 5, 10)` means include 5--10 items from content area #1. When imposing a constraint on a quantitative variable, leave *level* to *NA* or *NULL*. A almost always necessary constraint is the test length--e.g, `ata.constraint(x, "len", NA, 10, 10)` setting the test length to be 10 items. 

Don't forget to limit the maximum selection for items by calling `ata.maxselect(x, maxselect)`. Usually one item is allowed to selet once. Use `ata.fixitem` to fix items to be select or not select. After all is done, call `ata.solve(x)` to solve the problem. If the problem is successfully solved, `rs.index` and `rs.items` should be added to the *'ata'* object.

####Examples
```r
pool <- gen.irt(1, 100)$items # generate item pool
pool$content <- sample(1:3, nrow(pool), replace=TRUE) # generate content codes
pool$time <- round(exp(rnorm(nrow(pool), log(20), .1))) # generate response time

x <- ata(pool, 2) # assemble 2 forms
x <- ata.maxselect(x, 1) # an item can be used at most once
x <- ata.constraint(x, "len", NA, 10, 10) # 10 items in each form
x <- ata.obj.rel(x, -0.5, "max") # maximize informaiton at theta=-0.5
x <- ata.obj.rel(x,  0.5, "max") # also maximize information at theta=0.5
x <- ata.solve(x) # solve
y1 <- x$rs.items[[1]] # extract form #1
y2 <- x$rs.items[[2]] # extract form #2
y1
y2
with(y1, plot(irt(c(-.5, .5), a, b, c), type="information")) # plot TIF of form #1
with(y2, plot(irt(c(-.5, .5), a, b, c), type="information")) # plot TIF of form #2

x <- ata(pool, 2) # assemble another 2 forms
x <- ata.maxselect(x, 1) # an item can be used at most once
x <- ata.constraint(x, "len", NA, 10, 10) # still, 10 items in each form
x <- ata.constraint(x, "content", 1, 0, 0) # no item from content area 1
x <- ata.obj.rel(x, "b", "max") # maximize b parameters, i.e., most difficult forms
x <- ata.solve(x) # solve
y1 <- x$rs.items[[1]] # extract form #1
y2 <- x$rs.items[[2]] # extract form #2
with(y1, plot(irt(0, a, b, c), type="information")) # plot TIF of form #1
with(y2, plot(irt(0, a, b, c), type="information")) # plot TIF of form #2

x <- ata(pool, 2) # assemble another 2 forms
x <- ata.maxselect(x, 1) # an item can be used at most once
x <- ata.constraint(x, "len", NA, 10, 10) # 10 items in each form
x <- ata.obj.abs(x, -0.5, 3.5) # make information at theta=-0.5 approach 3.5
x <- ata.obj.abs(x,  0.0, 3.5) # make information at theta=0.0 approach 3.5
x <- ata.obj.abs(x,  0.5, 3.5) # make information at theta=0.5 approach 3.5
x <- ata.solve(x) # solve
y1 <- x$rs.items[[1]] # extract form #1
y2 <- x$rs.items[[2]] # extract form #2
with(y1, plot(irt(c(-.5,0,.5),a,b,c), type="information")) # plot TIF of form #1
with(y2, plot(irt(c(-.5,0,.5),a,b,c), type="information")) # plot TIF of form #2

x <- ata(pool, 2) # assemble another 2 forms
x <- ata.maxselect(x, 1) # an item can be used at most once
x <- ata.constraint(x, "len", NA, 10, 10) # 10 items in each form
x <- ata.obj.abs(x, "b", 1.0 * 10) # mean of b parameters to be 1.0
x <- ata.obj.abs(x, (pool$b-1.0)^2, 1.0 * 10) # variance of b parameters to be 1.0 
x <- ata.solve(x) # solve
y1 <- x$rs.items[[1]] # extract form #1
mean(y1$b); sd(y1$b) # check mean and sd of b parameters in form #1
y2 <- x$rs.items[[2]] # extract from #2
mean(y2$b); sd(y2$b) # check mean and sd of b parameters in form #2
x$rs.items # show assemled items
with(y1, plot(irt(0, a, b, c), type="information")) # plot TIF of form #1
with(y2, plot(irt(0, a, b, c), type="information")) # plot TIF of form #2
```

###Module V: MST
Multistage testing (MST) is a mode of testing comprising of multiple testing stage and test adaptation between stages. Compared withe CAT, MST gives test developers more controls over the test quality and characteristis by allowing tests to be assembled in house prior to administration/publishing. Call `mst(pool, design, npanel)` to initiate a *'mst'* object. Multiple panels are typically assembled and randomly assigned to a test taker to ameliorate the risk of item overexposure. By default, one item is only selected once. Use `mst(mst.object, route, op)` to add/remove a route to/from the *'mst'* object. Some routes are forbid in order to avoid radical routing. 

Use `mst.objective(mst.object, thetas, targets, routes)` to add objectives for optimization on routes. *thetas* are theta points at which informations are optimized. *targets* are absolute values of targets or *Inf* to maximize information. Use `mst.constraint(mst.object, variable, level, min, max, routes)` to add constraints on routes. Leave *level* to *NA* or *NULL* for a quantitative variable. 

The top-down design approach is adopted in this module, meaning objectives and constraints are directly imposed on routes as opposed to being broken down to stages and modules. The bottom-up design approach is still possible by using the *ata* module, because it is essentially a multi-form assembly problem. 

In the top-down approach, don't forget to set minimum length in stage by calling `mst.stagelength(mst.object, stage, min, max)`; otherwise, it is very likely that *lpsolve* allocates all items in the final stage. After all is done, use `mst.assemble(mst.object)` to assemble items, which adds a *data.frame* of assembled items to the *'mst'* object, if the problem is solved successfully. 

Use `mst.summary(mst.object)` and `plot(mst.object)` to summarize and visualize an assembled *'mst'*. Passing a `by.route=TRUE` argument to the call to summarize and visualize results by routes; otherwise, by modules. `mst.get(mst.object, panel, module, route)` is a helper function to retrieve assembly results. Either *module* or *route* is set, not both. Also, use `mst.sim(mst.object, theta, routing, estimator)` to run a mst simulation.

When the assembly problem is too large to be solve simultaneously, try `mst.assemble.sequence(mst.object)` to assemble panels in sequence. Results will favor the panels assembled earlier than later. Try to run another round of simultaneous assembly  `mst.assemble` using a reduced item pool which include items in the the sequential assembly result as well as some additional items randomly selected from the remaining pool.

####Examples
```r
pool <- gen.irt(1, 200)$items # generate item pool
pool$content <- sample(1:3, nrow(pool), replace=TRUE) # generate content codes

x <- mst(pool, c(1, 2, 3), 2) # assebmle 2 panels of a 1-2-3 MST
x <- mst.route(x, c(1, 2, 6), "-", print=TRUE) # remove the route 1-2-6 (1M-2E-3H)
x <- mst.route(x, c(1, 3, 4), "-", print=TRUE) # remove the route 1-3-4 (1M-2H-3E)
x <- mst.objective(x, -1.0, Inf, routes=1) # maximize information at -1.0 for low-ability route (Route #1: 1M-2E-3E)
x <- mst.objective(x,  0.0, Inf, routes=2:3) # maximize information at 0.0 for mid-ability routes (Route #2: 1M-2E-3M and Route #3: 1M-2H-3M)
x <- mst.objective(x,  1.0, Inf, routes=4) # maximize information at 1.0 for high-ability route (Route #4: 1M-2H-3H)
x <- mst.constraint(x, "len", NA, 10, 10) # 10 items in each route
x <- mst.constraint(x, "content", 1, 3, 3) # 3 items from content area 1 in each route
x <- mst.stagelength(x, 1, 2, 10) # at least 2 items in stage 1
x <- mst.stagelength(x, 2, 2, 10) # at least 2 items in stage 2
x <- mst.stagelength(x, 3, 2, 10) # at least 2 items in stage 3
x <- mst.assemble(x) # assemble

# check content
for(p in 1:x$npanel){
 for(r in 1:x$nroute){
   cat("Panel #", p, " Route #", r, ", Content Area 1 has ",
   freq(mst.get(x, panel=p, route=r)$content, 1:3)$n[1], " items.\n", sep="")
 }
}

# check TIFs
plot(x, by.route=TRUE)

# run simulation
mst.sim(x,  1.5)
mst.sim(x,  0.5)
mst.sim(x, -0.5)
mst.sim(x, -1.5)

# Solve a big problem in sequence
rm("x")
pool <- gen.irt(1, 1000)$items
pool$content <- sample(1:3, nrow(pool), replace=TRUE)
x <- mst(pool, c(1, 2, 3), 3)
x <- mst.route(x, c(1, 2, 6), "-")
x <- mst.route(x, c(1, 3, 4), "-")
x <- mst.objective(x, -1.0, Inf, routes=1)
x <- mst.objective(x,  0.0, Inf, routes=2:3)
x <- mst.objective(x,  1.0, Inf, routes=4)
x <- mst.constraint(x, "len", NA, 10, 10)
x <- mst.constraint(x, "content", 1, 3, 3)
x <- mst.stagelength(x, 1, 2, 10)
x <- mst.stagelength(x, 2, 2, 10)
x <- mst.stagelength(x, 3, 2, 10)
# unlikely to be solved simulatanouesly within 1 minute
# invisible(mst.assemble(x, timeout=60))
# may be solved in sequence within 1 minute
x <- mst.assemble.sequence(x, timeout=60)
plot(x, by.route=TRUE)
```

###Ending
This is an early version of an ongoing project. Please leave comments, suggestions, questions to [the author](mailto:xluo1986@gmail.com).
