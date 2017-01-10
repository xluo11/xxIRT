# xxIRT: Item Response Theory and Computer-based Testing in R
###### Author: [Xiao Luo](mailto:xluo1986@gmail.com) || Last Edit: December 31, 2016 || Version: 2.0.1

###### **Citation**: Luo, X. (2016). xxIRT: Item Response Theory and Computer-based Testing in R (Version 2.0) [Software]. Retrieved from [https://github.com/xluo11/xxIRT](https://github.com/xluo11/xxIRT)

### Introduction
xxIRT is a R package that intends to bring latest advancements in psychometric research, especially in the areas of item response theory and computer-based testing, into practice. To make new findings, methods, and techniques in psychometric research be more accessible to practitioners allow them to be tested, refined, and eventually utilized to enhance operational work and applied research. In short, this is a R package developed by psychometric practitioners for practitioners to bridge the gap between psychometric research and practice. 

In current version, this package consists of five modules: (1) item response theory (IRT), (2) parameter estimation, (3) automated test assembly (ATA), (4) computerized adaptive testing (CAT), and (5) computerized multistage testing (MST). The application programming interface, or API, is for intermediate R users who are comfortable with the R programming language, whereas the graphic user interface, or GUI, is for novice users who are not necessarily familiar with R. GUIs are more user-friendly and include most, but not all, of the key functionalities available in the API. 

### Installation
In R console, use the command `install.packages("xxIRT")` to install the stable version from CRAN. Use the command `devtools::install_github("xluo11/xxIRT")` to install the latest version from my [github.com](https://github.com/xluo11/xxIRT) page, and make sure the *devtool* package has already been installed beforehand (i.e., `install.packages("devtools")`). The github version would be frequently updated to add new features and revisions. When it is deemed a significant upgrade from the previous version, it would be submitted to CRAN. To remove the installed package, call `remove.packages("xxIRT")`. This command is rarely used, because the package is so precious that you always want to retain a copy of it in your system even though you won't use it :-D

### Module I: Item Response Theory (IRT)
Item response theory (IRT) parameterizes the characteristics of test takers and testing items, and use these parameters to build statistical models that describes the mechanisms underlying observed responses. IRT includes a wide variety of models, and it is commonly used in practice to develop modern assessment instruments. This module includes common IRT computations and graphing, and it is a foundation for other more advanced IRT applications in this package. At the heart of this module is the `irt.model` object which contains the name of the model (*string*), people parameters (*data.frame*), items parameters (*data.frame*) and responses (*data.frame*). In addition, the object contains the functions for computing probability, information and likelihood and the function for generating data from the model. The probability is the model-based probability of getting a correct response given the people and item parameters. The information indicates the conference or error associated with the estimated people parameters. And the likelihood is the probability of the observed responses. Currently, only the **3pl** model has been implemented. Polytomous models is the next step.

To create a *3pl* object, call `irt.model.3pl` and pass people and item parameters and responses. All arguments are allowed to be `NULL` if they are unknown. If required parameters are `NULL`, some computation may be unavailable. Users can also call `irt.model.3pl()$gen.data` to generate 3PL data. The people parameters will be drawn from a normal distribution with mean (`theta.mu`) and SD (`theta.sig`) equal to 0 and 1 by default. The a-parameters will be drawn from a log-normal distribution with log-mean (`a.mu`) and log-SD (`a.sig`) equal to 0 and .2 by default, the b-parameters from a log-normal distribution with mean (`b.mu`) and SD (`b.sig`) equal to 0 and 1 by default, and the c-parameters from a beta distribution with alpha (`c.alpha`) and beta (`c.beta`) equal to 5 and 42. Change the above parameters to draw parameters from a user-defined distribution. Users can also pass people and/or item parameters to fix those parameters.

After creating a `3pl` object, call `probability`, `information`, and `likelihood` functions to compute those statistics which return a matrix of resulting values. Call `plot` to draw graphs. Use the `stats` argument to control which statistic to draw and use `total` argument to draw test or item level statistic. For example, `stats='probability', total=FALSE` draws multiple item characteristic curves, where `stats='probability', total=TRUE` draws a test character curve. When drawing `likelihood` or `loglikelihood`, make sure the object has only one person's data.

##### Code Sample
```r
# create a 3pl model with manual inputs
people <- data.frame(theta=c(-1,0,1))
items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), 
item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
x <- irt.model.3pl(people, items, responses) 
# compute probability, information and likelihood
x$probability(x)
x$information(x)
x$likelihood(x)
# create a 3pl model with generated data
x <- irt.model.3pl()$gen.data(10, 5)
x$probability(x)
x$information(x)
x$likelihood(x)
```

Some wrapper functions are provide to establish a common interface for all `irt.model` objects. For example, call `irt.model` function and use `model` argument to create a specific new `irt.model` object.  Call `irt.stats` function and use `stats`  argument to compute statistics. Also, use `summary` and `fun` arguments to summarize results for people or items if needed. In addition, call `irt.select` to subset data, and call `irt.sample` to sample from data. The generic methods `print` and `plot` are overridden for `irt.model` objects too.

In addition, several helper and utility functions are developed to simplify coding. For example, the function `irt.model.3pl.init` is a helper function for creating a 3pl object from vectors instead of data frames. The function `irt.model.rescale.3pl` is a helper function for rescaling parameters of a 3pl object. The function `freq` is a utility function for computing frequencies in given categories, and `rmse` is a utility function for computing root mean square errors.

##### Code Samples
```r
# create a 3PL model with manual inputs
people <- data.frame(theta=c(-1,0,1))
items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
x <- irt.model(people, items, responses, "3PL")
x
# create a 3PL model with generated data
x <- irt.model(model="3pl")$gen.data(20, 5)
x
# Generate Rasch items
irt.model(model="3pl")$gen.data(20, 5, a.mu=0, a.sig=0, c.alpha=0, c.beta=0)
# Generate an 3PL item pool
irt.model(model="3pl")$gen.data(1, 100)$items
# Generate responses using given people and item parameters
irt.model(model="3pl")$gen.data(people=people, items=items)$responses
# create 3pl model using parameter vectors
irt.model.3pl.init(people$theta, items$a, items$b, items$c, responses)
irt.model.3pl.init(people$theta, .58, items$b, 0, NULL)
 
# compute probability, information, likelihood and loglikelihood
x <- irt.model(model="3pl")$gen.data(20, 5)
irt.stats(x, "probability")
irt.stats(x, "probability", "people", sum)
irt.stats(x, "information")
irt.stats(x, "information", "items", sum)
irt.stats(x, "likelihood")
irt.stats(x, "likelihood", "people", prod)
log(irt.stats(x, "likelihood", "people", prod))
irt.stats(x, "loglikelihood", "people", sum)
# plot probability, information and log-likelihood
x <- irt.model(model="3pl")$gen.data(20, 5)
plot(x, stats="probability")
plot(x, stats="probability", total=FALSE)
plot(x, stats="information")
plot(x, stats="information", total=FALSE)
plot(irt.sample(x, n.people=1), stats="loglikelihood")

# subset from an IRT model
irt.select(x, people.index=c(1,3,5))
irt.select(x, items.index=c(1,3,5))
# sample wihtout replacement from an IRT model
irt.sample(x, n.people=3)
irt.sample(x, n.items=3)
# sample with replacement from an IRT model
irt.sample(x, n.people=30)
irt.sample(x, n.items=30)
# rescale theta
x <- irt.model(model="3pl")$gen.data(5,3)
c(mean(x$people$theta), sd(x$people$theta))
y <- irt.rescale.3pl(x, "theta", 0, 1)
c(mean(y$people$theta), sd(y$people$theta))
irt.stats(x, "probability")
irt.stats(y, "probability")
# rescale b
x <- irt.model(model="3pl")$gen.data(5,3)
c(mean(x$items$b), sd(x$items$b))
y <- irt.rescale.3pl(x, "b", 0, 1)
c(mean(y$items$b), sd(y$items$b))
irt.stats(x, "probability")
irt.stats(y, "probability")
```

### Module II: Parameter Estimation
People and item parameters are main ingredients in IRT analyses and applications. However, they are usually unknown and have to be estimated in practice. This modules provides an interface for estimating people and items parameters using a variety of methods for the 3PL model. 

When people parameters are estimaTed with known item parameters, users can choose to use the maximum likelihood estimation (MLE; `estimate.people.3pl.mle`), maximum a posteriori (MAP; `estimate.people.3pl.map`), or expected a posteriori (EAP; `estimate.people.3pl.eap`) methods. A wrapper function `estimate.people` is created to provide a common interface. Except for the EAP method, the debugging mode is on when `debug=TRUE` which draws the convergence progress over iterations.

##### Code Samples
```r
# data generation
data <- irt.model(model="3pl")$gen.data(500, 50)
# MLE
x <- estimate.people(data$responses, data$items, "3pl", "mle", debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
# MAP
x <- estimate.people(data$responses, data$items, "3pl", "map", debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
# EAP
x <- estimate.people(data$responses, data$items, "3pl", "eap")
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
```

When item parameters are estimated with known people parameters, users can choose to use the joint maximum likelihood estimation (JMLE; `estimate.items.3pl.jmle`), marginal maximum likelihood estimation (MMLE; `estimate.items.3pl.jmle`), and bayesian (`estimate.items.3pl.bme`) methods. All functions takes the `fix` argument, which is a list of fixed parameters. For instance, set `fix=list(c=0)` to calibrate a 2PL model, and set `fix=list(a=.5882, c=0)` to calibrate a Rasch model.
A wrapper function `estimate.items` is created to provide a common interface. When the debugging mode is on (`debug=TRUE`), it draws the convergence progress and returns diagnosis data.

##### Code Samples
```r
# data generation
data <- irt.model(model="3pl")$gen.data(2000, 50)
# JMLE
x <- estimate.items(data$responses, model="3pl", method="jmle", people=data$people, debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
# MMLE
x <- estimate.items(data$responses, model="3pl", method="mmle", debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
# BME
x <- estimate.items(data$responses, model="3pl", method="bme", debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0, 2), ylim=c(0, 2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4, 4), ylim=c(-4, 4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0, .3), ylim=c(0, .3), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
```

There are times that none of people or item parameters is available. In this case, the `estimate.3pl`  function calibrates item parameters using MMLE first and estimates people parameters using MLE afterwards. 


### Module III: Automated Test Assembly (ATA)
Test assembly is very common in practice, in which items are selected from the item pool to construct forms that meets certain criteria and conditions. Apparently it is astronomically daunting job to manually assemble tests to meet a number of complicated constraints from a large item pool. Automated test assembly is a technique that applies modern computing technology and advanced mathematical optimization algorithm to assemble test. Technically, the goal of ATA is to optimize the objective functions and be subject to constraints at the same time. Objective functions can be set in the relative (i.e., maximization or minimization) or absolute (i.e., targeting a value) sense. This module provides an interface for automated test assembly. It is built upon the open-source mixed integer linear programming solver *[lp_solve](http://lpsolve.sourceforge.net)* and its R interface package *[lpSolveAPI](https://CRAN.R-project.org/package=lpSolveAPI)*.  

To start off, use `ata` to create an *ata* object and set up a mixed integer linear programming (MILP) object for assembling *k* forms from a *n*-item pool. Afterwards, use `ata.obj.relative` to add a relative objective function or `ata.obj.absolute` to add an absolute objective function. The `ata.obj.relative` requires a `coef` argument (a variable name, a vector of theta points or a vector of *n* given values) as well as a `mode` argument (`max` for maximization and `min` for minimization). If the expected value of the objective function is a negative value, then set `negative=TRUE`. If it aims to have a flat test information function (TIF) over theta points in the objective functions, use the `flatten` argument to repress the peak. The `ata.obj.absolute` requires a `target` argument which is the targeted sum of the objective function. Both functions takes a `compensate` argument. When `compensate=TRUE`, it means coefficients are compensatory across objectives. For example, when `coef=c(-0.5, 0.5)` and `compensate=TRUE`, it means optimize the summed information at -0.5 and 0.5 altogether as opposed to independently.  Other common arguments are `forms` and `collapse`. When `forms=NULL`, objective functions are set for all forms; otherwise set `forms` to a vector of form indices. When `collapse=TRUE`, the objective functions are collapsed across forms into one objective function. 

Use `ata.constraint` to add constraints. This function requires a `coef` argument as well which can be a constant, a vector of *n* given values, or a variable name. The `level` argument is needed when the constraint is based on a categorial variable (e.g., content area). When `level=NULL` or `level=NA`, the function assumes the constraint is on a quantitative variable (e.g., response time). Use the `min` and/or `max` argument to set the lower- and/or upper-bound of the constraint. This function also takes the `forms` and `collapse` arguments.

In addition, use `ata.item.maxselect` to set maximum times of selection for items, use `ata.item.enemy` to set enemy relationship between items, and `ata.item.fix` to fix values for items.

After setting up all objectives and constraints, use `ata.solve` to solve the MILP object inside the *ata* object. If the MILP object is solved successfully, a binary matrix named `result` will be added to the *ata* object. The rows of the matrix represent items and columns forms and the binary value indicates whether the item is assembled into the form or not.
Use `ata.get.items` to extract selected items from a solved `ata` object which is expected to be more user-friendly. The generic method `plot` is overridden to draw TIFs of assembled forms.

##### Code Samples
```r
# ex. 1: 2 forms, 10 items, maximize b parmaters
items <- irt.model(model="3pl")$gen.data(1, 100)$items
items$content <- sample(1:3, nrow(items), replace=TRUE)
items$time <- round(rlnorm(nrow(items), log(60), .2), 0)
x <- ata(items, 2, debug=TRUE)
x <- ata.obj.relative(x, "b", "max")
x <- ata.constraint(x, 1, 10, 10)
x <- ata.item.maxselect(x, 1)
x <- ata.solve(x)
plot(x)
y <- ata.get.items(x, as.list=TRUE)
mean(y[[1]]$b)
mean(y[[2]]$b)
# ex. 2: 2 forms, 10 items, minimize b parmaeters
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.relative(x, "b", "min", negative=TRUE)
x <- ata.solve(x)
plot(x)
y <- ata.get.items(x, as.list=TRUE)
mean(y[[1]]$b)
mean(y[[2]]$b)
# ex. 3: 2 forms, 10 items, maximize information at -0.5 and 0.5
# content distribution: 3, 3, 4; response time: avg. 55--65s
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.relative(x, c(-0.5, 0.5), "max")
x <- ata.constraint(x, "content", 3, 3, 1)
x <- ata.constraint(x, "content", 3, 3, 2)
x <- ata.constraint(x, "content", 4, 4, 3)
x <- ata.constraint(x, "time", 55*10, 65*10)
x <- ata.solve(x)
plot(x)
y <- ata.get.items(x, TRUE)
freq(y[[1]]$content, 1:3)$n
mean(y[[1]]$time)
freq(y[[2]]$content, 1:3)$n
mean(y[[2]]$time)
# ex. 4: 2 forms, 10 items, mean(b) = 0.5, sd(b) = 1.0, content = (3, 3, 4)
x <- ata(items, 2, len=10, maxselect=1, debug=TRUE)
x <- ata.obj.absolute(x, "b", 0.5 * 10)
x <- ata.obj.absolute(x, (x$pool$b - 0.5)^2, 1.0 * 10)
x <- ata.constraint(x, "content", 3, 3, 1)
x <- ata.constraint(x, "content", 3, 3, 2)
x <- ata.constraint(x, "content", 4, 4, 3)
x <- ata.solve(x)
plot(x)
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
```

### Module IV: Computerized Adaptive Testing (CAT)
CAT is an innovative application of IRT on top of modern computing technology. CAT takes advantage of immense computing power in modern computers to customize the test for the test taker in the real time. The adaptivity in CAT reduces the administration of non-informative items so that the test length can be shorted without scarifying the measurement quality. 

A working CAT algorithm consists of three imperative components: the selection rule, the estimation rule, and the termination rule. The selection rule delineates how to select the next item given the status quo of the test taker. The estimation rule is a method of ability estimation. And the termination rule determines whether it is appropriate to exit the repetitive "seletion-estimation" cycles and terminate the test. A CAT research is usually about inventing a new rule within the general "selection-estimation-termination" workflow. Therefore, this module provides a framework for CAT simulation using user-defined rules. Some common rules are provided in the package and are readily available to use. If the user wants to try a new rule, only that particular rule needs to be written and passed into the `cat.sim` function. This approach allows users t write minimal code to get a CAT simulation up and running.

The default selection rule (`cat.select.default`) selects the most informative item for the next administration. To add some randomness to appease the item overexposure, set a `options$randomesque` value to randomly select an item from the *k* most informative ones. The constrained-CAT selection rule (`cat.select.ccat`) selects item under the content-balancing constraint. It first identifies the content domain with largest discrepancy from the target (`options$ccat.target`) and then select the most informative item from that content domain. Set a `options$cat.random` value to add some randomness to initial content domain sequence. The shadow test selection rule (`cat.select.shadow`) implements the shadow test algorithm. Set `options$shadow.constraints` to a data frame of constraints with four columns: `name` (variable name), `level` (`NA` for a quantitative variable), `min` and `max`.

The default estimation rule (`cat.estimate.default`) applies the EAP method to estimate the test taker's ability when the responses are all correct or all incorrect; otherwise, applies the MLE method.

The default stopping rule (`cat.stop.default`) is a trifold rule. Before the test reaches the minimum length (`options$min`), it never stops the test, and when the test reaches the maximum length (`options$max`) it always stops the test. Between the minimum and maximum length, it stops the test: (1) when the SE threshold (`options$stop.se`) is reached; (2) when none of items in the pool exceeds the MI threshold (`options$stop.mi`); or (3) when a clear pass/fail decision is available against the cut score (`options$stop.cut`). The projection-based stopping rule (`cat.stop.projection`) stops the test when the projected ability range under the constraints (`optioins$projection.constraints`) is clearly above or below the cut score (`options$projection.cut`) using the given projection method (`options$projection.method`).

When `debug=TRUE` is set in `cat.sim`, debugging information will be printed on the console to help troubleshooting and understand the internal process of the simulation.

Once a simulation is complete, it returns a `cat` object including a data frame of unused items (`pool`), a data frame of used items (`items`), responses and theta history (`stats`), administration history (`admin`), test length (`len`), true theta (`true`), and estimated theta (`eat`). The generic method `plot` is overridden to draw the simulation result.

##### Code Samples
```r
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
opts <- list(min=10, max=30, stop.se=.3, randomesque=5)
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
# cat simulation using the projection-based stopping rule
cons <- data.frame(name="content", level=1, min=10, max=10, stringsAsFactors=FALSE)
cons <- rbind(cons, c("content", 2, 10, 10))
cons <- rbind(cons, c("content", 3, 10, 10))
opts <- list(min=10, max=30, projection.cut=0, projection.constraints=cons,
projection.method="information")
x <- cat.sim(0.1, pool, opts, cat.stop=cat.stop.projection, debug=TRUE)
```

### Module V: Multistage Testing (MST)
MST is another adaptive testing model that has recently drawn a great deal of attention from practitioners. A MST consists of multiple testing phases, and in each stage a test taker will be administered with a pre-assembled *module*. The adaptivity takes place between stages and route the test taker to the module that best matches the test taker's ability in the next stage. When modules are connected with certain routing rules across stages, they constitute a *panel*. A common practice is to have multiple parallel and interchangeable panels in place for administration. One panel will randomly selected for administration in order to control the item exposure. With reduced adaptivity, it is difficult for MST to surmount CAT in terms of measurement quality. However, all modules are assembled in house prior to administration, and thus MST allows the preview and quality control opportunities to inspect both content and psychometric properties before the tests are published. This module provides an interface for designing, assembling and simulating a MST.

To start off, call `mst` to initiate a *mst* object and set up an *MILP* object for assembling *k* panels (the `npanel` argument) using the given item pool (the `pool` argument). The `mst` function requires the `design` and `method` arguments. The design is a numeric vector with each element representing the number of modules in that stage. For example, `c(1,2,3)` represents a 1-2-3 MST design (3 stages, 6 modules) and `c(1, 4)` represents a 1-4 MST design (2 stages, 5 modules). Internally, the modules will be indexed sequentially from the first to the last stage. A module-index map is constructed to illustrate the correspondence between index and stage-module position. A route map is also constructed to show all permissible routes. Use `mst.route` to remove an existing route or add a new route. In practice, routes with radical changes are oftentimes prohibited. 

The method can be `topdown` or `bottomup`. With the top-down approach, objectives and constraints are imposed on routes, and with the bottom-up, objectives and constraints are imposed on modules. For the top-down method, test-level objectives and constraints are set directly, whereas for the bottom-up method, objectives and constraints have to be broken down into stages. Use `mst.objective` to set TIF objectives, and use `mst.constraint` to impose constraints. Both functions takes a `indices` argument. It is a vector of module indices for the bottom-up design and route indices for the top-down design method. For the top-down method, an additional constraint is needed to regulate the minimum length in stages; otherwise the ATA is likely to cumulate all items into the final stage to achieve the optimum. 

After setting up all objectives and constraints, use `mst.assemble` to assemble items into panels, and results are added the object in the name of `items`. Use `mst.get.items` to extract items for a panel, module or a route. The function `mst.sim` simulate the administration of the assembled MST for the given `true.theta`. The generic method `plot` is overridden to draw TIFs of modules. When the argument `byroute=FALSE`, the plot is in a panel-by-stage grid. When the argument `byroute=TRUE`, the plot draws TIFs of each route in each panel. 

##### Code Samples 
```r
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
# ex. 3: 1-2-3 MST, 2 panels, topdown, 30 items,
# content = c(10, 10, 10), prohibit routes with radical changes
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
# ex. 4: 1-2-3 MST, 2 panels, bottomup, 10 items per stage,
# content = c(4, 3, 3), prohibit routes with radical changes
# target information at 6 (theta=-1) for easy, 6 (theta=0) for medium
# and 6 (theta=1) for hard modules
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
```
### Module 0: Graphic User Interfaces (GUIs)
Call `gui.irt()` to launch a shiny app for generating data and computing and drawing IRT statistics. Users can choose to generate parameters and data  or import from file. When importing people parameters, make sure it is a csv file with the proper column name (e.g., *theta* for 3PL model). When importing item parameters, make sure it is a csv file with proper column names (e.g., *a*, *b*, *c* for 3PL model). To download files, select the data and hit the "Download" button.

Call `gui.estimation()` to launch a shiny app for estimating parameters. After uploading a response file in cvs format, users can choose to import or estimate people and item parameters. When importing people parameter file, make sure it is a cvs file and has a column named *theta*. When estimating people parameters, choose an estimation method. When importing item parameters, make sure it is a csv file and has three columns named *a*, *b*, and *c*. When estimating item parameters, choose an estimation method. Click "Download" button in the *people* and *items* tabs to download people or item parameters.

Call `gui.ata()` to launch a shiny app for ATA. First, upload an item pool file. Make sure it is a cvs file and has three columns named *a*, *b*, and *c*. When add an objective, make sure the *coefficient* is variable name or a theta point. When add a constraint, leave the *level* empty if the variable is a quantitative variable. The app will crash if the user try to add a wrong objective or constraint (e.g., wrong variable name, wrong minimum/maximum value). Click "Download" button in the *Results* tab to download assembly results.

Call `gui.cat()` to launch a shiny app for CAT simulation. First, upload an item pool file. Make sure it is a cvs file and has three columns named *a*, *b*, and *c*. Next, choose to run a single simulation or a batch simulation. For a single simulation, enter the true theta directly. For a batch simulation, upload a people parameter file. Afterwards, select the appropriate selection, estimation and termination rules and click "Run" button to run the simulation. In the *Results* tab, use arrows above the table to navigate among simulations. If it is a batch simulation, two additional graphs will be provided in the *Summary* tab to illustrate the relationship between true and estimated thetas and the relationship between true theta and test length.

Call `gui.mst()` to launch a shiny app for MST simulation. First, upload an item pool file. Make sure it is a cvs file and has three columns named *a*, *b*, and *c*. The route map, the module map, and the ATA object will be shown in the *console* tab. Use *Route Management* to add or remove allowable routes in the MST. Make sure type in the correct route index connected by hyphen (e.g., 1-2-4). When setting objectives, leave the *target* empty if it is to maximize the objective. When setting constraints, leave the *level* empty if it is a quantitative variable. Objectives and constraints will be printed for monitoring in the *console* tab. Don't forget to test length constraint and the stage length constraint (for the top-down method). Click "Assemble" button to assemble MST. Go to *Simulation* tab to run a simulation using the assembled MST.

### Ending
This is an early version of an ongoing project. Please send your comments, questions, feature request and bug reporting to the author [Xiao Luo](mailto:xluo1986@gmail.com).
