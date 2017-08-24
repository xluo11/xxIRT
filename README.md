xxIRT: Practical Item Response Theory and Computer-Based Testing in R
================
Xiao Luo
24 August 2017

### Table of Contents

-   [Installation](#installation)
-   [Introduction](#introduction)
-   [Package Modules](#package-modules)
    -   [IRT Models](#irt-models)
    -   [IRT Utils](#irt-utils)
    -   [Parameter Estimation](#parameter-estimation)
    -   [Automated Test Assembly](#automated-test-assembly)
    -   [Computerized Adaptive Testing](#computerized-adaptive-testing)
    -   [Multistage Testing](#multistage-testing)
-   [Graphical User Interfaces](#graphical-user-interfaces)
-   [Ending](#ending)

### Installation

To install a stable version from [CRAN](https://cran.r-project.org/package=xxIRT), call `install.packages("xxIRT")` in R console. To install the most recent version from [GitHub](https://github.com/xluo11/xxIRT), call `devtools::install_github("xluo11/xxIRT")` in R console (if *devtools* package has not been installed yet, install it first). To remove the installed package, call `remove.packages("xxIRT")` in R console.

### Introduction

*xxIRT* is a R package designed to implement latest advancements in psychometric research, especially pertaining to computer-based testing, in hopes of facilitating the psychometric research and operations in practice. The package is organized into six modules:

1.  IRT models
2.  IRT Utils
3.  Parameter Estimation
4.  Automated Test Assembly
5.  Computerized Adaptive Testing
6.  Multistage Testing

The application programming interface (API) is for the intermediate R users who are familiar with R, whereas the graphic user interaface (GUI) is for novice users who don't feel like writing code in R.

#### Package Modules

##### IRT Models

The 3-parameter-logistic (3PL) model was introduced by Birnbaum[1], which uses three item parmaeters (a, b, and c) and one people parameter ( **θ**) to describe the probablistic relationship of the item-people interaction. When fixing the *a*-parmaeters to 0.58 (since D=1.7) and *c*-parameters to 0, this model becomes **mathematically** equivalent to the Rasch model[2].

This module creates a S3 R object representing the 3PL model. It contains parameter and response data, as well as functions to compute the probability, information and likelihood. Use `model_3pl(people, items, responses)` or `model_3pl(theta, a, b, c, responses)` to create a *3PL* object. The *people* argument needs to be a data frame with a column named *theta*, the *items* argument a data frame with columns named *a*, *b*, and *c*, and the *responses* argument a matrix or data frame with dimensionality equal to the numbers of people and items. Alternatively, use *theta*, *a*, *b*, and *c* arguments to pass in numeric vectors of parameters directly. Upon its creation, the object comes with functions to compute the probability (`P(x)`), information (`I(x)`), and likelihood (`L(x, log)`) using the 3PL model. It also provides a helper function for generating data (`gendata(n.people, n.items, people, items, ...)`). Pass in the number of people (`n.people`) and items (`n.items`) to generate parameters, or pass in the people (`people`) or item (`items`) parameters to fix parameters in data generation. By default, the **θ** parmaeters are drawn from a normal distribution (`theta.mean=0`, `theta.sd=1`), *a*-parmaeters from a lognormal distribution (`a.mean=0`, `a.sd=0.2`), *b*-parameters from a normal distribution (`b.mean=0`, `b.sd=1`), and *c*-parameters from a beta distribution (`c.alpha=5`, `c.beta=46`).

###### Examples

``` r
### create a 3pl model using given parameters
theta <- c(-1, 0, 1)
a <- c(.588, 1)
b <- c(-1, 1)
c <- c(0, .2)
u <- matrix(c(1, 0, 1, 0, 1, 0), nrow=3)
people <- data.frame(theta=theta)
items <- data.frame(a=a, b=b, c=c)

# create 3pl model using different arguments
model_3pl(people=people, items=items, responses=u) 
model_3pl(people=people, items=items) 
model_3pl(theta=theta, a=a, b=b, c=c) 
model_3pl(people=people, a=a, b=b, c=c) 
model_3pl(theta=theta, items=items) 

# compute Probability, Information, Likelihood by calling itself
x <- model_3pl(people=people, items=items, responses=u)
x$P(x)
x$I(x)
x$L(x)

# compute Probability, Information, Likelihood by calling the class object
model_3pl()$P(x)
model_3pl()$I(x)
model_3pl()$L(x)

### create a 3PL model using generated data
x <- model_3pl()$gendata(5, 3)
x$P(x)
x$I(x)
x$L(x)
```

##### IRT Utils

Item response theory (IRT) is a family of measurement models describing the relationship between observed responses and unobserved item/people parameters. It has been widely used to design and analyze large-scale high-stakes psychological and educaitonal assessments. It is the foundation of some advanced applications such as computerized adaptive testing and multistage testing. See Hambleton and Swaminathan's [3] and Lord's [4] books for more details pertaining to IRT.

This module provides a collection of functions for commonly-used computations and graphing in IRT. For example,

-   `irt_stats(obj, stats, summary, fun)`: compute probability, information, or likelihood, and summarize it over *people* or *item* using customized functions optionally.
-   `irt_select(obj, people.index, items.index)`: subset data
-   `irt_sample(obj, n.people, n.items)`: sample data
-   `irt_rescale_3pl(obj, parameter, mean, sd)`: rescale parameters to the new scale of the **θ** or *b* parameters.
-   `plot(obj, stats, total)`: visualize the specified statistics

###### Examples

``` r
# create a 3pl model using generated data
x <- model_3pl()$gendata(3, 5)

# compute probability, summed over items
irt_stats(x, "prob", summary="people", fun=sum)
# compute information, people(rows) by items (columns)
irt_stats(x, "info")
# compute likelihood, multiplied over items
irt_stats(x, "lik", summary="people", fun=prod)

# retain items [1, 3, 5] and people [2, 4]
irt_select(x, c(1, 3), c(2, 4))
# sample 5 people and 3 items
irt_sample(x, 5, 3)

# rescale parameters
x <- irt_rescale_3pl(x, "theta", 0, 1)
c(mean=mean(x$people$theta), sd=sd(x$people$theta))
x <- irt_rescale_3pl(x, "b", 0, 1)
c(mean=mean(x$items$b), sd=sd(x$items$b))

# draw item characteristic curves
plot(x, stats='prob', total=FALSE)
# draw test information functions
plot(x, stats='info', total=TRUE)
# draw response log-likelihood
plot(x, stats='loglik', total=TRUE)
```

##### Parameter Estimation

This module provides an interface for estimating people and item parameters in the 3PL model with dichtomous responses. Two estimators are available: the maximum likelihood estimator `estimate_mle(u, ...)` and the Bayesian estimator `estimate_bayesian(u, ...)`. To fix values, put the values at the corresponding positions in the `a` (discrimination), `b` (difficulty), `c` (pseudo-guessing), and `t` (ability) arguments, and put `NA` to estimate parameters otherwise. The lower bound of *a* and *c* parameters are 0.0 and the upper bound are controlled by `bound_a` (default=2.0) and `bound_c` (default=0.25). The lower and upper bound of *b* and *t* parameters are controlled by `bound_b` and `bound_t`, default = 3.5. Both estimators iteratively update the parameters until the maximum iterations (`iter=20` by default) or parameter converges (`conv=0.005` by default). When `debug=TRUE`, return additional data for debugging. The `scale` argument controls on which parameter the scale is set (`theta` vs. `b`), and the `scale_mean` and `scale_sd` specify the mean and standard deviation of the scale (0.0 and 1.0 by default).

In addition, the maximum likelihood estimator has the following arguments:

-   `method`: `jmle` to use joint maximum likleihood estimation and `mmle` to use maximum likelihood estimation
-   `mmle_mu`, `mmle_sig`: the mean and standard deviation of the marginal distribution of *t* parameters when `method='mmle'`

The Bayesian estimator has the following arguments:

-   `method`: the theta estimation method, `map`(maximum a posteriori) or `eap`(expected a posteriori)
-   `t_mu`, `t_sig`: the priors of *t* parameters (normal distribution)
-   `a_mu`, `a_sig`: the priors of *a* parameters (log-normal distribution)
-   `b_mu`, `b_sig`: the priors of *b* parameters (normal distribution)
-   `c_alpha`, `c_beta`: the priors of *c* parameters (beta distribution)
-   `reported_sd`: `TRUE` to report the posterior SD of *t* parameters

###### Examples

Example 1: Joint maximum likelihood estimation of all parameters:

``` r
data <- model_3pl()$gendata(2000, 50)
# joint maximum likelihood estimation
x <- estimate_mle(data$responses, method='jmle')
```

Example 2: Marginal maximum likelihood estimation of all parameters:

``` r
# marginal maximum likelihood estimation
x <- estimate_mle(data$responses, method='mmle')
```

Example 3: Bayesian estimation of all parameters (EAP for */$theta/* parameters):

``` r
# bayesian estimation: eap
x <- estimate_mle(data$responses, method='eap')
```

Example 4: Bayesian estimation of all parameters (MAP for */$theta/* parameters):

``` r
# bayesian estimation: map
x <- estimate_mle(data$responses, method='map')
```

Example 5: Estimator comparison under different sample sizes and test lengths:

``` r
n_samples <- seq(500, 2000, by=500)
n_items <- seq(30, 90, by=30)
params_format <- function(data, x, ns, ni, estimator){
  rbind(data.frame(params='t', true=data$people$theta, est=x$t),
        data.frame(params='a', true=data$items$a, est=x$a),
        data.frame(params='b', true=data$items$b, est=x$b),
        data.frame(params='c', true=data$items$c, est=x$c)) %>%
    mutate(n_sample=ns, n_items=ni, estimator=estimator)
}
results <- NULL
for(ns in n_samples){
  for(ni in n_items){
    cat("data:", ns, "by", ni, "\n")
    data <- model_3pl()$gendata(ns, ni)
    x_jmle <- estimate_mle(data$responses, method='jmle', debug=TRUE)
    x_mmle <- estimate_mle(data$responses, method='mmle', debug=TRUE)
    x_eap <- estimate_bayesian(data$responses, method='eap', debug=TRUE)
    x_map <- estimate_bayesian(data$responses, method='map', debug=TRUE)
    rs <- rbind(params_format(data, x_jmle, ns, ni, 'JMLE'),
          params_format(data, x_mmle, ns, ni, 'MMLE'),
          params_format(data, x_eap, ns, ni, 'EAP'),
          params_format(data, x_map, ns, ni, 'MAP'))
    results <- rbind(results, rs)
  }
}
```

Example 6: Fix parameters

``` r
data <- model_3pl()$gendata(1000, 50)
x <- estimate_mle(data$response, t=data$people$theta)
```

Example 7: Estimation with missing data

``` r
data <- model_3pl()$gendata(1000, 50)
na_index <- runif(prod(dim(data$responses))) < .1
data$responses[na_index] <- NA
x <- estimate_mle(data$responses, method='mmle', debug=TRUE, scale="b")
x <- irt_rescale(model_3pl(t=x$t, a=x$a, b=x$b, c=x$c), "theta", 0, 1)
x <- list(t=x$people$theta, a=x$items$a, b=x$items$b, c=x$items$c)
```

##### Automated Test Assembly

Automated test assembly (ATA) is a technique that uses advanced algorithms to assemble test forms with optimized psychometric properties under a set of constraints. ATA is an indispensable component in advanced applications that involves sophisticated test assembly problems. The ATA problem can be solved by heuristic algorithms [5][6] or the mixed integer linear programming (MILP) algorithms [7]. Heuristic algorithms are fast, but cannot guarantee a global optiimality. Conversely, the MILP is more versatile in terms of the size and complexity of the problme it can address. This module provides an ATA interface buit on two open source solvers: [lp\_solve](http://lpsolve.sourceforge.net/5.5/) and [glpk](https://www.gnu.org/software/glpk/).

Call `ata(pool, nform, len, maxselect, debug)` to create a *ata* object which wraps a LP object. Use `ata_obj_relative(ata, coef, mode, negative, flatten, forms, collapse)` and `ata_obj_absolute(ata, coef, target, forms, collapse)` functions to add relative and absolute objective functions to the LP respectively. The relative objective functions is to maximize or minimize the objective function (the `mode` argument), whereas the absolute objective functions is to minimize the discrepancy between the objective function and target (the `target` argument). The `coef` argument can be a pool-size numeric vector, a variable name, or a numeric vector of theta values (when length is not equal to the number of items in the pool). When the expected value of the optimized objective function is negative, set the `negative=TRUE`. The `forms` argument specifies which forms the objectives are set, and by default `forms=NULL` for all forms. When `collapse=TRUE`, forms are collapsed into one combined form. Otherwise, the same objective is set for each form.

Use `ata_constraint(ata, coef, min, max, level, forms, collapse)` to add constraints. The `coef` argument can be a variable name, a constant, or a pool-size numeric vector. When `min=NA` or `max=NA`, the lower or the upper bound of the constraint is not set. When `min==max`, the constraint is set to equal to the value. When `coef` is a categorical variable, use `level` to specify which level is constrained. The `forms` and `collapses` work in the same way as in setting objectives.

Use `ata_item_enemy(ata, items)` to add enemy items which should not be selected into the same form at the same time. Use `ata_item_fixedvalue(ata, items, min, max, forms, collapse)` to force the selection or not selection of items. Use `ata_item_maxselect(ata, maxselect, items)` to set the maximum number of selection for items.

Finally, use `ata_solve(ata, solver, as.list, timeout, mip_gap, verbose, ...)` to solve the LP problem wrapped in the `ata` object. The `solver` argument specifies waht solver will be used: (`lpsolve`)\[<https://cran.r-project.org/package=lpSolveAPI>\] or (`glpk`)\[<https://cran.r-project.org/package=glpkAPI>\]. When `as.list=TRUE` (default value), the results are in a list; otherwise, in a data frame. The `timeout`, `min_gap`, `verbose` arguments are three important MILP parameters. Additional parameters are taken by `...`. See the documentation of \*lpSolveAPI** and **glpkAPI\*\* for more detail. When the LP is successfully solve, the function adds a four types of data to the original object:

-   `status`: the status of the solution
-   `optimum`: the value of the objective function
-   `result`: a binary matrix of assembly result
-   `items`: a list or data frame of assembled items

Use `plot(ata)` to visualize the TIFs of assembled test forms.

###### Examples

Below are five examples to illustrate how to use this module. First, a 100-item pool with content area (categorical) and response time (quantitative) variables is generated and used throughout all examples. The first example assembles four parallel forms with 10 items to maximize *b* parameters. The second example assembles four parallel forms with 10 items to minimize *b* parameters in which the expected objective function has a negative value. The third example assembles four parallel forms with 10 items to maximize information at *θ* = −1 and *θ* = 1 while subject to (3, 3, 4) items from content area (1, 2, 3) and average response is between 55 and 65 seconds. The fourth example assembles 2 10-item parallel forms that has mean and sd of *b* parameters equal to 0 and 1 while subject to (3, 3, 4) items from content area (1, 2, 3). The fifth example assembles 2 10-item parallel forms that has a flat TIF over the interval *θ* = \[ − 1, 1\].

``` r
### generate a pool with 100 3PL items
### item properties: content and response time
items <- irt_model("3pl")$gendata(1, 100)$items
items$content <- sample(1:3, nrow(items), replace=TRUE)
items$time <- round(rlnorm(nrow(items), log(60), .2), 0)

### ex. 1: assemble 6 parallel forms, each with 10 items
### objecrive: maximize b parmaters
## solved by GLPK and LpSolve respectively
x <- ata(items, 6, len=10, maxselect=1)
x <- ata_obj_relative(x, "b", "max")
glpk <- ata_solve(x, solver="glpk")
glpk$optimum
sapply(glpk$items, function(x)
  c(mean=mean(x$b), sd=sd(x$b), min=min(x$b), max=max(x$b)))
lpsolve <- ata_solve(x, solver="lpsolve")
lpsolve$optimum
sapply(lpsolve$items, function(x)
  c(mean=mean(x$b), sd=sd(x$b), min=min(x$b), max=max(x$b)))

### ex. 2: assembled 4 parallel forms, each with 10 items
### objective: minimize b parmaeters
x <- ata(items, 3, len=10, maxselect=1)
x <- ata_obj_relative(x, "b", "min", negative=TRUE)
glpk <- ata_solve(x, solver="glpk", as.list=FALSE, timeout=5)
group_by(glpk$items, form) %>%
  summarise(mean=mean(b), sd=sd(b), min=min(b), max=max(b))
lpsolve <- ata_solve(x, solver="lpsolve", as.list=FALSE, timeout=5)
group_by(lpsolve$items, form) %>%
  summarise(mean=mean(b), sd=sd(b), min=min(b), max=max(b))

### ex. 3: assemble 3 parallel forms, each with 10 items
### objective: maximize information at -1 and 1
### content: [3, 3, 4] items in area 1--3
### response time: avg. 55-65 seconds
x <- ata(items, 3, len=10, maxselect=1)
x <- ata_obj_relative(x, c(-1, 1), "max")
x <- ata_constraint(x, "content", min=3, max=3, level=1)
x <- ata_constraint(x, "content", min=3, max=3, level=2)
x <- ata_constraint(x, "content", min=4, max=4, level=3)
x <- ata_constraint(x, "time", min=55*10, max=65*10)
lpsolve <- ata_solve(x, solver="lpsolve")
lpsolve$optimum
plot(lpsolve)
sapply(lpsolve$items, function(x)
  c(freq(x$content, 1:3)$freq, mean(x$time)))

### ex. 4: assemble 2 parallel forms, each with 10 items
### objective: mean(b) = 0, sd(b) = 1.0
### content: [3, 3, 4] items in area 1--3
x <- ata(items, 2, len=10, maxselect=1) %>%
     ata_obj_absolute(items$b, 0 * 10) %>%
     ata_obj_absolute((items$b - 0)^2, 1 * 10) %>%
     ata_constraint("content", min=3, max=3, level=1) %>%
     ata_constraint("content", min=3, max=3, level=2) %>%
     ata_constraint("content", min=4, max=4, level=3)
lpsolve <- ata_solve(x, "lpsolve", verbose="normal", timeout=5)
sapply(lpsolve$items, function(x) c(mean=mean(x$b), sd=sd(x$b)))

### ex. 5: assemble 2 parallel forms, each with 10 items
### objective: flat TIF over [-1, 1]
x <- ata(items, 2, len=10, maxselect=1) %>%
     ata_obj_relative(seq(-1, 1, .5), "max", flatten=0.05)
x <- ata_solve(x, "lpsolve")
plot(x)
```

##### Computerized Adaptive Testing

Computerized adaptive testing (CAT) is a computer-based testing (CBT) model that tailors the test form in real time to match the test taker's ability. The test adaptation in CAT avoids overly difficulty or easy items in relation to the test taker's ability and hence improves the testing efficiency. An operational CAT algorithm often comprises three important sub-algorithms: the item selection rule, the ability estimation rule, and the stopping rule. The selection rule delineates how to select the next item given the latest ability estimate of the test taker. The estimation rule estimates the ability parameter using the responses that have been collected so far. And the stopping rule determines how to exit the repetitive "seletion-estimation" cycles to finish the test.

This module provides a flexible framework for simulating CAT using different selection, estimation, and/or stopping rules. To expeirment a new rule, the rule of interest is the only component that needs to be authored in the CAT system. This could greatly reduce one's workload. Moreover, this module provides multiple common rules for use as well, including the maximum information selection rule [8], the C-CAT selection rule [9], the shadow-test selection rule [10], the standard error stopping rule, the minimal information stopping rule, the confidence interval stopping, the projetion-based stopping rule\[^12\], and the EAP+MLE estimation rule.

Use `cat_sim(true.theta, pool, opts, cat.select, cat.estimate, cat.stop, debug)` to conduct a CAT simulation with the given true theta and item pool. The `opts` argument is a list of option parameters, into which the minimum (`min`) and maximum (`max`) test length and parameters required by the selection, estimation, stopping rules are passed. The default `cat.selection` rule is the maximum information selection rule. Add a `randomesque` value to `opts` to randomly select one item from the *k* items with largest information. Set `cat.select=cat_select_ccat` to use the C-CAT selection rule, which requires `opts` to include `ccat.target` (a vector of content distribution target in percentage) and `cat.random` (inital randomness in content area selection). Set `cat.select=cat_select_shadow` to use the shadow-test selection rule, which requires `opts` to include `shadow.constraints` (a data frame of constraints with four columns: name, level, min and max). The default `cat.stop` rule is a trifold rule depending on which parameter is passed to `opts`. The `stop.se` value in `opts` triggers the standard error rule, `stop.mi` the minimum information rule, and `stop.cut` the confidence interval rule. The default `cat.estimate` rule is the combined EAP and MLE rule, which applies EAP when the responses are all 1's or 0's and MLE otherwise.

When `debug=TRUE`, the debugging mode is on and the `cat_sim` function prints debugging information on the console to explain the internal process of the function. Once the simulation is completed, the `cat_sim` function returns a *cat* object, inclusive of a data frame of unused pool (`pool`), a data frame of used items (`items`), responses and theta history (`stats`), administration history (`admin`), test length (`len`), true theta (`true`), and estimated theta (`eat`). Use `plot(...)` to visualize the simulaiton.

###### Examples

``` r
### generate a pool of 200 3PL items
### item properties: content, response time
pool <- irt_model("3pl")$gendata(1,200)$items
pool$content <- sample(1:3, nrow(pool), replace=TRUE)
pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))

### ex. 1: 10-30 items
### maximum information selection rule
### standard error stopping rule (se=.3)
opts <- list(min=10, max=30, stop.se=.3)
x <- cat_sim(0.1, pool, opts)
tail(x$admin, 5) %>% round(., 2)
plot(x)

### ex. 2: 10-30 items
### maximum information selection rule
### minimum information stopping rule (mi=.3)
opts <- list(min=10, max=30, stop.mi=.8)
x <- cat_sim(0.1, pool, opts)
tail(x$admin, 5) %>% round(., 2)
plot(x)

### ex. 3: 10-30 items
### maximum information selection rule
### confidence interval stopping rule (cut=0)
opts <- list(min=10, max=30, stop.cut=0)
x <- cat_sim(0.1, pool, opts)
tail(x$admin, 5) %>% round(., 2)
plot(x)

### ex. 4: 10-30 items
### maximum selection rules, randomesque = 5
### standard error stopping rule (se=.3)
opts <- list(min=10, max=30, stop.se=.3, randomesque=5)
x <- cat_sim(0.1, pool, opts)
tail(x$admin, 5) %>% round(., 2)
plot(x)

### ex. 5: 10-30 items
### c-cat selection rule, first 10 areas are random, target = [.50, .25, .25]
### confidence interval stopping rule
opts <- list(min=30, max=60, stop.cut=0, ccat.target=c(.50,.25,.25), ccat.random=10)
x <- cat_sim(0.1, pool, opts, cat.select=cat_select_ccat)
tail(x$admin, 5) %>% round(., 2)
freq(x$admin$content, 1:3)
plot(x)

### ex. 6: 30 items
### shadow test selection rule
### content: [10, 10, 10] items in area 1--3
### response time: avg. 55--65 seconds
cons <- data.frame(name="content", level=c(1,2,3), min=c(10, 10, 10),
                   max=c(10, 10, 10), stringsAsFactors=FALSE)
cons <- rbind(cons, c("time", NA, 55*30, 65*30))
opts <- list(min=30, max=30, stop.se=.03, shadow.constraints=cons)
x <- cat_sim(0.1, pool, opts, cat.select=cat_select_shadow)
freq(x$admin$content, 1:3)
mean(x$items$time)

### ex. 7: 10-30 items
### shadow selection rule
### projection-based stopping rule
opts <- list(min=10, max=30, projection.cut=0, projection.constraints=cons, projection.method="difficulty", shadow.constraints=cons)
x <- cat_sim(0.1, pool, opts, cat.select=cat_select_shadow,
cat.stop=cat_stop_projection)
plot(x)
```

##### Multistage Testing

Multistage testing (MST) is an adaptive CBT model that navigates test takers through a set of pre-constructed item sets, spanning over multiple testing stages, in order to create a test form most suited to their abilities. The pre-constructed item sets are called *modules*, the entire collection of modules connected across stages by the routing rules is called a *panel*, and the pathway used to finish the test is called a *route*. A common practice is to have multiple parallel and interchangeable panels in place for administration. One panel will be randomly selected for a test taker to alleviate the item exposure rate.

With reduced adaptivity, it is difficult for MST to surmount CAT in terms of measurement quality and efficiency. However, because all tests are assembled before administration, the content and psychometric properties of the test can be reviewed before the pulication. This module provides an interface for designing, assembling and simulating a MST.

Call `mst(pool, design, npanel, method, len, maxselect)` to initiate a *mst* object. The design is the structural design of the MST. For instance, `design=c(1, 2, 2)` means a 1-2-2 MST with 3 stages, 5 modules, and 4 routes, and `design=c(1, 2, 3)` means a 1-2-3 MST with 3 stages, 6 modules, and 6 routes. When `method='bottomup'`, the bottom-up design approach is used, meaning objectives and constraints are imposed on modules. This is a very practical approach and has been commonly used in practice and research. When `method='topdown'`, objectives and constraints are imposed on routes. The top-down approach automatically finds an optimal way to allocate objectives, constraints, and items across stages, but it is a relatively new approach. Internally, the *form* number is the global unique identifier assigned to modules, and the *index* number is the in-panel unique identifier assigned to modules. The former is used for ATA to recognize modules/forms, and the latter is for users to find a module in a given panel. Modules will be indexed sequentially from the first to the last stage. The `module` data frame in a *mst* object shows the mapping between index and stage-module position. The `route` data frame shows all permissible routes. Use `mst_route(mst, route, op)` to add new routes or remove existing routes. In practice, routes with dramatic changes are often prohibited.

Use `mst_objective(mst, theta, indicies, target, flatten, ...)` to set the TIF objectives. Use the `target` argument to set a target value at given `theta` points. Or, leave it to `NULL` to maximize the information at given `theta` points. Use the `flatten` argument to have a flat TIF. Use `mst_constraint(mst, coef, min, max, level, indicies)` to add constraints. The `min` and `max` arguments are allowed to be `NA`. In the top-down approach, it is likely to have all items allocated to the vary last stage only for optimality. To circumvent this situation, use `mst_stage_length(mst, stages, min, max)` to regulate the test length in earlier stages. Or, use `mst_set_rdp(mst, theta, indices, tol)` and `mst_module_mininfo(mst, theta, mininfo, indices)` to impose the location and minimum information constraints on routing decision points (RDPs), which helps prevent the empty stages/modules. Finally, use `mst_assemble(mst)` to attempt to assemble the MST. If assembled successfully, the *mst* object adds a data frame of assembled items (called `items`), which adds a few extra columns to indicate the *panel*, *module*, in-panel *index*, and global *form* indices. Use `mst_get_items(mst, panel, stage, module, route)` to retrieve items from an assembled MST. To visualize the assembly results, call `plot(mst, byroute)`. When `byroute=FALSE`, it draws the TIFs of modules in a panel-by-stage grid. When `byroute=TRUE`, it draws the TIFs of routes in each panel.

Use `mst_sim(mst, true.theta, rdp)` to conduct a simulation on an assembled MST. If `rdp=NULL`, the maximum information routing rule is used. Otherwise, set `rdp` to a list of vectors of RDPs in each stage.

###### Examples

``` r
### generate a 300-item pool
pool <- irt_model("3pl")$gendata(1,300)$items
pool$content <- sample(1:3, nrow(pool), replace=TRUE)
pool$time <- round(exp(rnorm(nrow(pool), log(60), .2)))

### ex. 1: 1-2-2 MST, 2 panels, topdown
### 20 items, content = c(10, 5, 5)
### maximize information at -1 and 1 for easy and hard routes
x <- mst(pool, design=c(1, 2, 2), npanel=2, method='topdown', len=20, maxselect=1)
x <- mst_objective(x, theta=-1, indices=1:2)
x <- mst_objective(x, theta= 1, indices=3:4)
x <- mst_constraint(x, coef="content", min=10, max=10, level=1)
x <- mst_constraint(x, coef="content", min=5, max=5, level=2)
x <- mst_constraint(x, coef="content", min=5, max=5, level=3)
x <- mst_stage_length(x, stages=c(1, 2, 3), min=1)
x <- mst_assemble(x, timeout=10)
plot(x)
plot(x, byroute=TRUE)
freq(mst_get_items(x, panel=1, route=1)$content, 1:3)$freq
freq(mst_get_items(x, panel=2, route=4)$content, 1:3)$freq

### ex. 2: 1-2-3 MST, 2 panels, bottomup,
### 10 items in each module, content = c(4, 3, 3)
### maximize information at -1, 0 and 1 for easy, medium, and hard modules
x <- mst(pool, design=c(1, 2, 3), npanel=2, method='bottomup', len=10, maxselect=1)
x <- mst_route(x, c(1, 2, 6), "-")
x <- mst_route(x, c(1, 3, 4), "-")
x <- mst_objective(x, theta= 0, indices=1)
x <- mst_objective(x, theta=-1, indices=c(2,4))
x <- mst_objective(x, theta= 1, indices=c(3,5))
x <- mst_constraint(x, coef="content", min=4, max=4, level=1)
x <- mst_constraint(x, coef="content", min=3, max=3, level=2)
x <- mst_constraint(x, coef="content", min=3, max=3, level=3)
x <- mst_assemble(x, timeout=10)
plot(x)
plot(x, byroute=TRUE)

### ex. 3: 1-2-3 MST, 2 panels, topdown, 30 items,
### 10 items in each content area
### target information at 18 at -1, 0, and 1 for easy, medium and hard routes
x <- mst(pool, design=c(1, 2, 3), npanel=2, method='topdown', len=30, maxselect=1)
x <- mst_route(x, c(1, 2, 6), "-")
x <- mst_route(x, c(1, 3, 4), "-")
x <- mst_objective(x, theta=-1, indices=1, target=16)
x <- mst_objective(x, theta= 0, indices=2:3, target=16)
x <- mst_objective(x, theta= 1, indices=4, target=16)
x <- mst_constraint(x, coef="content", min=10, max=10, level=1)
x <- mst_constraint(x, coef="content", min=10, max=10, level=2)
x <- mst_constraint(x, coef="content", min=10, max=10, level=3)
x <- mst_stage_length(x, stages=c(1, 2, 3), min=3)
x <- mst_assemble(x, timeout=20)
plot(x, byroute=TRUE)

### ex. 4: simulation using the maximum information
y <- mst_sim(x, 0.2)
y$thetas
y$ses
y$route

### ex. 4: simulation using given RPDs
y <- mst_sim(x, -0.2, rdp=list(s1=c(0), s2=c(-.44, .44)))
y$thetas
y$ses
y$route
```

### Graphic User Interfaces

The GUIs built on *shiny* were reomved from this package. I'm trying to put together another standalone packages for GUIs.

### Ending

Please send your comments, questions, feature request and bug reporting to the author [Xiao Luo](mailto:xluo1986@gmail.com).

[1] Birnbaum, A. (1968). Some latent trait models. In F.M. Lord & M.R. Novick, (Eds.), Statistical theories of mental test scores. Reading, MA: Addison-Wesley.

[2] Rasch, G. (1966). An item analysis which takes individual differences into account. British journal of mathematical and statistical psychology, 19(1), 49-57.

[3] Hambleton, R. K., & Swaminathan, H. (1985). Item response theory: Principles and applications. New York, NY: Springer.

[4] Lord, F. M. (1980). Applications of item response theory to practical testing problems. New York NY: Routledge.

[5] Stocking, M. L., & Swanson, L. (1998). Optimal design of item banks for computerized adaptive tests. Applied Psychological Measurement, 22, 271-279.

[6] Luecht, R. M. (1998). Computer-assisted test assembly using optimization heuristics. Applied Psychological Measurement, 22, 224-236.

[7] van der Linden, W. J., & Reese, L. M. (1998). A model for optimal constrained adaptive testing. Applied Psychological Measurement, 22, 259-270.

[8] Weiss, D. J., & Kingsbury, G. (1984). Application of computerized adaptive testing to educational problems. Journal of Educational Measurement, 21, 361-375.

[9] Kingsbury, C. G., & Zara, A. R. (1991). A comparison of procedures for content-sensitive item selection in computerized adaptive tests. Applied Measurement in Education, 4, 241-261.

[10] van der Linden, W. J. (2000). Constrained adaptive testing with shadow tests. In Computerized adaptive testing: Theory and practice (pp. 27-52). Springer Netherlands.
