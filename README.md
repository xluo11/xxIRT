# xxIRT: R Package for Item Response Theory
######Author: [Xiao Luo](mailto:xluo1986@gmail.com) || Last Edit: August 09, 2016

######**Citation**: Luo, X. (2016). xxIRT: R Package for Item Response Theory (Version 2.0) [Software]. Retrieved from [https://github.com/xluo11/xxIRT](https://github.com/xluo11/xxIRT)


xxIRT is a R package to conduct item response theory (IRT) analysis and research. It comprises of five modules: common computations, computerized adaptive testing (CAT) simulation framework, estimation procedures, automated test assembly (ATA), and multistage testing (MST).

###Installation
If you haven't installed the the *devtools* package, install it first `install.packages("devtools")`. Then use `devtools::install_github("xluo11/xxIRT")` to install the **xxIRT** package from github.com.

###Module I: Commons & Utiities
At the heart of this package is an IRT object which contains a **thetas** vector of ability parameters and a **items** data.frame of item parameters. Use `irt(theta, a, b, c)` to create an IRT object with given values. To enjoy the laziness, use `gen.irt(n.peo, n.item)` to generate an IRT object with *n.peo* thetas and *n.item* items. Passing the IRT object to `gen.rsp(irt.object)` to generate binary response matrix. 

Computing model-based correct response probability is a common practice in IRT. Use `prob(irt.object)` to compute a probability matrix with given *thetas* and *items* in the IRT object. The resulting matrix has *thetas* in rows and *items* in columns. Use `info(irt.object)` to compute information in the similar fashion.

`plot(irt.object)` draws item/test characteristic curves or item/test information functions, depending on the arguments `summary` and `fun`. 

###Module II: Estimation
Estimation of people and item parameters from an observed response matrix is the most fun yet challenging task in IRT. Three estimators of theta parameters are provided in this package, which assumes item parameters are known or already calibrated. `estimate.theta.mle(u, a, b, c)` uses the maximum likelihodd method, `estimate.theta.eap(u, a, b, c)` the expected a posteriori method, and `estimate.theta.map(u, a, b, c)` uses the maximum a posteriori method. 

Another three estimators of item parameters are provided too. If *thetas* are known, `estimate.item.jmle(u, theta)` calibrates the item parameters using the joint maximum likelihood method. If *thetas* are unknown, `estimate.item.mmle(u)` calibrates the item parameters using the marginal maximum likelihood method by integrating out *thetas* with an assumed distribution. Usually we assume *thetas* are from a standard normal distribution, and use Hermite-Gauss quadrature to approximate the integration of *thetas*. `estimate.item.bme(u)` brings this Bayesian techniques to not only *thetas* but also item parameters. In the implementation, we assumes *a* is from a lognormal distribution, *b* from a normal distribution, and *c* from a beta distribution.

###Module III: CAT
Computerized adaptive testing (CAT) changes the landscape with new technologies. In CAT, tests are administered dynamically and customized to fit each individual's characteristic in real time. Testing becomes, to some extent, a two-way communication as in an interview where the next question is based on the answer to the previous question. `cat.sim(theta, u, opts)` establishes a general framework to conduct CAT simulation, where users pass in their own version of `cat.select` (item selection algorithm), `cat.estimate` (estimation method), and `cat.stop` (termination rule). This is flexible framework allows users to run CAT simulation with minimal modifications. Default algorithms of those CAT components are of course provided. The default select method, `cat.select.default`, randomly selects an item from the *k* most informative items. The default estimation method, `cat.estimate.default`, uses `estimate.theta.eap` for a response vector of all 1s or all 0s and `estimate.theta.mle` otherwise. The default termination method, `cat.stop.default`, evalutes one of the three criteria after reaching the minimum length: (1) if the se threshold is reached; (2) if all items fail to provide minimum information; (3) if a clear pass/fail decision can be made. Which criterion is used for termination decision depends on the parameters in options. 

The `cat.sim(theta, u, opts)` results in a `cat` object, which contains list components like `admins` (administration history), `items` (item history), `stats` (response and theta history), `pool` (unused pool), `true` (true theta), `est` (estimated theta), `len` (test length). Use `print(cat.object)` and `plot(cat.object)` to print and plot the `cat` object.

###Module IV: ATA
Automated test assembly (ATA) is a fairly useful technology in psychometrics, in which an advanced optimization algoirhtm is applied to select items from the item pool to optimize the objective function while satisfying a large quantity of complicated test constraints. The ATA module in this package is built upon the [lp_solve](http://lpsolve.sourceforge.net) library and [lpSolveAPI](https://cran.r-project.org/web/packages/lpSolveAPI/lpSolveAPI.pdf) package. 

Use `ata(pool, nfor)` to initiate an ATA object with specified item pool and number of forms to assemble. Afterwards, use `ata.obj.rel` (maximize/minimize) and `ata.obj.asb` (approach target values) to add objective functions, and use `ata.constraint` to add constraints. Don't forget to limit the maximum times of selection for items by calling `ata.maxselect`. After all done, call `ata.solve` to solve the problem. If the problem is successfully solved, `rs.index` and `rs.items` should be added to the ata object.

###Module V: MST
Multistage testing (MST) is a mode of testing comprising of multiple testing stage and test adaptation between stages. Compared withe CAT, MST gives test developers more control over the test quality and characteristis by allowing tests to be assembled in house prior to administration/publishing. Call `mst(pool, design, npanel)` to initiate a mst object. Multiple panels are assembled and randomly assigned to a test taker to ameliorate the risk of item overexposure. By default, one item is only selected once. Use `mst(mst.object, route, op)` to add/remove a route to/from the mst object. Some routes are forbid to avoid radical routing. Use `mst.objective(mst.object, thetas, targets, routes)` and `mst.constraint(mst.object, variable, level, min, max, routes)` to add objectives and constraints upon routes. This is a top-down approach, because objectives and constraints are directly imposed on routes as opposed to broken down to stages and modules. Don't forget to set minimum length in stage by calling `mst.stagelength(mst.object, stage, min, max)`; otherwise, it is very likely to allocate all items in final stage. Use `mst.assemble(mst.object)` to solve the problem and assemble items, which adds a data.frame of assembled items if the problem is solved successfully. 

Use `mst.summary(mst.object)` and `plot(mst.object)` to summarize and visualize an assembled mst. Passing a `by.route=TRUE` argument to the call to summarize and visualize results by routes. `mst.get(mst.object, panel, module, route)` is a helper function to retrieve assembly results. Either *module* or *route* is set, not both. Also, use `mst.sim(mst.object, theta, routing, estimator)` to run a mst simulation.

When the assembly problem is too large to be solve simultaneously, use `mst.assemble.sequence(mst.object)` to assemble panels in sequence. Results will favor the panels assembled earlier than later. Try to run another round of simultaneous assembly  `mst.assemble` using a reduced item pool which include items in the the sequential assembly result as well as some additional items randomly selected from the remaining pool.

###Ending
This is an early version of an ongoing project. Please leave comments, suggestions, questions to [the author](mailto:xluo1986@gmail.com).
