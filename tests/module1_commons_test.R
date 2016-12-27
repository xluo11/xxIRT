library(reshape2)
library(ggplot2)
library(lpSolveAPI)
library(gaussquad)

# create a 3PL model with manual inputs
people <- data.frame(theta=c(-1,0,1))
items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
x <- irt.model(people, items, responses, "3PL") 
# create a 3PL model with NULLs
irt.model(people, items, NULL, "3pl")
irt.model(NULL, items, NULL, "3pl")
irt.model(people, NULL, NULL, "3pl")

# Generate a 3PL model
x <- irt.model(model="3pl")$gen.data(20, 5)
x$probability(x)
x$information(x)
x$likelihood(x)
# Generate Rasch model
irt.model(model="3pl")$gen.data(20, 5, a.mu=0, a.sig=0, c.alpha=0, c.beta=0)
# Generate an 3PL item pool
irt.model(model="3pl")$gen.data(1, 100)$items
# Generate responses using given people and item parameters
irt.model(model="3pl")$gen.data(people=people, items=items)$responses

# create 3PL model using parameter vectors
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

# draw an IRT model
x <- irt.model(model="3pl")$gen.data(20, 5)
plot(x, stats="probability")
plot(x, stats="probability", total=FALSE)
plot(x, stats="information")
plot(x, stats="information", total=FALSE)
plot(irt.sample(x, n.people=1), stats="loglikelihood", theta=seq(-5,5,.1))

# select and sample from an IRT model
x <- irt.model(model="3pl")$gen.data(20, 5)
x
irt.select(x, people.index=c(1,3,5))
irt.select(x, items.index=c(1,3,5))
# sample wihtout replacement
irt.sample(x, n.people=3)
irt.sample(x, n.items=3)
# sample with replacement
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
