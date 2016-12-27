library(reshape2)
library(ggplot2)
library(lpSolveAPI)
library(gaussquad)

# create a 3pl model with manual inputs
people <- data.frame(theta=c(-1,0,1))
items <- data.frame(a=c(.5,1,1.5,1,1,1,1), b=c(0,0,0,-1,1,0,0), c=c(0,0,0,0,0,.1,.2))
responses <- data.frame(item1=c(0,0,1), item2=c(0,1,1), item3=c(0,1,1), item4=c(1,1,1), item5=c(0,0,1), item6=c(0,1,1), item7=c(1,1,1))
x <- irt.model.3pl(people, items, responses) 
x$probability(x)
x$information(x)
x$likelihood(x)

# create a 3pl model with NULLs
x <- irt.model.3pl(people, items, NULL)
x$probability(x)
x$information(x)
x <- irt.model.3pl(NULL, items, NULL)
x <- irt.model.3pl(people, NULL, NULL)
x <- irt.model.3pl(NULL, NULL, NULL)

# create a 3pl model with generated data
x <- irt.model.3pl()$gen.data(20, 5)
x
x$probability(x)
x$information(x)
x$likelihood(x)
