library(reshape2)
library(ggplot2)
library(lpSolveAPI)
library(gaussquad)

# estimate people parameters for 3pl model
data <- irt.model(model="3pl")$gen.data(500, 50)
# mle
x <- estimate.people.3pl.mle(data$responses, data$items, debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
# map
x <- estimate.people.3pl.map(data$responses, data$items, debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
# eap
x <- estimate.people.3pl.eap(data$responses, data$items)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
# the wrapper function
x <- estimate.people(data$responses, data$items, model="3pl", method="mle", debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
x <- estimate.people(data$responses, data$items, model="3pl", method="map", debug=TRUE)
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
x <- estimate.people(data$responses, data$items, model="3pl", method="eap")
cor(data$people$theta, x$people$theta)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)

# estimate people parameters with varying test lengths
n.items <- seq(10, 100, 10)
n.people <- 200
output <- matrix(nrow=length(n.items), ncol=3, dimnames=list(n.items, c("mle","map","eap")))
for(i in 1:length(n.items)){
  data <- irt.model(model="3pl")$gen.data(n.people, n.items[i])
  x <- estimate.people.3pl.mle(data$responses, data$items)
  output[i, 1] <- cor(data$people$theta, x$people$theta)
  x <- estimate.people.3pl.map(data$responses, data$items)
  output[i, 2] <- cor(data$people$theta, x$people$theta)
  x <- estimate.people.3pl.eap(data$responses, data$items)
  output[i, 3] <- cor(data$people$theta, x$people$theta)
}
output
# mle=red, map=green, eap=blue
plot(n.items, output[,"mle"], type="l", xlab="Number of Items", ylab="Correlation", ylim=c(.5,1), col=rgb(.8,.2,.2,.7))
lines(n.items, output[,"map"], col=rgb(.2,.8,.2,.7))
lines(n.items, output[,"eap"], col=rgb(.2,.2,.8,.7))


# estimate item parameters for 3pl model
data <- irt.model(model="3pl")$gen.data(1000, 50)
# JMLE
x <- estimate.items.3pl.jmle(data$responses, data$people, debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0,2), ylim=c(0,2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4,4), ylim=c(-4,4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0,.4), ylim=c(0,.4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
# MMLE
x <- estimate.items.3pl.mmle(data$responses, debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0,2), ylim=c(0,2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4,4), ylim=c(-4,4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0,.4), ylim=c(0,.4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
# BME
x <- estimate.items.3pl.bme(data$responses, debug=TRUE)
cor(data$items, x$items)
plot(data$items$a, x$items$a, xlim=c(0,2), ylim=c(0,2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4,4), ylim=c(-4,4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0,.4), ylim=c(0,.4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
# the wrapper function
x <- estimate.items(data$responses, model="3pl", method="jmle", debug=TRUE, people=data$people)
cor(data$items, x$items)
x <- estimate.items(data$responses, model="3pl", method="mmle", debug=TRUE)
cor(data$items, x$items)
x <- estimate.items(data$responses, model="3pl", method="bme", debug=TRUE)
cor(data$items, x$items)

# estimate item parameters with varying sample size
n.people <- seq(500, 3000, 250)
n.items <- 40
output <- array(NA, dim=c(3, length(n.people), 3), dimnames=list(c("a","b","c"), n.people, c("jmle","mmle","bme")))
for(i in 1:length(n.people)){
  cat("sample size:", n.people[i], "/n")
  data <- irt.model(model="3pl")$gen.data(n.people[i], n.items)
  x <- estimate.items.3pl.jmle(data$responses, data$people)
  output[, i, 1] <- diag(cor(data$items, x$items))
  x <- estimate.items.3pl.mmle(data$responses)
  output[, i, 2] <- diag(cor(data$items, x$items))
  x <- estimate.items.3pl.bme(data$responses)
  output[, i, 3] <- diag(cor(data$items, x$items))
}
output
# a: mle=red, map=green, eap=blue
plot(n.people, output["a",,"jmle"], type="l", xlab="Number of People", ylab="Correlation", ylim=c(.3,1), col=rgb(.8,.2,.2,.7))
lines(n.people, output["a",,"mmle"], col=rgb(.2,.8,.2,.7))
lines(n.people, output["a",,"bme"], col=rgb(.2,.2,.8,.7))
# b: mle=red, map=green, eap=blue
plot(n.people, output["b",,"jmle"], type="l", xlab="Number of People", ylab="Correlation", ylim=c(.5,1), col=rgb(.8,.2,.2,.7))
lines(n.people, output["b",,"mmle"], col=rgb(.2,.8,.2,.7))
lines(n.people, output["b",,"bme"], col=rgb(.2,.2,.8,.7))
# c: mle=red, map=green, eap=blue
plot(n.people, output["c",,"jmle"], type="l", xlab="Number of People", ylab="Correlation", ylim=c(0,1), col=rgb(.8,.2,.2,.7))
lines(n.people, output["c",,"mmle"], col=rgb(.2,.8,.2,.7))
lines(n.people, output["c",,"bme"], col=rgb(.2,.2,.8,.7))

# estimate both item and people parameters
data <- irt.model(model="3pl")$gen.data(2000, 50)
x <- estimate.3pl(data$responses, debug=TRUE)
plot(data$people$theta, x$people$theta, xlim=c(-4,4), ylim=c(-4,4), xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.5), pch=16)
plot(data$items$a, x$items$a, xlim=c(0,2), ylim=c(0,2), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$b, x$items$b, xlim=c(-4,4), ylim=c(-4,4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))
plot(data$items$c, x$items$c, xlim=c(0,.4), ylim=c(0,.4), pch=16, xlab="True", ylab="Est.", col=rgb(.8,.2,.2,.6))

# estimate people parameters with missing data
data <- irt.model(model="3pl")$gen.data(1000, 20)
miss <- matrix(ifelse(runif(1000*20) >.05, 1, NA), nrow=1000)
responses <- miss * data$responses
# MLE
x <- estimate.people.3pl.mle(responses, data$items, debug=TRUE)
cor(data$people$theta, x$people$theta)
# MAP
x <- estimate.people.3pl.map(responses, items, debug=TRUE)
cor(data$people$theta, x$people$theta)
# EAP
x <- estimate.people.3pl.eap(responses, items)
cor(data$people$theta, x$people$theta)

# estimate item parameters with missing data
data <- irt.model(model="3pl")$gen.data(2000, 50)
miss <- matrix(ifelse(runif(2000*50) >.05, 1, NA), nrow=2000)
responses <- miss * data$responses
# JMLE
x <- estimate.items.3pl.jmle(responses, data$people, debug=TRUE)
cor(data$items, x$items)
# MMLE
x <- estimate.items.3pl.mmle(responses, debug=TRUE)
cor(data$items, x$items)
# BME
x <- estimate.items.3pl.bme(responses, debug=TRUE)
cor(data$items, x$items)


