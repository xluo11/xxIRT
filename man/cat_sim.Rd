% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module4_cat.R
\name{cat_sim}
\alias{cat_sim}
\alias{cat_estimate_mle}
\alias{cat_estimate_eap}
\alias{cat_estimate_hybrid}
\alias{cat_stop_default}
\alias{cat_select_maxinfo}
\alias{cat_select_ccat}
\alias{cat_select_shadow}
\alias{print.cat}
\alias{plot.cat}
\alias{cat_stop_projection}
\title{Simulation of Computerized Adaptive Testing (CAT)}
\usage{
cat_sim(true, pool, ...)

cat_estimate_mle(len, theta, stats, admin, pool, opts)

cat_estimate_eap(len, theta, stats, admin, pool, opts)

cat_estimate_hybrid(len, theta, stats, admin, pool, opts)

cat_stop_default(len, theta, stats, admin, pool, opts)

cat_select_maxinfo(len, theta, stats, admin, pool, opts)

cat_select_ccat(len, theta, stats, admin, pool, opts)

cat_select_shadow(len, theta, stats, admin, pool, opts)

\method{print}{cat}(x, ...)

\method{plot}{cat}(x, ...)

cat_stop_projection(len, theta, stats, admin, pool, opts)
}
\arguments{
\item{true}{the true theta}

\item{pool}{the item pool (data.frame)}

\item{...}{option/control parameters}

\item{len}{the current test length}

\item{theta}{the current theta estimate}

\item{stats}{a matrix of responses, theta estimate, information and std error}

\item{admin}{a data frame of administered items}

\item{opts}{a list of option/control parameters}

\item{x}{a \code{cat} object}
}
\value{
\code{cat_sim} returns a \code{cat} object

an estimation rule should return a theta estimate

a stopping rule should return a boolean: \code{TRUE} to stop the CAT, \code{FALSE} to continue

a selection rule should return a list of (a) the selected item and (b) the updated pool
}
\description{
\code{cat_sim} runs a simulation of CAT. Use \code{theta} in options to set the starting
value of theta estimate.

\code{cat_estimate_mle} is the maximum likelihood estimation rule. Use 
\code{map_len} to apply MAP to the first K items and use \code{map_prior} to set the
prior for MAP.

\code{cat_estimate_eap} is the expected a posteriori estimation rule,
using \code{eap_mean} and \code{eap_sd} option parameters as the prior

\code{cat_estimate_hybrid} is a hybrid estimation rule, which uses MLE for
mixed responses and EAP for all 1's or 0's responses

\code{cat_stop_default} is a three-way stopping rule. When \code{stop_se}
is set in the options, it uses the standard error stopping rule. When
\code{stop_mi} is set in the options, it uses the minimum information stopping rule. When
\code{stop_cut} is set in the options, it uses the confidence interval (set by \code{ci_width})
stopping rule.

\code{cat_select_maxinfo} is the maximum information selection rule. Use \code{group}
(a numeric vector) to group items belonging to the same set. Use \code{info_random} to implement
the random-esque item exposure control method.

\code{cat_select_ccat} is the constrained CAT selection rule. Use
\code{ccat_var} to set the content variable in the pool. Use \code{ccat_perc} to set
the desired content distribution, with the name of each element being the content code
and tue value of each element being the percentage. Use \code{ccat_random} to add randomness
to initial item selections.

\code{cat_select_shadow} is the shadow-test selection rule. Use \code{shadow_id}
to group item sets. Use \code{constraints} to set constraints. Constraints should be in a data.frame
with four columns: var (variable name), level (variable level, \code{NA} for quantitative variable), 
min (lower bound), and max (upper bound).

\code{cat_stop_projection} is the projection-based stopping rule. Use 
\code{projection_method} to choose the projection method ('info' or 'diff'). Use
\code{stop_cut} to set the cut score. Use \code{constraints} to set the constraints.
Constraints should be a data.frame with columns: var (variable name), 
level (variable level, \code{NA} for quantitative varialbe), min (lower bound), max (upper bound)
}
\details{
\code{...} takes a variety of option/control parameters for the simulations from users. 
\code{min} and {max} are mandatory for setting limits on the test length. User-defined
selection, estimation, and stopping rules are also passed to the simulator via options.\cr
To write a new rule, the function siganiture must be: \code{function(len, theta, stats, admin, pool, opts)}.
See built-in rules for examples.
}
\examples{
\dontrun{
## generate a 100-item pool
num_items <- 100
pool <- with(model_3pl_gendata(1, num_items), data.frame(a=a, b=b, c=c))
pool$set_id <- sample(1:30, num_items, replace=TRUE)
pool$content <- sample(1:3, num_items, replace=TRUE)
pool$time <- round(rlnorm(num_items, mean=4.1, sd=.2))

## MLE, EAP, and hybrid estimation rule
cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_mle)
cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_eap)
cat_sim(1.0, pool, min=10, max=20, estimate_rule=cat_estimate_hybrid)

## SE, MI, and CI stopping rule
cat_sim(1.0, pool, min=10, max=20, stop_se=.3)
cat_sim(1.0, pool, min=10, max=20, stop_mi=.6)
cat_sim(1.0, pool, min=10, max=20, stop_cut=0)
cat_sim(1.0, pool, min=10, max=20, stop_cut=0, ci_width=2.58)

## maximum information selection with item sets
cat_sim(1.0, pool, min=10, max=20, group="set_id")$admin

## maximum information with item exposure control
cat_sim(1.0, pool, min=10, max=20, info_random=5)$admin

## Constrained-CAT selection rule with and without initial randomness
cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, 
        ccat_var="content", ccat_perc=c("1"=.2, "2"=.3, "3"=.5))
cat_sim(1.0, pool, min=10, max=20, select_rule=cat_select_ccat, ccat_random=5,
        ccat_var="content", ccat_perc=c("1"=.2, "2"=.3, "3"=.5))

## Shadow-test selection rule
cons <- data.frame(var='content', level=1:3, min=c(3,3,4), max=c(3,3,4))
cons <- rbind(cons, data.frame(var='time', level=NA, min=55*10, max=65*10))
cat_sim(1.0, pool, min=10, max=10, select_rule=cat_select_shadow, constraints=cons)

## Projection-based stopping rule
cons <- data.frame(var='content', level=1:3, min=5, max=15)
cons <- rbind(cons, data.frame(var='time', level=NA, min=60*20, max=60*40))
cat_sim(1.0, pool, min=20, max=40, select_rule=cat_select_shadow, stop_rule=cat_stop_projection, 
        projection_method="diff", stop_cut=0, constraints=cons)
}
}
