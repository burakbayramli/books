
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Discrete Distributions
#
#    This file is part of IPSUR.
#
#    IPSUR is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    IPSUR is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with IPSUR.  If not, see <http://www.gnu.org/licenses/>.

# This chapter's package dependencies
library(distrEx)

x <- c(0,1,2,3)
f <- c(1/8, 3/8, 3/8, 1/8)

mu <- sum(x * f)
mu

sigma2 <- sum((x-mu)^2 * f)
sigma2

sigma <- sqrt(sigma2)
sigma

F <- cumsum(f)
F

X <- DiscreteDistribution(supp = 0:3, prob = c(1,3,3,1)/8)
E(X); var(X); sd(X)

A <- data.frame(Pr=dbinom(0:4, size = 4, prob = 0.5))
rownames(A) <- 0:4 
A

pbinom(9, size=12, prob=1/6) - pbinom(6, size=12, prob=1/6)
diff(pbinom(c(6,9), size = 12, prob = 1/6))  # same thing

postscript(file="fig/discdist-binom-cdf-base.ps")
plot(0, xlim = c(-1.2, 4.2), ylim = c(-0.04, 1.04), type = "n", xlab = "number of successes", ylab = "cumulative probability")
abline(h = c(0,1), lty = 2, col = "grey")
lines(stepfun(0:3, pbinom(-1:3, size = 3, prob = 0.5)), verticals = FALSE, do.p = FALSE)
points(0:3, pbinom(0:3, size = 3, prob = 0.5), pch = 16, cex = 1.2)
points(0:3, pbinom(-1:2, size = 3, prob = 0.5), pch = 1, cex = 1.2)
dev.off()

X <- Binom(size = 3, prob = 1/2)
X

d(X)(1)   # pmf of X evaluated at x = 1
p(X)(2)   # cdf of X evaluated at x = 2

postscript(file="fig/discdist-binom-plot-distr.ps")
X <- Binom(size = 3, prob = 1/2)
plot(X, cex = 0.2)
dev.off()

X <- Binom(size = 3, prob = 0.45)
E(X)
E(3*X + 4)

var(X)
sd(X)

x <- c(4, 7, 9, 11, 12)
ecdf(x)

postscript(file="fig/discdist-empirical-CDF.ps")
plot(ecdf(x))
dev.off()

epdf <- function(x) function(t){sum(x %in% t)/length(x)}
x <- c(0,0,1)
epdf(x)(0)       # should be 2/3

x <- c(0,0,1)
sample(x, size = 7, replace = TRUE)

dhyper(3, m = 17, n = 233, k = 5)

A <- data.frame(Pr=dhyper(0:4, m = 17, n = 233, k = 5))
rownames(A) <- 0:4 
A

dhyper(5, m = 17, n = 233, k = 5)

phyper(2, m = 17, n = 233, k = 5)

phyper(1, m = 17, n = 233, k = 5, lower.tail = FALSE)

rhyper(10, m = 17, n = 233, k = 5)

pgeom(4, prob = 0.812, lower.tail = FALSE)

dnbinom(5, size = 7, prob = 0.5)

diff(ppois(c(47, 50), lambda = 50))
