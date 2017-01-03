
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Continuous Distributions
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
library(actuar)

f <- function(x) 3*x^2
integrate(f, lower = 0.14, upper = 0.71)

g <- function(x) 3/x^3
integrate(g, lower = 1, upper = Inf)

f <- function(x) 3*x^2
X <- AbscontDistribution(d = f, low1 = 0, up1 = 1)
p(X)(0.71) - p(X)(0.14)

E(X); var(X); 3/80

pnorm(1:3) - pnorm(-(1:3))

g <- function(x) pnorm(x, mean = 100, sd = 15) - 0.99
uniroot(g, interval = c(130, 145))

temp <- round(uniroot(g, interval = c(130, 145))$root, 4)

qnorm(0.99, mean = 100, sd = 15)

qnorm(c(0.025, 0.01, 0.005), lower.tail = FALSE)

X <- Norm(mean = 0, sd = 1)
Y <- 4 - 3*X
Y

Y <- exp(X)
Y

W <- sin(exp(X) + 27)
W

p(W)(0.5)
W <- sin(exp(X) + 27)
p(W)(0.5)

postscript(file="fig/contdist-chisq-dist-vary-df.ps")
curve(dchisq(x, df = 3), from = 0, to = 20, ylab = "y")
ind <- c(4, 5, 10, 15)
for (i in ind) curve(dchisq(x, df = i), 0, 20, add = TRUE)
dev.off()

mgamma(1:4, shape = 13, rate = 1)

postscript(file="fig/contdist-gamma-mgf.ps")
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, 
     from=-0.1, to=0.1, ylab = "gamma mgf")
dev.off()
