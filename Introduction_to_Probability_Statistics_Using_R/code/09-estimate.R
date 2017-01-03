
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Estimation
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
library(TeachingDemos)
library(Hmisc)
library(RcmdrPlugin.IPSUR)
library(reshape)

postscript(file="fig/estimate-capture-recapture.ps")
heights = rep(0, 16)
for (j in 7:15) heights[j] <- dhyper(3, m = 7, n = j - 7, k = 4)
plot(6:15, heights[6:15], pch = 16, cex = 1.5, xlab = "number of fish in pond", ylab = "Likelihood")
abline(h = 0)
lines(6:15, heights[6:15], type = "h", lwd = 2, lty = 3)
text(9, heights[9]/6, bquote(hat(F)==.(9)), cex = 2, pos = 4)
lines(9, heights[9], type = "h", lwd = 2)
points(9, 0, pch = 4, lwd = 3, cex = 2)
dev.off()

postscript(file="fig/estimate-fishing-part-two.ps")
curve(x^5*(1-x)^2, 0, 1, xlab = "p", ylab = "L(p)")
curve(x^4*(1-x)^3, 0, 1, add = TRUE)
curve(x^3*(1-x)^4, 0, 1, add = TRUE)
dev.off()

postscript(file="fig/estimate-species-mle.ps")
dat <- rbinom(27, size = 1, prob = 0.3)
like <- function(x){
r <- 1
for (k in 1:27){ r <- r*dbinom(dat[k], size = 1, prob = x)}
return(r)
}
curve(like, from = 0, to = 1, xlab = "parameter space", ylab = "Likelihood", lwd = 3, col = "blue")
abline(h = 0, lwd = 1, lty = 3, col = "grey")
mle <- mean(dat)
mleobj <- like(mle)
lines(mle, mleobj, type = "h", lwd = 2, lty = 3, col = "red")
points(mle, 0, pch = 4, lwd = 2, cex = 2, col = "red")
text(mle, mleobj/6, substitute(hat(theta)==a, list(a=round(mle, 4))), cex = 2, pos = 4)
dev.off()

x <- mtcars$am
L <- function(p,x) prod(dbinom(x, size = 1, prob = p))
optimize(L, interval = c(0,1), x = x, maximum = TRUE)

A <- optimize(L, interval = c(0,1), x = x, maximum = TRUE)
amax <- A$maximum; aobj <- A$objective

minuslogL <- function(p,x){
                -sum(dbinom(x, size = 1, prob = p, log = TRUE))
             }
optimize(minuslogL, interval = c(0,1), x = x)

minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}

x <- PlantGrowth$weight
MaxLikeEst <- mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))
summary(MaxLikeEst)

mean(x); var(x)*29/30; sd(x)/sqrt(30)

postscript(file="fig/hypoth-ci-examp.ps")
ci.examp()
dev.off()

with(PlantGrowth, stem.leaf(weight))

dim(PlantGrowth)   # sample size is first entry

with(PlantGrowth, mean(weight))

qnorm(0.975)

temp <- with(PlantGrowth, z.test(weight, stdev = 0.7))
temp

plot(temp, "Conf")

binconf(x = 7, n = 25, method = "asymptotic")

binconf(x = 7, n = 25, method = "wilson")

data(RcmdrTestDrive)

tab <- xtabs(~gender, data = RcmdrTestDrive)
prop.test(rbind(tab), conf.level = 0.95, correct = FALSE)

A <- as.data.frame(Titanic)
B <- with(A, untable(A, Freq))
