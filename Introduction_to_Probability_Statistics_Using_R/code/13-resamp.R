
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Resampling Methods
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

srs <- rnorm(25, mean = 3)
resamps <- replicate(1000, sample(srs, 25, TRUE), simplify = FALSE)
xbarstar <- sapply(resamps, mean, simplify = TRUE)

postscript(file="fig/resamp-Bootstrap-se-mean.ps")
hist(xbarstar, breaks = 40, prob = TRUE)
curve(dnorm(x, 3, 0.2), add = TRUE) # overlay true normal density
dev.off()

mean(xbarstar)
mean(srs)
mean(xbarstar) - mean(srs)

sd(xbarstar)

resamps <- replicate(1000, sample(rivers, 141, TRUE), simplify = FALSE)
medstar <- sapply(resamps, median, simplify = TRUE)
sd(medstar)

postscript(file="fig/resamp-Bootstrapping-se-median.ps")
hist(medstar, breaks = 40, prob = TRUE)
dev.off()

hist(medstar, breaks = 40, prob = TRUE)

median(rivers)
mean(medstar)
mean(medstar) - median(rivers)

mean_fun <- function(x, indices) mean(x[indices])
boot(data = srs, statistic = mean_fun, R = 1000)

median_fun <- function(x, indices) median(x[indices])
boot(data = rivers, statistic = median_fun, R = 1000)

btsamps <- replicate(2000, sample(stack.loss, 21, TRUE), simplify = FALSE)
thetast <- sapply(btsamps, median, simplify = TRUE)
mean(thetast)
median(stack.loss)
quantile(thetast, c(0.025, 0.975))

med_fun <- function(x, ind) median(x[ind])
med_boot <- boot(stack.loss, med_fun, R = 2000)
boot.ci(med_boot, type = c("perc", "norm", "bca"))

oneway_test(len ~ supp, data = ToothGrowth)

t.test(len ~ supp, data = ToothGrowth, 
       alt = "greater", var.equal = TRUE)

A <- show(oneway_test(len ~ supp, data = ToothGrowth))
B <- t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)
