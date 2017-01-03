
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Sampling Distributions
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

postscript(file="fig/sampdist-Students-t-dist-vary-df.ps")
curve(dt(x, df = 30), from = -3, to = 3, lwd = 3, ylab = "y")
ind <- c(1, 2, 3, 5, 10)
for (i in ind) curve(dt(x, df = i), -3, 3, add = TRUE)
dev.off()

qt(0.01, df = 23, lower.tail = FALSE)

example(clt.examp)

example(illustrateCLT)

iqrs <- replicate(100, IQR(rnorm(100)))

mean(iqrs)    # close to 1

sd(iqrs)

postscript(file="fig/sampdist-simulated-IQR.ps")
hist(iqrs, breaks = 20)
dev.off()

mads <- replicate(100, mad(rnorm(100)))

mean(mads)    # close to 1.349

sd(mads)

postscript(file="fig/sampdist-simulated-MAD.ps")
hist(mads, breaks = 20)
dev.off()

k <- 1
n <- sample(10:30, size=10, replace = TRUE)
mu <- round(rnorm(10, mean = 20))
