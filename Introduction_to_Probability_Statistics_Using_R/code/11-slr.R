
#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2014  G. Jay Kerns
#
#    Chapter: Simple Linear Regression
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
library(ggplot2)
library(HH)
library(lmtest)

postscript(file="fig/slr-philosophy.ps")
plot(c(0,5), c(0,6.5), type = "n", xlab="x", ylab="y")
abline(h = 0, v = 0, col = "gray60")
abline(a = 2.5, b = 0.5, lwd = 2)
x <- 600:3000/600
y <- dnorm(x, mean = 3, sd = 0.5)
lines(y + 1.0, x)
lines(y + 2.5, x + 0.75)
lines(y + 4.0, x + 1.5)
abline(v = c(1, 2.5, 4), lty = 2, col = "grey")
segments(1, 3, 1 + dnorm(0,0,0.5),3, lty = 2, col = "gray")
segments(2.5, 3.75, 2.5 + dnorm(0,0,0.5), 3.75, lty = 2, col = "gray")
segments(4,4.5, 4 + dnorm(0,0,0.5),4.5, lty = 2, col = "gray")
dev.off()

head(cars)

postscript(file="fig/slr-carscatter.ps")
plot(dist ~ speed, data = cars)
dev.off()

qplot(speed, dist, data = cars)

tmpcoef <- round(as.numeric(coef(lm(dist ~ speed, cars))), 2)

cars.lm <- lm(dist ~ speed, data = cars)

coef(cars.lm)

postscript(file="fig/slr-carline.ps")
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(shape = 19) + 
  geom_smooth(method = lm, se = FALSE)
dev.off()

cars[5, ]

fitted(cars.lm)[1:5]

predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))

residuals(cars.lm)[1:5]

tmpred <- round(as.numeric(predict(cars.lm, newdata = data.frame(speed = 8))), 2)
tmps <- round(summary(cars.lm)$sigma, 2)

carsumry <- summary(cars.lm)
carsumry$sigma

A <- matrix(as.numeric(round(carsumry$coef, 3)), nrow = 2)
B <- round(confint(cars.lm), 3)

summary(cars.lm)

confint(cars.lm)

new <- data.frame(speed = c(5, 6, 21))

predict(cars.lm, newdata = new, interval = "confidence")

carsCI <- round(predict(cars.lm, newdata = new, interval = "confidence"), 2)

predict(cars.lm, newdata = new, interval = "prediction")

carsPI <- round(predict(cars.lm, newdata = new, interval = "prediction"), 2)

library(HH)
ci.plot(cars.lm)

postscript(file="fig/slr-carscipi.ps")
print(ci.plot(cars.lm))
dev.off()

summary(cars.lm)

anova(cars.lm)

carsumry$r.squared

sqrt(carsumry$r.squared)

anova(cars.lm)

tmpf <- round(as.numeric(carsumry$fstatistic[1]), 2)

postscript(file="fig/slr-Normal-q-q-plot-cars.ps")
plot(cars.lm, which = 2)
dev.off()

shapiro.test(residuals(cars.lm))

postscript(file="fig/slr-std-resids-fitted-cars.ps")
plot(cars.lm, which = 3)
dev.off()

bptest(cars.lm)

postscript(file="fig/slr-resids-fitted-cars.ps")
plot(cars.lm, which = 1)
dev.off()

dwtest(cars.lm, alternative = "two.sided")

sres <- rstandard(cars.lm)
sres[1:5]

sres[which(abs(sres) > 2)]

sdelres <- rstudent(cars.lm)
sdelres[1:5]

t0.005 <- qt(0.005, df = 47, lower.tail = FALSE)
sdelres[which(abs(sdelres) > t0.005)]

leverage <- hatvalues(cars.lm)
leverage[which(leverage > 4/50)]

dfb <- dfbetas(cars.lm)
head(dfb)

dff <- dffits(cars.lm)
dff[1:5]

cooksD <- cooks.distance(cars.lm)
cooksD[1:4]

postscript(file="fig/slr-Cooks-distance-cars.ps")
plot(cars.lm, which = 4)
dev.off()

F0.50 <- qf(0.5, df1 = 2, df2 = 48)
any(cooksD > F0.50)

influence.measures(cars.lm)

plot(cars.lm)

postscript(file="fig/slr-Diagnostic-plots-cars.ps")
par(mfrow = c(2,2))
plot(cars.lm)
par(mfrow = c(1,1))
dev.off()

plot(cars.lm, which = 5)          # std'd resids vs lev plot
identify(leverage, sres, n = 4)   # identify 4 points
