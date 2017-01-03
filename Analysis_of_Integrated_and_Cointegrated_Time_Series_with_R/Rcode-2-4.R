library(fracdiff)
set.seed(123456)
y <- fracdiff.sim(n=1000, ar=0.0, ma=0.0, d=0.3)
y.spec <- spectrum(y$series, plot=FALSE)
lhs <- log(y.spec$spec)
rhs <- log(4*(sin(y.spec$freq/2))^2)
gph.reg <- lm(lhs ~ rhs)
gph.sum <- summary(gph.reg)
sqrt(gph.sum$cov.unscaled*pi/6)[2,2]
