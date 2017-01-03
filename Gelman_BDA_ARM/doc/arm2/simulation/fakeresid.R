grades <- read.table ("gradesW4315.dat", header=T)
midterm <- grades[,"Midterm"]
final <- grades[,"Final"]
lm.1 <- lm (final ~ midterm)
display (lm.1)

n <- length (final)
X <- cbind (rep(1,n), midterm)
predicted <- X %*% beta.hat(lm.1)

resid <- final - predicted

postscript ("c:/books/multilevel/fakeresid1a.ps", height=3.8, width=4.5)
plot (predicted, resid, xlab="predicted value", ylab="residual", main="Residuals vs.\ predicted values", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis (2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline (0, 0, col="gray", lwd=.5)
dev.off()

postscript ("c:/books/multilevel/fakeresid1b.ps", height=3.8, width=4.5)
plot (final, resid, xlab="observed value", ylab="residual", main="Residuals vs.\ observed values", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis (2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline (0, 0, col="gray", lwd=.5)
dev.off()

# now simulate fake data


a <- 65
b <- 0.7
sigma <- 15
y.fake <- a + b*midterm + rnorm (n, 0, 15)
lm.fake <- lm (y.fake ~ midterm)

predicted.fake <- X %*% beta.hat(lm.fake)

resid.fake <- y.fake - predicted.fake

postscript ("c:/books/multilevel/fakeresid2a.ps", height=3.8, width=4.5)
plot (predicted.fake, resid.fake, xlab="predicted value", ylab="residual", main="Fake data:  resids vs.\ predicted", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis (2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline (0, 0, col="gray", lwd=.5)
dev.off()

postscript ("c:/books/multilevel/fakeresid2b.ps", height=3.8, width=4.5)
plot (y.fake, resid.fake, xlab="observed value", ylab="residual", main="Fake data:  resids vs.\ observed", mgp=c(1.5,.5,0), pch=20, yaxt="n")
axis (2, seq(-40,40,20), mgp=c(1.5,.5,0))
abline (0, 0, col="gray", lwd=.5)
dev.off()

