### linear regression star data
postscript("linear.regress.star.eps",
            horizontal = FALSE, onefile = FALSE, paper ="special",
           width=6,height=6)
x <- scan("=hand.data/datasets/star.dat")
x <- matrix(x,ncol=6,byrow=T)
x1 <- x[,1:3];x2 <- x[,4:6];
x <- rbind(x1,x2)
x <- x[,-1]
x <- x[-48,]
y <- x[,1]
x <- x[,2]
n <- length(x)
o <- order(x)
x <- x[o]
y <- y[o]
I <- (1:n)[y >= 4.0]
x <- x[I]
y <- y[I]
n <- length(x)
plot(x,y,xlab="",ylab="", pch=20)
out <- lm(y ~ x)
abline(out,lwd=2,col=2)
print(summary(out))

dev.off()

