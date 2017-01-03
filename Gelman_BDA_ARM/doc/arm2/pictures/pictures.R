# picture of hypothetical data for regression

postscript ("c:/books/multilevel/regression.ps", horizontal=F)
plot (c(0,8), c(0,20), type="n", bty="O", xlab="", ylab="", xaxt="n",
  yaxt="n", xaxs="i", yaxs="i")
lines (c(1,1), c(0,20))
lines (c(0,8), c(5,5))
data <- array (rnorm (20*8, 2, 1), c(20,8))
data[,1] <- 1
data[,3] <- ifelse (data[,3]<2,1,-1)
data[,4] <- data[,2]*data[,3]
for (i in 2:8){
  for (j in 1:20){
    text (i-.5, j-.5, round(data[j,i-1], 2), cex=1.5)
  }
}
for (i in 1){
  for (j in 1:5){
    text (i-.5, j-.5, "?", cex=1.5)
  }
  for (j in 6:20){
    text (i-.5, j-.5, fround(rnorm(1,1,1),1), cex=1.5)
  }
}
text (4.5, 2.7, expression (tilde(X)), cex=6.5)
text (.5, 2.7, expression (tilde(y)), cex=5)
text (4.5, 12.7, expression (X), cex=6.5)
text (.5, 12.7, expression (y), cex=5)
dev.off()
