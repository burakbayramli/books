### input the data
#counts <- c(35,59,47,112,42,77,26,76)
counts <- c(35,42,59,77,47,26,112,76)
x2 <- c(0,0,0,0,1,1,1,1)
x3 <- c(0,0,1,1,0,0,1,1)
x1 <- c(0,1,0,1,0,1,0,1)

### look at the data; bind the vectors together as columns
print(cbind(counts,x1,x2,x3))
### tell S that x1, x2 and x3 are discrete random variables
center <- as.factor(x1)
grade <- as.factor(x2)
survival <- as.factor(x3)

out <- glm(counts ~ center*grade*survival,family=poisson)
print(summary(out,correlation=F))

### choose the best sub-model; show the search procedure and final model
final <- step(out)
print(summary(final,correlation=F))
residuals <- (counts - final$fitted.values)/sqrt(final$fitted.values)

postscript("loglin.ps")
plot(1:length(counts),residuals,type="h")
dev.off()


