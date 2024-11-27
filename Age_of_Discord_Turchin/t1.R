data <- read.csv("PSImodel2020.csv")

w_0 = 1
mu_0 = 0.1
lambda = 0.5 

RelWage1 <- data$ProdWage/data$GDPpc
RelWage1 <- RelWage1/RelWage1[data$year == 1980]
RelWage2 <- data$UnskillWage/data$GDPpc
RelWage2 <- RelWage2/RelWage2[data$year == 1980]
data$RelWage <- (RelWage1+RelWage2)/2

################    Extrapolations
### Extrapolate RelWage to 2020
RegrDat <- subset(data[data$year > 1960,], select = c(year, RelWage))
print( res <- summary(fit <- lm(RegrDat[2:1])) )
plot(RegrDat, pch=16, ylim=c(0.5,1.5))
abline(fit, lty=2, lwd=2)
legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
data$RelWage[data$year > 2011] <- data$RelWage[data$year == 2011] + fit$coefficients[2]*1:9
points(data$year, data$RelWage)

### Extrapolate RelDebt forward to 2020
RegrDat <- subset(data[data$year > 1980,], select = c(year, RelDebt))
print( res <- summary(fit <- lm(RegrDat[2:1])) )
plot(RegrDat, pch=16, ylim=c(0, 200))
abline(fit, lty=2, lwd=2)
legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
data$RelDebt[data$year > 2012] <- data$RelDebt[data$year == 2012] + fit$coefficients[2]*1:8
points(data$year, data$RelDebt)

### Extrapolate Distrust back from 1958 and forward to 2020
data$Distrust[data$year < 1958] <- data$Distrust[data$year == 1958]
RegrDat <- subset(data[data$year > 1980,], select = c(year, Distrust))
print( res <- summary(fit <- lm(RegrDat[2:1])) )
plot(RegrDat, pch=16, ylim=c(0, 100))
abline(fit, lty=2, lwd=2)
legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
data$Distrust[data$year > 2012] <- data$Distrust[data$year == 2012] + fit$coefficients[2]*1:8
points(data$year, data$Distrust)

########  Calculate e and epsilon
data <- data[data$year > 1944,]
data$elite[data$year == 1945] <- 1
for(t in 2:nrow(data)){
  data$elite[t] <- data$elite[t-1] + mu_0*(w_0 - data$RelWage[t-1])/data$RelWage[t-1]
}

data$epsilon <- (1 - lambda*data$RelWage)/data$elite
data$epsilon <- data$epsilon/data$epsilon[1]

### Convert percentages to proportions
data$Urbanization <- data$Urbanization/100
data$Age20_29 <- data$Age20_29/60  ### Discounting children and old
data$RelDebt <- data$RelDebt/100
data$Distrust <- data$Distrust/100

### Calculate and plot PSI
data$PSI <- 100*(1/data$RelWage)*data$Urbanization*data$Age20_29*(data$elite/data$epsilon)*data$RelDebt*data$Distrust 
gdat <- subset(data, select = c(year, PSI))
plot(gdat[gdat$year < 2012,], xlim=c(1958,2020), ylim=c(0,80), type="l", lwd=3, xlab="")
