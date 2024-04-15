### Computational model for PSI: contemporary US
data <- read.csv("PSImodel2020.csv")

####   Variables
# ProdWage = nominal hourly wage of production workers (source: MeasuringWorth, Officer and Williamson 2013)
# UnskillWage = nominal wage of production workers (source: MeasuringWorth, scaled to 1 in 1860)
# GDPpc = nominal GDP per capita in nominal (source: MeasuringWorth)
# RelWage = relative wage, to be calculated 
# Urbanization = percent of population in urban places (source: Hist Stats of the US, Carter et al. 2004)
# A20_29 = percent of population in the age cohort 20-29 years (source: US Census Bureau, includes the forecast to 2020)
# RelDebt = national debt scaled by GDP (source: US Dept of the Treasury)
# Distrust = percent responding negatively to the government trust question (source: Pew Research Center)

################    Parameters
w_0 = 1
mu_0 = 0.1
lambda = 0.5 

################    Calculate RelWage, averaging production and unskilled wages, scaled to 1 in 1980
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

print (data$elite)



