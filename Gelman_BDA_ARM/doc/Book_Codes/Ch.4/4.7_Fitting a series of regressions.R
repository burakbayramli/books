## Figure 4.6

 # Read in the data
library("arm")

brdata <- read.dta("nes5200_processed_voters_realideo.dta",convert.factors=F)
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

 # Clean the data
brdata <- brdata[is.na(brdata$black)==FALSE&is.na(brdata$female)==FALSE&is.na(brdata$educ1)==FALSE
&is.na(brdata$age)==FALSE&is.na(brdata$income)==FALSE&is.na(brdata$state)==FALSE,]
kept.cases <- 1952:2000
matched.cases <- match(brdata$year, kept.cases)
keep <- !is.na(matched.cases)
data <- brdata[keep,]
plotyear <- unique(sort(data$year))
year.new <- match(data$year,unique(data$year))
n.year <- length(unique(data$year))
income.new <-data$income-3
age.new <- (data$age-mean(data$age))/10
y <- data$rep_pres_intent
data <- cbind(data, year.new, income.new, age.new, y)
nes.year <- data[,"year"]
age.discrete <- as.numeric (cut (data[,"age"], c(0,29.5, 44.5, 64.5, 200)))
race.adj <- ifelse (data[,"race"]>=3, 1.5, data[,"race"])
data <- cbind (data, age.discrete, race.adj)

female <- data[,"gender"] - 1
black <- ifelse (data[,"race"]==2, 1, 0)
rvote <- ifelse (data[,"presvote"]==1, 0, ifelse(data[,"presvote"]==2, 1, NA))

region.codes <- c(3,4,4,3,4,4,1,1,5,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,
   1,3,2,3,3,4,1,3,4,1,2,4)
attach.all(data)

 # Regression & plot
regress.year <- function (yr) {
  this.year <- data[nes.year==yr,]
  lm.0 <- lm (partyid7 ~ real_ideo + race.adj + factor(age.discrete) + educ1 + gender + income,
      data=this.year)
  coefs <- summary(lm.0)$coef[,1:2]
}

summary <- array (NA, c(9,2,8))
for (yr in seq(1972,2000,4)){
  i <- (yr-1968)/4
  summary[,,i] <- regress.year(yr)
}
yrs <- seq(1972,2000,4)

coef.names <- c("Intercept", "Ideology", "Black", "Age.30.44", "Age.45.64", "Age.65.up", 
   "Education", "Female", "Income")

par (mfrow=c(2,5), mar=c(3,4,2,0))
for (k in 1:9){
  plot (range(yrs), range(0,summary[k,1,]+.67*summary[k,2,],summary[k,1,]-.67*summary[k,2,]), 
     type="n", xlab="year", ylab="Coefficient", main=coef.names[k], mgp=c(1.2,.2,0), cex.main=1,
      cex.axis=1, cex.lab=1, tcl=-.1)
  abline (0,0,lwd=.5, lty=2)
  points (yrs, summary[k,1,], pch=20, cex=.5)
  segments (yrs, summary[k,1,]-.67*summary[k,2,], yrs, summary[k,1,]+.67*summary[k,2,], lwd=.5)
}
