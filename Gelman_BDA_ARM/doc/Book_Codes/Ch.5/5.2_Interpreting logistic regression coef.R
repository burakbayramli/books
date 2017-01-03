## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

# The R codes & data files should be saved in the same directory for
# the source command to work

source("4.7_Fitting a series of regressions.R") # where data was cleaned

## Evaluation at the mean

invlogit(-1.40 + 0.33*mean(income, na.rm=T))

 # equivalently

fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
display (fit.1)
invlogit(coef(fit.1)[1] + coef(fit.1)[2]*mean(income, na.rm=T))

## Fitting and displaying the model

 # see file "5.1_Logistic regression with one predictor.R" for the commands
 # to plot figure 5.1 (a) & (b)

## Displaying the results of several logistic regressions

 # Figure 5.4

income.year <- NULL
income.coef <- NULL
income.se <- NULL
for (yr in seq(1952,2000,4)){
  ok <- nes.year==yr & presvote<3
  vote <- data$presvote[ok] - 1
  income <- data$income[ok]
  fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
  income.year <- c (income.year, yr)
  income.coef <- c (income.coef, fit.1$coef[2])
  income.se <- c (income.se, se.coef(fit.1)[2])
}

plot (income.year, income.coef, xlim=c(1950,2000), ylim=range(income.coef+income.se, 
    income.coef-income.se), mgp=c(2,.5,0), pch=20, ylab="Coefficient of income", xlab="Year")
for (i in 1:length(income.year)){
  lines (rep(income.year[i],2), income.coef[i]+income.se[i]*c(-1,1), lwd=.5)
}
abline (0,0,lwd=.5, lty=2)


