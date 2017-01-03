# coefficient for black in 1964 to illustrate nonidentifiability of logistic regression for chap 5 hwk

attach.all (data)

# logistic regression of vote preference on income


postscript ("income1a.ps", horizontal=T, height=4, width=5)
yr <- 1992
  ok <- nes.year==yr & presvote<3
  vote <- presvote[ok] - 1
  income <- data$income[ok]
  fit.1 <- glm.all (vote ~ income, family=binomial(link="logit"))
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), 1, 5, ylim=c(-.01,1.01),
         xlim=c(-2,8), xaxt="n", xaxs="i", mgp=c(2,.5,0),
         ylab="Pr (Republican vote)", xlab="Income", lwd=4)
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), -2, 8, lwd=.5, add=T)
  axis (1, 1:5, mgp=c(2,.5,0))
  mtext ("(poor)", 1, 1.5, at=1, adj=.5)
  mtext ("(rich)", 1, 1.5, at=5, adj=.5)
  points (jitt (income, .3), jitt (vote, .04), pch=20, cex=.1)
dev.off()

postscript ("c:/books/multilevel/income1b.ps", horizontal=T, height=4, width=5)
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), .5, 5.5, ylim=c(-.01,1.01),
         xlim=c(.5,5.5), xaxt="n", xaxs="i", mgp=c(2,.5,0),
         ylab="Pr (Republican vote)", xlab="Income", lwd=1)
  for (j in 1:20){
    curve (invlogit(fit.1$beta[j,1] + fit.1$beta[j,2]*x), col="gray", lwd=.5, add=T)
  }
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), add=T)
  axis (1, 1:5, mgp=c(2,.5,0))
  mtext ("(poor)", 1, 1.5, at=1, adj=.5)
  mtext ("(rich)", 1, 1.5, at=5, adj=.5)
  points (jitt (income, .3), jitt (vote, .04), pch=20, cex=.1)
dev.off()

# series of regressions

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
  income.se <- c (income.se, fit.1$summary.coef[2,2])
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), ylim=c(0,1),
         xlim=c(.1,5.9), yaxs="i", xaxs="i",
         ylab="Pr (Republican vote)", xlab="Income")
  for (i in 1:5)
    lines (i + c(-.4,.4), rep(mean(vote[income==i],na.rm=T),2))
}

postscript ("c:/books/multilevel/incomeseries.ps", horizontal=T, height=4, width=5)
plot (income.year, income.coef, xlim=c(1950,2000), ylim=range(income.coef+income.se, income.coef-income.se), mgp=c(2,.5,0), pch=20,
         ylab="Coefficient of income", xlab="Year")
for (i in 1:length(income.year)){
  lines (rep(income.year[i],2), income.coef[i]+income.se[i]*c(-1,1), lwd=.5)
}
abline (0,0,lwd=.5, lty=2)
dev.off()

# using "black" as a predictor for hwk assignment in chap 5

vote <- rvote

for (yr in seq (1956,1972,4)){
#  glm.1 <- glm (rvote ~ female + black + factor(age.discrete) + educ1 + income, subset=(year==yr), family=binomial(link="logit"))
  glm.1 <- glm (vote ~ female + black + income, subset=(year==yr), family=binomial(link="logit"))
  display (glm.1)
}

glm.1 <- glm (rvote ~ black, subset=(nes.year==1960), family=binomial(link="logit"))
display (glm.1)
glm.1 <- glm (rvote ~ black, subset=(nes.year==1964), family=binomial(link="logit"))
display (glm.1)
glm.1 <- glm (rvote ~ black, subset=(nes.year==1968), family=binomial(link="logit"))
display (glm.1)
