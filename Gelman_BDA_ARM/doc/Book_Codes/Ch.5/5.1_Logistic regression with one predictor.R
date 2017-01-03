## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/nes

# The R codes & data files should be saved in the same directory for
# the source command to work

source("4.7_Fitting a series of regressions.R") # where data was cleaned; set the directory to be where this file is

yr <- 1992
  ok <- nes.year==yr & presvote<3
  vote <- presvote[ok] - 1
  income <- data$income[ok]

 # Estimation
fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
display(fit.1)

 # Graph figure 5.1 (a)
 curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), 1, 5, ylim=c(-.01,1.01),
         xlim=c(-2,8), xaxt="n", xaxs="i", mgp=c(2,.5,0),
         ylab="Pr (Republican vote)", xlab="Income", lwd=4)
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), -2, 8, lwd=.5, add=T)
  axis (1, 1:5, mgp=c(2,.5,0))
  mtext ("(poor)", 1, 1.5, at=1, adj=.5)
  mtext ("(rich)", 1, 1.5, at=5, adj=.5)
  points (jitter (income, .5), jitter (vote, .08), pch=20, cex=.1)

 # Graph figure 5.1 (b)
sim.1 <- sim(fit.1)
curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), .5, 5.5, ylim=c(-.01,1.01),
         xlim=c(.5,5.5), xaxt="n", xaxs="i", mgp=c(2,.5,0),
         ylab="Pr (Republican vote)", xlab="Income", lwd=1)
  for (j in 1:20){
    curve (invlogit(sim.1$coef[j,1] + sim.1$coef[j,2]*x), col="gray", lwd=.5, add=T)
  }
  curve (invlogit(fit.1$coef[1] + fit.1$coef[2]*x), add=T)
  axis (1, 1:5, mgp=c(2,.5,0))
  mtext ("(poor)", 1, 1.5, at=1, adj=.5)
  mtext ("(rich)", 1, 1.5, at=5, adj=.5)
  points (jitter (income, .5), jitter (vote, .08), pch=20, cex=.1)



