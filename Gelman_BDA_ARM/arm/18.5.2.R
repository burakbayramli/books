library ("arm")

heightweight <- read.fwf ("../doc/gelman/ARM_Data/earnings/wfw90.dat", widths=c(143,1,2,3))
height1 <- heightweight[,2]
height2 <- heightweight[,3]
weight <- heightweight[,4]
ok <- !is.na (height1+height2+weight) & weight<998 & height1<7 & height2<12
height <- 12*height1[ok] + height2[ok]
weight <- weight[ok]
n <- length(height)

C <- 200

censored <- weight>=C
# greater than 200 is = 200
weight.censored <- ifelse (censored,C,weight)
print (weight.censored)

## Regression
c.height <- height - mean(height)

# greater than 200 have NA
z <- ifelse (weight.censored==C, NA, weight.censored) 

## Gibbs sampler

## starting values
n.censored <- sum (censored)

# regression if exact values were known
x <- c.height

# impute missing values given fitted regression
rnorm.trunc <- function (n, mu, sigma, lo=-Inf, hi=Inf){
  p.lo <- pnorm (lo, mu, sigma)
  p.hi <- pnorm (hi, mu, sigma)
  u <- runif (n, p.lo, p.hi)
  return (qnorm (u, mu, sigma))
}

# puting it together in a loop
n.chains <- 3
n.iter <- 100
sims <- array (NA, c(n.iter, n.chains, 3 + n.censored))
dimnames (sims) <- list (NULL, NULL,
  c ("a", "b", "sigma", paste ("z[", (1:n)[censored], "]", sep="")))
for (m in 1:n.chains){
  z[censored] <- runif (n.censored, C, 2*C)
  for (t in 1:n.iter){
    lm.1 <- lm (z ~ x)
    sim.1 <- sim (lm.1, n.sims=1)
    a <- coef(sim.1)[1]
    b <- coef(sim.1)[2]
    sigma <- sim.1@sigma
    z[censored] <- rnorm.trunc (n.censored, a + b*x[censored], sigma, lo=C)
    sims[t,m,] <- c (a, b, sigma, z[censored])
  }
}

sims.bugs <- as.bugs.array (sims)
print (sims.bugs)
plot (sims.bugs)

