# Tokdar's original file

source("utility.r")

## Get Data

prez88 <- read.table("prez48to88.txt", header = T)
prez92 <- read.table("prez92.txt", header = T)

## Get y, X 

y <- prez88[,1]
X <- as.matrix(prez88[, -(1:4)])
n <- nrow(X)
p <- ncol(X)

## Least square calculations

beta.ls <- lm(y ~ 0 + X)$coeff
V <- crossprod(X)
resid.ls <- y - c(X %*% beta.ls)
s2 <- sum(resid.ls^2) / (n - p)

## Xnew from 1992 data

Xnew <- as.matrix(prez92[, -(1:4)])
nnew <- nrow(Xnew)

## Spread and mean of p(ynew | y)

H <- diag(nnew) + Xnew %*% solve(V, t(Xnew))
sh <- sqrt(s2 * diag(H))
mean.ynew <- c(Xnew %*% beta.ls)

## Quantiles -- note: median = mean.

pr <- c(0.025, 0.25, 0.5, 0.75, 0.975)
ynew.CI <- mean.ynew + outer(sh, qt(pr, df = n - p))

## Map plots of predictive mean ( = median)

ynew.mean <- ynew.CI[, 3]
st.col <- rgb(1 - ynew.mean, 0, ynew.mean) # color coding in red / blue

## library(maps)
library(maps, lib.loc = "~/public_html/ABM724/maps")

map.ix <- read.table("usamap_ix.txt")[,1]      # this index vector is needed for 'map'
map("state", fill = T, col = st.col[map.ix])
legendUSmap(0.1)
title(main = "OLR : predicted Dvote share in 1992")


## Map of Pr(Dvot share > 0.5 | data)

x11()
pr.Dmajor <- pt((0.5 - mean.ynew) / sh, df = n - p, lower.tail = F)
st.col <- rgb(1 - pr.Dmajor, 0, pr.Dmajor)
map("state", fill = T, col = st.col[map.ix])
legendUSmap(0.1)
title("OLR : predicted prob of Democrats winning in 1992")


###################################
# Changing the prior distribution #
###################################

#
# Shall use a non-conjugate specification
# p(beta, sigmaSq) = N(beta | 0, tauSq0 * diag(p)) x InvChiSq(sigmaSq | nu0, sigmaSq0)
#

## First get samples from the posterior with the default prior: p(beta, sigmaSq) = 1 / sigmaSq
## Notice that p(beta, InvsigmaSq) = 1 / InvsigmaSq

nsamp <- 1e4
inv_sigmaSq <- rgamma(nsamp, (n - p) / 2, (n - p) * s2 / 2)
Vchol <- chol(V)
z <- scale(matrix(rnorm(nsamp * p), ncol = nsamp), center = FALSE, scale = sqrt(inv_sigmaSq))
beta <- beta.ls + backsolve(Vchol, z) ## each column is a sample from the posterior

ynew <- Xnew %*% beta + scale(matrix(rnorm(nnew * nsamp), ncol = nsamp), center = FALSE, scale = sqrt(inv_sigmaSq))

## function to get prediction for a new prior 
## by reweighting the sample from the default analysis

pred <- function(tauSq0, nu0, sigmaSq0){

  log.wt <- (dgamma(inv_sigmaSq, nu0 / 2, nu0 * sigmaSq0 / 2, log = TRUE)
             + log(inv_sigmaSq)
             - 0.5 * colSums(beta^2) / tauSq0)

  wt <- exp(log.wt - max(log.wt))
  wt <- wt / sum(wt)

  pDemWin <- apply(ynew > 0.5, 1, weighted.mean, w = wt)
  cat("ESS = ", 1 / sum(wt^2), "\n")
  return(pDemWin)
}


		   
		   
## Informal graphical checks of residuals

year <- prez88$year

x11()
par(mfrow = c(2,2))
nsamp <- 4
sig2 <- 1 / rgamma(nsamp, (n - p) / 2, (n - p) * s2 / 2)
beta.z <- solve(chol(crossprod(X)), matrix(rnorm(nsamp * p), nrow = p))
beta <- beta.ls +  beta.z %*% diag(sqrt(sig2))
resid <- y - X %*% beta

# If Partisan effects were present, then errors from the same
# election year will tend to have same sign:

for(i in 1:4){
  sres <- split(resid[,i], as.factor(year))
  boxplot(sres)
  abline(h = 0)
}
title(main = "residuals grouped by year - samples from posterior", outer = T, line = -2)

# Check normality assumption 

x11()
par(mfrow = c(2,2))
for(i in 1:4){
  qqnorm(c(resid[,i]) / sqrt(sig2[i]), main = "")
  abline(0, 1, col = "gray")
}
title(main = "residuals: normal QQplot - samples from posterior", outer = T, line = -2)



################################
## Including partisan effects ##
################################

regions <- list(Northeast = c(7, 8, 19, 20, 21, 29, 30, 32, 38, 39, 45, 48),
                South = c(1, 4, 9, 10, 17, 18, 24, 33, 36, 40, 42, 43, 46),
                Midwest = c(13, 14, 15, 16, 22, 23, 25, 27, 34, 35, 41, 49),
                West = c(2,  3,  5,  6, 11, 12, 26, 28, 31, 37, 44, 47, 50))

st2regn <- rep(NA, 50)
for(i in 1:4)
  st2regn[regions[[i]]] <- i

x11()
map("state", fill = T, col = st2regn[map.ix])
legend(-125, 33, names(regions), fill = 1:4, bty = "n")
title(main = "states grouped as regions")

year <- prez88$year
state <- prez88$state

t <- (year - 1948)/4 + 1      ## It is not recommended to use "t" as a variable
r <- st2regn[state]           ## but I'd do it here to be compatible with
rt <- 11*(r - 1) + t          ## notations used in class

X.del <- diag(11)[t, ]        ## The X_delta design matrix 
n.t <- apply(X.del, 2, sum)

X.gam <- diag(44)[rt, ]
n.rt <- apply(X.gam, 2, sum)  ## The X_gamma design matrix


## QR decomposition of X -- useful for repeated use of least squares

X.qr <- qr(X)     ## X = Q %*% R, where Q is orthonormal and R is upper triang
R <- qr.R(X.qr)   ## t(X) %*% X = t(R) %*% R -- so to generate from N(0, (X'X)^{-1})
                  ## we can use backsolve(R, rnorm(p))


##################
# Gibbs sampler ##
##################


prez.gibbs <- function(pars = NULL, nu.d, tau2.0d, nu.g, tau2.0g, n.sweep = 1e3){

  if(!is.null(pars)){
    beta <- pars[1:p]
    resid <- y - c(X %*% beta)
    sig2 <- pars[p + 1]
    del <- pars[p + 1 + (1:11)]
    gam <- pars[p + 12 + (1:44)]
    tau2.d <- pars[p + 57]
    tau2.g <- pars[p + 57 + (1:4)]

  } else{
    beta <- as.numeric(lm(y ~ 0 + X)$coeff)
    resid <- y - c(X %*% beta)
    sig2 <- sum(resid^2) / (n - p)
    del <- rep(0, 11)
    gam <- rep(0, 44)
    tau2.d <- 1
    tau2.g <- rep(1, 4)
  }

  par.store <- matrix(NA, nrow = n.sweep, ncol = p + 1 + 11 + 44 + 5)

  for(iter in 1:n.sweep){
  
    #----------------------
    # Update sig2 and beta
    #----------------------

    y.b <- y - del[t] - gam[rt]   ## modified response for beta
    beta.ls <- as.numeric(qr.coef(X.qr, y.b))
    res.b <- qr.resid(X.qr, y.b)
    s2.b <- sum(res.b^2) / (n - p)

    sig2 <- 1 / rgamma(1, (n - p)/2, (n - p) * s2.b / 2)
    beta <- beta.ls + sqrt(sig2) * backsolve(R, rnorm(p))

    fit.b <- c(X %*% beta)

    #--------------
    # Update delta
    #--------------

    y.d <- y - fit.b - gam[rt]   ## modified response for delta
    bar.y.d <- apply(y.d * X.del, 2, sum) / n.t

    del.mean <- (n.t * bar.y.d / sig2) / (n.t / sig2 + 1 / tau2.d)
    del.var <- 1 / (n.t / sig2 + 1 /tau2.d)

    del <- rnorm(11, del.mean, sqrt(del.var) )

    #--------------
    # Update tau2.d
    #--------------

    tau2.d <- 1 / rgamma(1, (11 + nu.d) / 2, (nu.d * tau2.0d + sum(del^2))/2)


    #--------------
    # Update gamma
    #--------------

    y.g <- y - fit.b - del[t]  ## modified response fo gamma
    bar.y.g <- apply(y.g * X.gam, 2, sum) / n.rt

    gam.mean <- (n.rt * bar.y.g / sig2) / (n.rt / sig2 + 1 / rep(tau2.g, each = 11))
    gam.var <- 1 / (n.rt / sig2 + 1 / rep(tau2.g, each = 11))

    gam <- rnorm(44, gam.mean, sqrt(gam.var) )

    #--------------
     # Update tau.g
    #--------------

    tau2.g <- 1 / rgamma(4, (11 + nu.g) / 2, (nu.g * tau2.0g + apply(matrix(gam, 11, 4)^2, 2, sum)) / 2)

    #--------
    # Store
    #--------

    par.store[iter, ] <- c(beta, sig2, del, gam, tau2.d, tau2.g)
  }

  return(par.store) 

}

## run sampler

prez.mc <- prez.gibbs(nu.d = -1, tau2.0d = 0, nu.g = -1, tau2.0g = 0, n.sweep = 4e3)


# Retain last half

last <- seq(2e3 + 10, 4e3, 10)
pr <- c(0.025, 0.25, 0.5, 0.75, 0.975)
par.samp <- prez.mc[last, ]


## Plot credible intervals for regional partisan shifts = del[t] + gam[rt]

del <- par.samp[, p + 1 + (1:11)]
gam <- par.samp[, p + 1 + 11 + (1:44)]

shifts <- kronecker(t(rep(1,4)), del) + gam
shifts.CI <- apply(shifts, 2, quantile, p = pr)

x11()
par(mfrow = c(2,2), mar = c(5, 4, 6, 2) + 0.1)
start <- 0
for(i in 1:4){
  plot(year, 0 * year, ty = "n", ylim = range(shifts.CI[, start + 1:11]), ann = F)
  title(main = names(regions)[i])
  for(j in 1:11)
    fivept(shifts.CI[,start + j], x = 1944 + 4 * j, max.wd = 4 * 0.33)
  abline(h = 0, lty = 3)
  start <- start + 11
}
title(main = "Regional partisan effect by year", outer = T, line = -2)

## Get posterior predictive probability of 
## Dshare > 0.5 in 1992 (separately for each state)
##
## Notice that 
## p(ynew_s | y, beta, sig2, del, gam) 
##     = integral N(ynew_s | xnew_s'beta, sig2 + tau2.d + tau2.g[r(s)])

getprob <- function(pars){
  beta <- pars[1:p]
  sig2 <- pars[p + 1]
  tau2.d <- pars[p + 57]
  tau2.g <- pars[p + 57 + (1:4)]

  return(pnorm(0.5, c(Xnew %*% beta), sqrt(sig2 + tau2.d + tau2.g[st2regn]), lower.tail = F))
}

prob.samp <- apply(par.samp, 1, getprob)
pred.Dwin <- apply(prob.samp, 1, mean)
st.col <- rgb(1 - pred.Dwin, 0, pred.Dwin)

x11()
map("state", fill = T, col = st.col[map.ix])
legendUSmap(0.1)
title(main = "predicted prob of Dem winning in 1992")



#####################################
#                                   # 
# Convergence diagnostics via PSRF  #
#                                   #
#####################################


## Variables of interest? 
## We will take phi = Prob of winning in 1992
## Other choices are Xnew %*% beta, or the parameters 
## themselves

out <- list()
phi <- list()


## get overdispersed starting values -- 
## we will simply overdisperse on  beta and hope that
## would automatically overdisperse the variable of
## interest.

## Look at the posterior distribution of beta in the
## the OLR version. 

sig2.start <- 1 / rgamma(5, (n - p) / 2, (n - p) * s2 / 2)
sd.beta <- sqrt(diag(solve(V)))
beta.start <- beta.ls + 10 * sd.beta * matrix(rnorm(5 * p), ncol = 5) %*% diag(sqrt(sig2.start))
del.start <- matrix(0, 11, 5)
gam.start <- matrix(0, nrow = 44, ncol = 5)
pars.start <- rbind(beta.start, sig2.start, del.start, gam.start, matrix(1e-5, 5, 5))

for(i in 1:5){
  prez.mc <- prez.gibbs(pars = pars.start[, i], nu.d = -1, tau2.0d = 0, nu.g = -1, tau2.0g = 0, n.sweep = 4e3)
  par.samp <- prez.mc[last, ]
  phi[[i]] <- apply(par.samp, 1, getprob)
  out[[i]] <- par.samp
  cat(i, " ")
}
cat("\n")

logit <- function(p)
  return(log(p) - log(1 - p))

## plot of 4 randomly selected state pairs

x11()
st.ix <- replicate(4, sample(50, size = 2))
phi.start <- apply(pars.start, 2, getprob)
par(mfrow = c(2,2))
for(j in 1:4){
  plot(0, 0, ty = "n", ylim = c(-10,10), xlim = c(-10,10), ann = F)
  for(i in 1:5){
    lines(logit(t(cbind(phi.start[st.ix[,j], i], phi[[i]][st.ix[,j], ]))), col = i)
    points(logit(t(phi.start[st.ix[,j], i])), pch = 19, col = i)
  }
}

## function to compute PSRF for state i

getPSRF <- function(i, n.chain, ch.len){
  phi.i <- matrix(NA, n.chain, ch.len)
  for(j in 1:n.chain)
    phi.i[j, ] <- phi[[j]][i, ]

  psi <- apply(phi.i, 1, mean)
  s2 <- apply(phi.i, 1, var)
  W <- mean(s2)
  B <- ch.len * var(psi)
  return(sqrt(((1 - 1 / ch.len) * W + ((n.chain + 1) / (n.chain * ch.len))* B) / W))
#       1    234              3       45           4   5                43   2    10         
}


## Use above function to get the PSRF values

PSRF <- sapply(1:50, getPSRF, n.chain = 5, ch.len = 200)
print(PSRF) ## want each < 1.1

## pooling the chains

phi.pool <- matrix(NA, 50, 1000)
for(i in 1:5)
  phi.pool[, 200 * (i - 1) + (1:200)] <- phi[[i]]

pred.Dwin <- apply(phi.pool, 1, mean)
st.col <- rgb(1 - pred.Dwin, 0, pred.Dwin)

x11()
map("state", fill = T, col = st.col[map.ix])
legendUSmap(0.1)
title(main = "predicted prob of Dem winning in 1992")








