# Does only 92 presidential prediction. 

prez88 <- read.table("prez48to88.txt", header = T)
prez92 <- read.table("prez92.txt", header = T)

y <- prez88[,1]
X <- as.matrix(prez88[, -(1:4)])
n <- nrow(X)
p <- ncol(X)


Xnew <- as.matrix(prez92[, -(1:4)])
nnew <- nrow(Xnew)
		   
year <- prez88$year

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

del <- par.samp[, p + 1 + (1:11)]
gam <- par.samp[, p + 1 + 11 + (1:44)]

shifts <- kronecker(t(rep(1,4)), del) + gam
shifts.CI <- apply(shifts, 2, quantile, p = pr)

par(mfrow = c(2,2), mar = c(5, 4, 6, 2) + 0.1)
start <- 0

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

print (pred.Dwin)


