# Some of the longer scripts used in the book
# `Introductory Time Series with R' 
# Copyright (c) 2009 P. Cowpertwait and A. Metcalfe
# 
# To use just paste the script you want into R
#

# Chapter 3, p52-53, Bass curve fitted using nls
T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <- nls(Sales ~ M * ( ((P+Q)^2 / P) * exp(-(P+Q) * T79) ) /
  (1+(Q/P)*exp(-(P+Q)*T79))^2, start = list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p+q) * Tdelt)
Bpdf <- m * ( (p+q)^2 / p ) * ngete / (1 + (q/p) * ngete)^2
plot(Tdelt, Bpdf, xlab = "Year from 1979", 
                    ylab = "Sales per year", type='l')
points(T79, Sales)
Bcdf <- m * (1 - ngete)/(1 + (q/p)*ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979", 
                    ylab = "Cumulative sales", type='l')
points(T79, Cusales)


# Chapter 7, p144
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3]) 
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6]) 
    {
       fit <- arima(x.ts, order = c(p,d,q),  
                          seas = list(order = c(P,D,Q), 
                          frequency(x.ts)), method = "CSS")
       fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
       if (fit.aic < best.aic) 
       {
         best.aic <- fit.aic
         best.fit <- fit
         best.model <- c(p,d,q,P,D,Q) 
       }
    }
  list(best.aic, best.fit, best.model)
}

# Chapter 7, p149, simulated GARCH model
set.seed(1)
alpha0 <- 0.1
alpha1 <- 0.4
beta1 <- 0.2
w <- rnorm(10000)
a <- rep(0, 10000)
h <- rep(0, 10000)
for (i in 2:10000) {
     h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i - 
         1]
     a[i] <- w[i] * sqrt(h[i])
 }
acf(a)
acf(a^2)

# Chapter 8, p162, calculation of fractionally differenced series etc
library(fracdiff)
set.seed(1)
fds.sim <- fracdiff.sim(10000, ar = 0.9, d = 0.4)
x <- fds.sim$series
fds.fit <- fracdiff(x, nar=1)

n <- length(x)
L <- 30
d <- fds.fit$d
fdc <- d
fdc[1] <- fdc
for (k in 2:L) fdc[k] <- fdc[k-1] * (d+1-k) / k 
y <- rep(0, L)
for (i in (L+1):n) {
    csm <- x[i]
    for (j in 1:L) csm <- csm + ((-1)^j) * fdc[j] * x[i-j]
    y[i] <- csm
  }
y <- y[(L+1):n]
z.ar <- ar(y)
ns <- 1 + z.ar$order
z <- z.ar$res [ns:length(y)]
par(mfcol = c(2, 2))
plot(as.ts(x), ylab = "x")
acf(x) ; acf(y) ; acf(z)

# Chapter 9, p184, spectrum of broken motor
www <- "http://www.massey.ac.nz/~pscowper/ts/imotor.txt"
imotor.dat <- read.table(www, header = T)
attach (imotor.dat)
xg.spec <- spectrum(good,   span = 9)
xb.spec <- spectrum(broken, span = 9)
freqg <- 400 * xg.spec$freq [4400:5600]
freqb <- 400 * xb.spec$freq [4400:5600]
plot(freqg, 10*log10(xg.spec$spec[4400:5600]), main = "",
    xlab = "Frequency (Hz)", ylab = "Current spectrum (dB)", type="l")
lines(freqb, 10 * log10(xb.spec$spec[4400:5600]), lty = "dashed")
sd(good)
sd(broken)

# Chapter 9, p186, plots for excavator series
www <- "http://www.massey.ac.nz/~pscowper/ts/zdd.txt"
zdotdot.dat <- read.table(www, header = T)
attach (zdotdot.dat)
www <- "http://www.massey.ac.nz/~pscowper/ts/vibdoswt.txt"
wt.dat <- read.table (www, header = T)
attach (wt.dat)
acceln.spec <- spectrum (Accelnz, span = sqrt(2 * length(Accelnz)))
Frequ <- 200 * acceln.spec$freq
Sord <- 2 * acceln.spec$spec / 200
Time <- (1:1000) / 200
layout (1:3)
plot (Time, Accelnz, xlab = "Time (s)",
                       ylab = expression(mm~ s^-2),
                       main = "Acceleration", type = "l")
plot (Frequ, Sord, main = "Spectrum", xlab = "Frequency (Hz)",
                     ylab = expression(mm^2~s^-4~Hz^-1), type = "l")
plot (Frequ, Weight, xlab = "Frequency (Hz)", 
                       main = "Weighting function", type = "l")
sd (Accelnz)
sqrt( sum(Sord * Weight) * 0.2 )

# Chapter 10, p205, investigation of difference equation approximation, 
# plot in Figure 10.1, etc.  Note that this one takes a while to simulate!
m <- 1; c <- 1; k <- 16.25; Delta <- 0.01
a0 <- m / Delta^2 + c / Delta + k
a1 <- -2 * m / Delta^2 - c / Delta; a2 <- m / Delta^2
n <- 100000
y <- c(0, 0); x <- c(0, 0)
set.seed(1)
for (i in 3:n) {
     x[i] <- x[i-1] - 0.5 * x[i-2] + rnorm(1)
     y[i] <- (-a1 * y[i-1] - a2 * y[i-2]) / a0 + x[i] / a0
  }
Sxx <- spectrum(x, span = 31)
Syy <- spectrum(y, span = 31)
Gemp <- sqrt( Syy$spec[1:5000] / Sxx$spec[1:5000] )
Freq <- Syy$freq[1:5000]
FreH <- Freq / Delta
Omeg  <- 2 * pi * Freq
OmegH <- 2 * pi * FreH
Gth <- sqrt( 1/( (k-m*OmegH^2)^2 + c^2*OmegH^2 ))
Gar <- 1 / abs( 1 + a1/a0 * exp(-Omeg*1i) + a2/a0 * exp(-Omeg*2i) )
plot(FreH, Gth, xlab = "Frequency (Hz)", ylab = "Gain", type="l")
lines(FreH, Gemp, lty = "dashed")
lines(FreH, Gar, lty = "dotted")

# Chapter 10, p206-207, tugboat series, plot in Figure 10.3 etc
www <- "http://www.massey.ac.nz/~pscowper/ts/leg4.dat"
tug.dat <- read.table(www, header = T)
attach(tug.dat)
Heave.spec <- spectrum( Heave, span = sqrt( length(Heave) ), 
                                  log = c("no"), main = "" )
Wave.spec  <- spectrum( Wave,  span = sqrt( length(Heave) ), 
                                  log = c("no"), main = "" )
G <- sqrt(Heave.spec$spec/Wave.spec$spec)
par(mfcol = c(2, 2))
plot( as.ts(Wave) )
acf(Wave)
spectrum(Wave, span = sqrt(length(Heave)), log = c("no"), main = "")
plot(Heave.spec$freq, G, xlab="frequency Hz", ylab="Gain", type="l")


# Chapter 12, p234, Kalman filter, plot in Figure 12.1, etc
library(sspir)
set.seed(1)
Plummet.dat <- 20 + 2*rnorm(20) + c(rep(0,10), rep(-10,10))
n <- length(Plummet.dat)
Plummet.mat <- matrix(Plummet.dat, nrow = n, ncol = 1)
m1 <- SS(y = Plummet.mat,
           Fmat = function(tt,x,phi) return( matrix(1) ),
           Gmat = function(tt,x,phi) return( matrix(1) ),
           Wmat = function(tt,x,phi) return( matrix(0.1)),
           Vmat = function(tt,x,phi) return( matrix(2) ),
           m0   = matrix(25), C0 = matrix(10)
          )
plot(m1$y, ylab = "Closing price", main = "Simulated")
m1.f <- kfilter(m1)
m1.s <- smoother(m1.f)
lines(m1.f$m, lty = 2)
lines(m1.s$m, lty = 3)

# Chapter 12, regression with time varying coefficients, Figure 12.3
library(sspir)
set.seed(1)
x1 <- 1:30
x1 <- x1/10 + 2
a <- c(rep(4,15), rep(5,15)) 
b <- c(rep(2,15), rep(-1,15))
n <- length(x1)
y1 <- a + b * x1 + rnorm(n)
x0 <- rep(1, n)
xx <- cbind(x0, x1)
x.mat <- matrix(xx, nrow = n, ncol = 2)
y.mat <- matrix(y1, nrow = n, ncol = 1)
m1 <- SS(y = y.mat, x = x.mat,
         Fmat = function(tt,x,phi) 
         return( matrix(c(x[tt,1], x[tt,2]), nrow = 2, ncol = 1)),
         Gmat = function(tt,x,phi) return (diag(2)),
         Wmat = function(tt, x, phi) return (0.1*diag(2)),
         Vmat = function(tt,x,phi) return (matrix(1)),
         m0 = matrix(c(5,3),nrow=1,ncol=2),C0=10*diag(2) 
         )

m1.f <- kfilter(m1)
par(mfcol=c(2,1))
plot(m1.f$m[,1], type='l')
plot(m1.f$m[,2], type='l')

# Chapter 12, p241-242, Murray River series, Figures 12.5 and 12.6
library(sspir)
www <- 'http://www.massey.ac.nz/~pscowper/ts/Murray.txt'
Salt.dat <- read.table(www, header=T) ; attach(Salt.dat)
n <- 81 ; Time <- 1:n
SIN  <- sin(2 * pi * Time /12)[-1]
COS  <- cos(2 * pi * Time /12)[-1]
Chowilla <- Chowilla - mean(Chowilla)
Flow <- Flow - mean(Flow)
Chow <- Chowilla[-1]
Chow.L1 <- Chowilla[-n]
Flo <- Flow[-1]
Flo.L1 <- Flow[-n]
Sal.mat <- matrix(c(Chow, Flo), nrow = 80, ncol = 2)
x0 <- rep(1, (n-1))
xx <- cbind(x0, Chow.L1, Flo.L1, COS, SIN)
x.mat <- matrix(xx, nrow = n-1, ncol = 5)
G.mat <- diag(10)
W.mat <- diag(rep(c(10, 0.0001, 0.0001, 0.0001, 0.0001), 2))
m1 <- SS(y = Sal.mat, x = x.mat,
         Fmat =
          function(tt, x, phi) return (matrix(
            c(x[tt,1], x[tt,2], x[tt,3], x[tt,4], x[tt,5], rep(0,10), 
              x[tt,1], x[tt,2], x[tt,3], x[tt,4], x[tt,5]), 
                               nrow=10,ncol=2)),
         Gmat = function(tt, x, phi) return (G.mat),
         Wmat = function(tt, x, phi) return (W.mat),
         Vmat = function(tt, x, phi) return
                  (matrix(c(839, -348, -348, 1612), nrow=2, ncol=2)),
         m0=matrix(c(0,0.9,0.1,-15,-10,0,0,0.7,30,20),nrow=1,ncol=10),
         C0 = 100 * W.mat
         )

m1.f <- kfilter (m1)
par(mfcol=c(2,3))
plot(m1.f$m[,1], type='l')
plot(m1.f$m[,2], type='l')
plot(m1.f$m[,3], type='l')
plot(m1.f$m[,6], type='l')
plot(m1.f$m[,7], type='l')
plot(m1.f$m[,8], type='l')
par(mfcol=c(2,2))
plot(m1.f$m[,4], type='l')
plot(m1.f$m[,5], type='l')
plot(m1.f$m[,9], type='l')
plot(m1.f$m[,10], type='l')


# Chapter 12, p246, Exercise 7
set.seed(1)
x1 <- c(1:30)
x1 <- x1/10 + 2
a <- 4
b <- 2
n <- length(x1)
y1 <- a + b * x1 + 0.1 * rnorm(n)
x0 <- rep(1, n)
xx <- cbind(x0, x1)
F <- matrix(xx, nrow = n,ncol=2)
y <- matrix(y1, nrow = n,ncol=1)
G <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
W <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
V <- matrix(1)
m0 <- matrix(c(5,1.5), nrow = 2, ncol = 1)
C0 <- matrix(c(.1,0,0,.1), nrow = 2, ncol = 2)
a <- 0;R <- 0;f <- 0;Q <- 0;e <- 0;A <- 0;m <- 0;C <- 0;tt <- 0;
Kfilt.m <- cbind(rep(0, n), rep(0, n))
m <- m0
C <- C0
for (tt in 1:n) {
    Fmat <- matrix(c(F[tt,1],F[tt,2]), nrow = 2, ncol = 1)
    a <- G %*% m  
    R <- G %*% C %*% t(G) + W 
    f <- t(Fmat) %*% a
    Q <- t(Fmat) %*% R %*% Fmat + V
    e <- y[tt]-f
    A <- R %*% Fmat %*% solve(Q)
    m <- a + A %*% e
    C <- R - A %*% Q %*% t(A)
    Kfilt.m[tt,1] <- m[1,1]
    Kfilt.m[tt,2] <- m[2,1]
  }
plot(Kfilt.m[1:n, 1])
plot(Kfilt.m[1:n, 2])

## END ##
