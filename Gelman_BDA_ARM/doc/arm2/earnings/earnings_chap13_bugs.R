# Fit varying-intercept, varying-slope earnings models using bugs

# 1.  Varying-intercept, varying-slope model of earnings on height, with ethnicity categories

# set up the data

attach.all (heights.clean0)

y <- log(earn)
x <- height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- age.category
age.label <- c("age 18-34", "age 35-49", "age 50-64")
eth.label <- c("blacks", "hispanics", "whites", "others")

x.jitter.add <- runif(n, -.2,.2)

# set up and fit bugs model

x <- height

data <- list ("n", "x", "y", "n.eth", "eth")
inits <- function(){
  list (B=array(rnorm(n.eth*2), c(n.eth,2)), sigma.y=runif(1),
        mu.a=rnorm(1), mu.b=runif(1),
        sigma.a=runif(1), sigma.b=runif(1), rho=runif(1))
}
parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y",
                 "sigma.a", "sigma.b", "rho")
M1.bugs <- bugs (data, inits, parameters, "earnings1.bug", n.chains=3, n.iter=10000)
plot (M1.bugs)

# again, this time with x centered

x <- height - mean(height)
M2.bugs <- bugs (data, inits, parameters, "earnings1.bug", n.chains=3, n.iter=10000)
plot (M2.bugs)


# alternative M1a using scaled-inv-wish

library ("MCMCpack")

x <- height
W <- diag (2)
data <- list ("n", "x", "y", "n.eth", "eth", "W")
inits <- function(){
  list (B.raw=array(rnorm(n.eth*2), c(n.eth,2)), sigma.y=runif(1),
        mu.a.raw=rnorm(1), mu.b.raw=rnorm(1), Tau.B.raw=rwish(3,diag(2)),
        xi.a=runif(1), xi.b=runif(1))
}
parameters <- c ("a", "b", "mu.a", "mu.b", "sigma.y",
                 "sigma.a", "sigma.b", "rho")
M1a.bugs <- bugs (data, inits, parameters, "earnings1a.bug", n.chains=3, n.iter=100)
plot (M1a.bugs)

# alternative M2a using scaled-inv-wish

x <- height - mean(height)
M2a.bugs <- bugs (data, inits, parameters, "earnings1a.bug", n.chains=3, n.iter=1000)
plot (M2a.bugs)

# Go back to model 1

attach.bugs (M2.bugs)

x <- height

a <- a - b*mean(height)
a.hat <- apply (a, 2, mean)
a.se <- apply (a, 2, sd)
b.hat <- apply (b, 2, mean)
b.se <- apply (b, 2, sd)

postscript ("c:/books/multilevel/heights.mult1a.ps", height=2.5, horizontal=TRUE)
par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot ((x + x.jitter.add)[eth==j], y[eth==j], xlab="height (inches)", ylab="log earnings", mgp=c(2,.7,0), xlim=range(x), ylim=range(y),
        yaxt="n",
        cex.lab=1.4, cex.axis=1.3, pch=20, cex=.6, cex.main=1.5, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.3, mgp=c(2,.5,0))
  for (s in 1:20){
    curve (a[s,j] + b[s,j]*x, lwd=.5, col="gray20", add=TRUE)
  }
  curve (a.hat[j] + b.hat[j]*x, lwd=1, add=TRUE)
}
dev.off()

postscript ("c:/books/multilevel/heights.mult2a.ps", horizontal=TRUE)
par (mar=c(7,8,4,2)+.1)
plot (a.hat, b.hat,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=3, cex.lab=3.3, cex=3, mgp=c(5.5,1.5,0), type="n", yaxt="n", ylim=c(0,max(b.hat)*1.1), xlim=range(min(a.hat)-.1, max(a.hat)+.1))
axis (2, seq(0,.06,.02), cex.axis=3, mgp=c(4.5, 1.5,0))
text (a.hat, b.hat, substr (eth.label,1,1), cex=3.3)
abline (0, 0, lwd=.5, col="gray10")
dev.off()


postscript ("c:/books/multilevel/heights.mult3a.ps", horizontal=TRUE)
par (mar=c(5,5,4,2)+.1)
plot ((x + x.jitter.add), y, xlim=c(0,max(height)+3), xlab="height (inches)", ylab="log earnings",
      cex.lab=2.5, cex.axis=2.5, xaxs="i", pch=20, cex=.8, yaxt="n")
axis (2, seq(6,12,2), cex.axis=2.5, mgp=c(3,.5,0))
for (j in 1:4){
  curve (a.hat[j] + b.hat[j]*x, add=TRUE)
}
adj1 <- c(4, 2, 2, -4)
adj2 <- c(-.1, -.2, -.1, .3)
text (20 + adj1, a.hat + b.hat*20 + adj2, eth.label, cex=2.7)
dev.off()

# Now make the plots with model 2 (centered)

attach.bugs (M2.bugs)

x <- height - mean(height)

a.hat <- apply (a, 2, mean)
a.se <- apply (a, 2, sd)
b.hat <- apply (b, 2, mean)
b.se <- apply (b, 2, sd)

postscript ("c:/books/multilevel/heights.mult4a.ps", height=2.5, horizontal=TRUE)
par (mfrow=c(1,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:4){
  plot ((x + x.jitter.add)[eth==j], y[eth==j], xlab="height (inches from mean)", ylab="log earnings", mgp=c(2,.7,0), xlim=range(x), ylim=range(y),
        yaxt="n",
        cex.lab=1.4, cex.axis=1.3, pch=20, cex=.6, cex.main=1.5, main=eth.label[j])
  axis (2, seq(6,12,2), cex.axis=1.3, mgp=c(2,.5,0))
  for (s in 1:20){
    curve (a[s,j] + b[s,j]*x, lwd=.5, col="gray20", add=TRUE)
  }
  curve (a.hat[j] + b.hat[j]*x, lwd=1, add=TRUE)
}
dev.off()

postscript ("c:/books/multilevel/heights.mult5a.ps", horizontal=TRUE)
par (mar=c(7,8,4,2)+.1)
plot (a.hat, b.hat,
      xlab=expression(paste("intercept, ",alpha[j])), ylab=expression(paste("slope, ", beta[j])),
      pch=20, cex.axis=3, cex.lab=3.3, cex=3, mgp=c(5.5,1.5,0), type="n", yaxt="n", ylim=c(0,max(b.hat)*1.1), xlim=range(min(a.hat)-.01, max(a.hat)+.01))
axis (2, seq(0,.06,.02), cex.axis=3, mgp=c(4.5, 1.5,0))
text (a.hat, b.hat, substr (eth.label,1,1), cex=3.3)
abline (0, 0, lwd=.5, col="gray10")
dev.off()

# 2.  Ethnicity and age categories

# fit the model

x <- height - mean(height)

zeros <- rep (0, 2)
Tau.0 <- diag (rep (.0001, 2))
data <- list ("n", "x", "y", "n.eth", "eth", "n.age", "age", "zeros", "Tau.0")
inits <- function(){
  list (B=array(rnorm(n.eth*n.age*2), c(n.eth,n.age,2)),
        G.eth=array(rnorm(n.eth,2), c(n.eth,2)),
        G.age=array(rnorm(n.age,2), c(n.age,2)),
        sigma.y=runif(1),
        mu=rnorm(2),
        sigma.B=runif(2), sigma.eth=runif(2), sigma.age=runif(2),
        rho.B=runif(1), rho.eth=runif(1), rho.age=runif(1))
}
parameters <- c ("a", "b", "mu", "G.age", "G.eth", "sigma.y",
                 "sigma.B", "sigma.eth", "sigma.age",
                 "rho.B", "rho.eth", "rho.age")
M3.bugs <- bugs (data, inits, parameters, "earnings3.bug", n.chains=3, n.iter=5000)
plot (M3.bugs)

# make the graph

attach.bugs (M3.bugs)
postscript ("c:/books/multilevel/heights.mult6.ps", height=7.5, horizontal=TRUE)
par (mfcol=c(3,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:n.eth){
  for (k in 1:n.age){
     plot ((x + x.jitter.add)[eth==j&age==k], y[eth==j&age==k], xlab="height (inches from mean)", ylab="log earnings",
        cex.lab=1.4, cex.axis=1.3, pch=20, cex=.6, cex.main=1.5, xlim=range(x),
           ylim=range(y), mgp=c(2,.7,0), yaxt="n",
        main=paste(eth.label[j], ", ", age.label[k], sep=""))
     axis (2, seq(6,12,2), cex.axis=1.3, mgp=c(2,.5,0))
     for (i in 1:20){
       curve (a[i,j,k] + b[i,j,k]*x, lwd=.5, col="gray30", add=TRUE)
     }
     curve (mean(a[,j,k]) + mean(b[,j,k])*x, lwd=1, add=TRUE)
  }
}
dev.off()
