#############################################################################
#
# Classification using VGAM  (polytomous logistic regression)
#
#############################################################################

# Use Packages -> Install Package(s)... -> mclust to install "VGAM"
# library...

library()             # list all available libraries
library(help=VGAM)    # general info about one library

library(VGAM)         # load the VGAM library

help(vgam)            # get help on the main routines in VGAM...
help(vglm)


data(iris)     # try http://en.wikipedia.org/wiki/Sepal

iris


cols=c(1,2)
plot(iris[,cols],type="n")
points(iris[iris$Species=="setosa",cols],col=1,pch="*")
points(iris[iris$Species=="versicolor",cols],col=2,pch="*")
points(iris[iris$Species=="virginica",cols],col=3,pch="*")

cols=c(3,4)
plot(iris[,cols],type="n")
points(iris[iris$Species=="setosa",cols],col=1,pch="*")
points(iris[iris$Species=="versicolor",cols],col=2,pch="*")
points(iris[iris$Species=="virginica",cols],col=3,pch="*")


myiris <- cbind(iris,setosa=ifelse(iris$Species=="setosa",1,0))
myiris <- cbind(myiris,versicolor=ifelse(iris$Species=="versicolor",1,0))
myiris <- cbind(myiris,virginica=ifelse(iris$Species=="virginica",1,0))

myiris

myiris <- myiris[,-5] # drop the old labels

myiris

# First try regular logistic regression, to distinguish setosa from the others

iris.glm <- glm(setosa ~ Sepal.Length + Sepal.Width + Petal.Length +
Petal.Width, family=binomial, data=myiris)

summary(iris.glm)

# How did we do???

fitted(iris.glm)

fitted.iris <- round(fitted(iris.glm))

cbind(fitted.iris,myiris$setosa)


# Now try multinomial logistic regression, for all 3 types

iris.vglm <- vglm(cbind(setosa,versicolor,virginica) ~ Sepal.Length +
Sepal.Width + Petal.Length + Petal.Width, family=multinomial,
data=myiris)

summary(iris.vglm)

# How did we do???

fitted(iris.vglm)

# Since the fitted probabilities are so close to 0 and 1,
# we can get fitted classifications with just

fitted.iris <- round(fitted(iris.vglm))

# if some of the probabilities are not close to 0 or 1, or
# we are just not sure, a better strategy is to use which.max:

max.loc <- apply(fitted(iris.vglm),1,which.max)
n <- length(max.loc)
fitted.iris <- matrix(0,nrow=n,ncol=3)
for (i in 1:n) {
  fitted.iris[i,max.loc[i]] <- 1
}

# Now we use 'fitted.iris' as the model-based classifications
# of which flower is of each type.  How does this compare
# to the real types?

fitted.iris

fitted.iris - myiris[,c(5,6,7)]

abs(fitted.iris - myiris[,c(5,6,7)])

apply(abs(fitted.iris - myiris[,c(5,6,7)]),1,sum)

ifelse(apply(abs(fitted.iris - myiris[,c(5,6,7)]),1,sum)==0,1,0)

mean(ifelse(apply(abs(fitted.iris -
                      myiris[,c(5,6,7)]),1,sum)==0,1,0)) 


#############################################################################
#
# Some univariate normal mixture examples
#
#############################################################################

mixture <- function(x,mu,sigma,alpha) {

  alpha <- alpha/sum(alpha)

  K <- length(alpha)

  tmp <- NULL

  for (k in 1:K) {

    tmp <- cbind(tmp,dnorm(x,mu[k],sigma[k]))

  }

  return(tmp%*%alpha)
  
}

x <- seq(-4,4,length=100)

# Mixtures of one or two normals

par(mfrow=c(2,2))

plot(x,mixture(x,0,1,1),type="l",ylab="density")

plot(x,mixture(x,c(0,1),c(1,1),c(1,3)),type="l",ylab="density")
plot(x,mixture(x,c(0,1),c(.5,.5),c(1,3)),type="l",ylab="density")
plot(x,mixture(x,c(0,1),c(.25,.25),c(1,3)),type="l",ylab="density")

# Mixtures of four normals

plot(x,mixture(x,c(-3.3,-2,0,1),c(2,1,1,1),c(.25,1,2,1)),type="l",
     ylab="density")
plot(x,mixture(x,c(-3.3,-2,0,1),rep(.25,4),c(1,1,2,1)),type="l",
     ylab="density")

############################################################################
#
# Some bivariate normal mixture examples
#
############################################################################

dmix <- function(x,mu,sigma,alpha) {

# mvn density is the usual density evaluated at an inner product, and scaled
# by [2pi*(det(sigma)]^(d/2); so from the dnorm function this is
#
# dnorm(i.p.,0,1)/[2pi]^[(d-1)/2]/det(sigma)^(d/2)
#
# here we assume
#
# x is n x d
# mu is K x d
# sigma is d x d x K
# alpha is K x 1
#
  alpha <- alpha/sum(alpha)

  K <- length(alpha)

  n <- dim(x)[1]
  d <- dim(x)[2]

  if (d!=2) stop("Only implemented for d=2 so far")

  xx <- cbind(rep(x[,1],rep(n,n)),rep(x[,2],n))

  tmp <- NULL

  for (k in 1:K) {

    ip <- xx - matrix(mu[k,],byrow=T,nrow=n*n,ncol=d)

    ip <- diag(ip%*%solve(sigma[,,k])%*%t(ip)) # inefficient but easy...

    tmp <- cbind(tmp,dnorm(ip,0,1)/det(sigma[,,k])^(d/2))
    
  }

  tmp <- tmp%*%alpha
  tmp <- tmp/(2*pi)^(d-1)/2
  return(matrix(tmp,ncol=n))

  return(tmp)
  
}

x <- seq(-4,4,length=20)

mu <- matrix(c(-1,-1,1,1),byrow=T,ncol=2)
sigma <- array(c(1,.2,.2,1,1,-.4,-.4,1),c(2,2,2))
# sigma <- array(c(1,0,0,1,1,0,0,1),c(2,2,2))

par(mfrow=c(2,2))

contour(x,x,dmix(cbind(x,x),mu,sigma,c(1,2)))
persp(x,x,dmix(cbind(x,x),mu,sigma,c(1,2)),theta=45,phi=45,
      xlab="x",ylab="y",zlab="density")

#####################################################################

par(mfrow=c(3,2))

x <- seq(-4,4,length=100)
# plot(x,mixture(x,0,1,1),type="l",ylab="density")
plot(x,mixture(x,c(0,1),c(1,1),c(1,3)),type="l",ylab="density")
plot(x,mixture(x,c(0,1),c(.5,.5),c(1,3)),type="l",ylab="density")
# plot(x,mixture(x,c(0,1),c(.25,.25),c(1,3)),type="l",ylab="density")
plot(x,mixture(x,c(-3.3,-2,0,1),c(2,1,1,1),c(.25,1,2,1)),type="l",
     ylab="density")
plot(x,mixture(x,c(-3.3,-2,0,1),rep(.25,4),c(1,1,2,1)),type="l",
     ylab="density")

x <- seq(-4,4,length=20)

mu <- matrix(c(-1,-1,1,1),byrow=T,ncol=2)
sigma <- array(c(1,.2,.2,1,1,-.4,-.4,1),c(2,2,2))
# sigma <- array(c(1,0,0,1,1,0,0,1),c(2,2,2))

contour(x,x,dmix(cbind(x,x),mu,sigma,c(1,2)))
persp(x,x,dmix(cbind(x,x),mu,sigma,c(1,2)),theta=45,phi=45,
      xlab="x",ylab="y",zlab="density")

############################################################################
#
# The Old Faithful Example
#
############################################################################

data(faithful)

y <- faithful$waiting

hist(y)

nll <- function(p) {

  a  <- p[1]
  m1 <- p[2]
  s1 <- p[3]
  m2 <- p[4]
  s2 <- p[5]

  return(-sum(log(a*dnorm(y,m1,s1) + (1-a)*dnorm(y,m2,s2))))
  
}

nlm(nll,c(.25,52,10,82,10))

#####################################################

x <- seq(min(y),max(y),length=100)

mle <- nlm(nll,c(.25,52,10,82,10))$estimate

hist(y,probability=T)

mu <- c(mle[2],mle[4])
sigma <- c(mle[3],mle[5])
alpha <- c(mle[1],1-mle[1])

lines(x,mixture(x,mu,sigma,alpha))

############################################################################
#
# Model-Based Clustering with Mclust
#
############################################################################

# Use Packages -> Install Package(s)... -> mclust to install "mclust"
# library...

library()             # list all available libraries
library(help=mclust)  # general info about one library

library(mclust)       # load the mclust library

help(Mclust)          # get help on the main routine in mclust...

############################################################

# Try Mclust with the Old Faithful data...

y <- faithful$waiting

faithfulMclust <- Mclust(y)



plot(faithfulMclust,y)

# Need to look at
#  help(Mclust)
#  help(plot.Mclust)
#  help(mclustModelnames)
# to interpret some of this...
#
# N.b. BIC = 2*log(likelihood) - k*log(n)  [k = # param's, n = # obs's]

##############################################################

# Try Mclust with Fisher's "IRIS" data...

iris   # try http://en.wikipedia.org/wiki/Sepal

irisMclust <- Mclust(iris[,-5])

plot(irisMclust, iris[,-5])

# Need to look at
#  help(Mclust)
#  help(plot.Mclust)
#  help(mclustModelnames)
# to interpret some of this...
#
# N.b. BIC = 2*log(likelihood) - k*log(n)  [k = # param's, n = # obs's]

# How did we do?

rbind(as.numeric(iris[,5]),irisMclust$classification)

mean(ifelse(as.numeric(iris[,5])==irisMclust$classification,1,0))

# note that the "optimal" clustering makes only 2 clusters

# what if we want to see how we do with 3 clusters?

irisMclust.3 <- Mclust(iris[,-5],G=3)

rbind(as.numeric(iris[,5]),irisMclust.3$classification)

mean(ifelse(as.numeric(iris[,5])==irisMclust.3$classification,1,0))

#############################################################






