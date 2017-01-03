## Read the data & redefine variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/sesame
library ("arm")
library(foreign)
sesame <- read.dta("sesame.dta")

y=sesame$postlet
d=sesame$regular
yt=cbind(sesame$postlet,sesame$regular)
z=sesame$encour
n=nrow(sesame)

siteset=numeric(nrow(sesame))
for(j in 1:2){
for(i in 1:5){
siteset[sesame$site==i & sesame$setting==j]=i+5*(j-1)
}
}
J=9

## Fit the model for example 1
ses.data <- list("yt","z","n","siteset","J")
ses.params <- c("a","g","d","b","sigma.y","sigma.t","rho.yt","sigma.g",
   "sigma.a","rho.ag")

ses.inits <- function(){
list(d=rnorm(1,.35,.1),b=rnorm(1),sigma.y=runif(1),sigma.t=runif(1),
  rho.yt=runif(1,-1,1),sigma.a=runif(1),sigma.g=runif(1),
  rho.ag=runif(1,-1,1),ag=cbind(rnorm(J),rnorm(J)))
}

ses <- bugs(ses.data, ses.inits, ses.params,"ses.bug",n.chains=3,n.iter=1000, 
    bugs.directory="c:/.../", working.directory=NULL, clearWD=TRUE, debug=TRUE )