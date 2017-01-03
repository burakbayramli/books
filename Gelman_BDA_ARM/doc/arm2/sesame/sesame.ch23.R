### "truth" for this group is 7.4

# 
library(foreign)
sesame <- read.dta("sesame.dta")

#fit <- lm(regular ~ encour + prelet + as.factor(site) + setting, data=sesame)
#regular.hat <- fit$fitted
#lm(postlet ~ regular.hat + prelet + as.factor(site) + setting, data=sesame)

library(sem)
iv2=tsls(postlet~regular+prelet+as.factor(site)+setting,~encour+prelet+as.factor(site)+setting,data=sesame)

########### iv and mlm
# first bayesian iv

y=sesame$postlet
d=sesame$regular
yd=cbind(sesame$postlet,sesame$regular)
z=sesame$encour
n=nrow(sesame)

siteset=numeric(nrow(sesame))
for(j in 1:2){
for(i in 1:5){
siteset[sesame$site==i & sesame$setting==j]=i+5*(j-1)
}
}
J=9

ses.data <- list("yd","z","n","siteset","J")
ses.params <- c("alpha","gamma","delta","beta","sigma.y","sigma.d","rho.yd","theta","sigma.g","sigma.a","rho.ag")

ses.inits <- function(){
list(delta=rnorm(1,.35,.1),beta=rnorm(1),sigma.y=runif(1),sigma.d=runif(1),rho.yd=runif(1,-1,1),theta=rnorm(2),sigma.a=runif(1),sigma.g=runif(1),rho.ag=runif(1,-1,1),ag=cbind(rnorm(J),rnorm(J)))
}

#ses.fit2 <- bugs(ses.data, ses.inits, ses.params,"ses2.bug",n.chains=3,n.iter=10,debug=T)
ses.fit2 <- bugs(ses.data, ses.inits, ses.params,"ses2.bug",n.chains=3,n.iter=1000)

