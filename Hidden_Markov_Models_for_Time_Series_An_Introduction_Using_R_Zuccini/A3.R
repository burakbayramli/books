# Copy of R-code given in the Appendix 
# A.3 HMM with bivariate normal state-dependent distributions

bivnorm.HMM.pn2pw <- 
 function(mu,sigma,corr,gamma,m)
{
 tsigma <- log(sigma)
 tcorr<-log((1+corr)/(1-corr))
 tgamma <- NULL
 if(m>1)
   {
   foo    <- log(gamma/diag(gamma))
   tgamma <- as.vector(foo[!diag(m)])
   }
 parvect <- c(as.vector(mu),as.vector(tsigma),
            tcorr,tgamma)
 parvect
}

bivnorm.HMM.pw2pn <- function(parvect,m)
{
 mu    <- matrix(parvect[1:(2*m)],m,2)
 sigma <- matrix(exp(parvect[(2*m+1):(4*m)]),m,2)
 temp<-exp(parvect[(4*m+1):(5*m)])
 corr<-(temp-1)/(temp+1)
 gamma <- diag(m)
 if(m>1)
   {
   gamma[!gamma]<-exp(parvect[(5*m+1):(m*(m+4))])
   gamma<-gamma/apply(gamma,1,sum)
   }
 delta<-solve(t(diag(m)-gamma+1),rep(1,m))
 list(mu=mu,sigma=sigma,corr=corr,gamma=gamma,
      delta=delta)
}


bivnorm.HMM.mllk<-function(parvect,x1,x2,m,...)
{
 n        <- dim(x1)[1]
 p        <- bivnorm.HMM.pw2pn(parvect,m)
 foo      <- p$delta
 covs     <- array(NA,c(2,2,m))
 for (j in 1:m)                             
   {
   covs[,,j] <- diag(p$sigma[j,])%*%
                matrix(c(1, p$corr[j],p$corr[j],1),2,2)%*%
                diag(p$sigma[j,])
   }                                        
 P        <- rep(NA,m)
 lscale   <- 0                                
 for (i in 1:n)
   {
   for (j in 1:m)
     {
     P[j] <- pmvnorm(lower=c(x1[i,1],x2[i,1]),  
                     upper=c(x1[i,2],x2[i,2]),
                     mean=p$mu[j,], sigma=covs[,,j])
     }
   foo    <- foo%*%p$gamma*P
   sumfoo <- sum(foo)
   lscale <- lscale+log(sumfoo)
   foo    <- foo/sumfoo
   }
 mllk     <- -lscale                    
 mllk
}


bivnorm.HMM.mle<-
 function(x1,x2,m,mu0,sigma0,corr0,gamma0,...)
{
 n     <- dim(x1)[1]
 start <- bivnorm.HMM.pn2pw(mu0,sigma0,corr0,gamma0,m)
 mod   <- nlm(bivnorm.HMM.mllk,p=start,x1=x1,x2=x2,m=m,
          steptol = 1e-4,iterlim = 10000)
 mllk  <- mod$minimum
 code  <- mod$code
 p     <- bivnorm.HMM.pw2pn(mod$estimate,m)
 np    <- m*(m+4)
 AIC   <- 2*(mllk+np)
 BIC   <- 2*mllk+np*log(n)
 list(mu=p$mu,sigma=p$sigma,corr=p$corr,
            gamma=p$gamma,delta=p$delta,code=code,
            mllk=mllk,AIC=AIC,BIC=BIC)
}
