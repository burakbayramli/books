# Copy of R-code given in the Appendix 
# A.4 Fitting a categorical HMM by constrained optimization

cat.HMM.nllk <- function(parvect,x,m,q,...)
{
 n     <- length(x)
 gamma <- matrix(0,m,m)          #IM alteration 17.04.2009
 pr    <- matrix(0,m,q)
 for (i in 1:m) 
   {
   gamma[i,1:(m-1)] <-
     parvect[((i-1)*(m-1)+1):(i*(m-1))]
   gamma[i,m]<-1-sum(gamma[i,1:(m-1)])
   pr[i,1:(q-1)] <- 
     parvect[((i-1)*(q-1)+m*(m-1)+1):
                    (i*(q-1)+m*(m-1))]
   pr[i,q] <- 1-sum(pr[i,1:(q-1)])
   }
 delta  <- solve(t(diag(m)-gamma+1),rep(1,m))
 lscale <- 0
 foo    <- delta
 for (i in 1:n)
   {
   foo    <- foo%*%gamma*pr[,x[i]]
   sumfoo <- sum(foo)
   lscale <- lscale+log(sumfoo)
   foo    <- foo/sumfoo
   }
 nllk <- -lscale
 nllk
 }




cat.HMM.mle <- function(x,m,gamma0,pr0,...)
{
 q        <- ncol(pr0)
 parvect0 <- c(as.vector(t(gamma0[,-m])),
               as.vector(t(pr0[,-q])))
 np       <- m*(m+q-2)                                     
 u1       <- diag(np)
 u2 <- u3 <- matrix(0,m,np)
 for (i in 1:m)
   { 
   u2[i,((i-1)*(m-1)+1):(i*(m-1))] <- rep(-1,m-1)
   u3[i,(m*(m-1)+(i-1)*(q-1)+1):(m*(m-1)+i*(q-1))]  <- rep(-1,q-1)  
   }
 ui  <- rbind(u1,u2,u3)
 ci  <- c(rep(0,np),rep(-1,2*m))                            
 mod <- constrOptim(parvect0,cat.HMM.nllk,grad=NULL,ui=ui,  
           ci=ci,mu=1e-07,method="Nelder-Mead",x=x,m=m,q=q)
 mod    
}     
