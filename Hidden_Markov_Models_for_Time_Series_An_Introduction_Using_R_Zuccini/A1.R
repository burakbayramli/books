# Copy of R-code given in the Appendix 
# A.1 Fit a stationary Poisson–HMM by direct numerical maximization

pois.HMM.pn2pw <- function(m,lambda,gamma)
{                                              
 tlambda <- log(lambda)                         
 tgamma  <- NULL                              
 if(m>1)                                        
   {                                            
   foo   <- log(gamma/diag(gamma))           
   tgamma<- as.vector(foo[!diag(m)])             
   }                                             
 parvect <- c(tlambda,tgamma)                    
 parvect                                         
}  


pois.HMM.pw2pn <- function(m,parvect)                 
{                                                     
 epar   <- exp(parvect)                              
 lambda <- epar[1:m]                                   
 gamma  <- diag(m)                                    
 if(m>1)                                               
   {                                                  
   gamma[!gamma] <- epar[(m+1):(m*m)]                  
   gamma         <- gamma/apply(gamma,1,sum)          
   }                                                   
 delta  <- solve(t(diag(m)-gamma+1),rep(1,m))          
 list(lambda=lambda,gamma=gamma,delta=delta)           
}  


pois.HMM.mllk <- function(parvect,x,m,...)       
{                                                  
 if(m==1) return(-sum(dpois(x,exp(parvect),log=TRUE))) 
 n          <- length(x)                            
 pn         <- pois.HMM.pw2pn(m,parvect)            
 allprobs   <- outer(x,pn$lambda,dpois)             
 allprobs   <- ifelse(!is.na(allprobs),allprobs,1)  
 lscale     <- 0                                    
 foo        <- pn$delta                             
 for (i in 1:n)                                    
   {                                                
   foo    <- foo%*%pn$gamma*allprobs[i,]            
   sumfoo <- sum(foo)                               
   lscale <- lscale+log(sumfoo)                    
   foo    <- foo/sumfoo                            
   }                                               
 mllk       <- -lscale                            
 mllk                                              
}    


pois.HMM.mle <- function(x,m,lambda0,gamma0,...)
{                                                      
 parvect0  <- pois.HMM.pn2pw(m,lambda0,gamma0)         
 mod       <- nlm(pois.HMM.mllk,parvect0,x=x,m=m)       
 pn        <- pois.HMM.pw2pn(m,mod$estimate)            
 mllk      <- mod$minimum                              
 np        <- length(parvect0)                          
 AIC       <- 2*(mllk+np)                              
 n         <- sum(!is.na(x))                            
 BIC       <- 2*mllk+np*log(n)                         
 list(lambda=pn$lambda,gamma=pn$gamma,delta=pn$delta,   
              code=mod$code,mllk=mllk,AIC=AIC,BIC=BIC)   
}  

  