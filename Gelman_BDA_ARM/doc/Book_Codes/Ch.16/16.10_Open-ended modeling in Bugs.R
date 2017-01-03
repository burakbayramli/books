## Setting up parameters

inits <- function (){
  list (a=rnorm(1), b=rnorm(J), C=array(rnorm(J*K), c(J,K)), 
        sigma=runif(1))
}

 # or, using Normal and Uniform distributions

inits <- function (){
  list (a=rnorm(1,0,2), b=rnorm(J,0,2), C=array(rnorm(J*K,0,2), c(J,K)), 
        sigma=runif(1,.1,10))
}

