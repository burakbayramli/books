
mix.dens.fun = function(w,mu,sigma,grid){
   ###generate density for a mixture of normals
   k = length(mu)
   out = rep(0,length(grid))
   for(i in 1:k){
     out = out+w[i]*dnorm(grid,mu[i],sigma[i])
     }
   return(out)
   }


mix.fun = function(w,mu,sigma,n){
   ###generate n observations from a mixture of normals
   k = length(mu)
   out = rnorm(n*k,rep(mu,rep(n,k)),rep(sigma,rep(n,k)))
   out = matrix(out,n,k)
   index = rep(1,n);w = cumsum(w)
   u = runif(n)
   for(i in 2:k){
      index[ (u>w[i-1]) & (u<=w[i])] = i
      }
   outt = rep(0,n)
   for(i in 1:n){
      outt[i] = out[i,index[i]]
      }
   return(outt)
   }


ww = list( c(1,1,3)/5, rep(1/8,8), c(2,1)/3, c(1,9)/10,
        c(1,1)/2,   c(1,1)/2,   c(3,1)/4, c(9,9,2)/20,
        c(5,1,1,1,1,1)/10, c(49/100,49/100,rep(1/350,7)),
        c(1/2,8/31,4/31,2/31,1/31,.5/31),
        c(46/100,46/100,1/300,1/300,1/300,7/300,7/300,7/300),
        2^(5-(0:5))/63, c(rep(2/7,3),rep(1/21,3)) )

mu = list( c(0,1/2,13/12), 3*((2/3)^(0:7)-1), c(0,0),
         c(0,0), c(-1,1), c(-3/2,3/2), c(0,3/2),
         c(-6/5, 6/5, 0), c(0, (0:4)/2 -1),
         c(-1,1, ((0:6)-3)/2), c(0, (-2:2)+.5),
         c(2*(0:1)-1,-(1:3)/2,(1:3)/2),
         c( (65-96*(1/2)^(0:5))/21), c( (12*(0:2)-15)/7,2*(8:10)/7))

sigma = list( c(1,2/3,5/9), (2/3)^(0:7) , c(1,1/10),
            c(1,1/10), c(2/3,2/3), c(1/2,1/2),
            c(1,1/3), c(3/5,3/5,1/4), c(1,rep(1/10,5)),
            c(2/3,2/3,rep(.01,7)), c(1,2^c(2,1,0,-1,-2)/10),
            c(2/3,2/3,.01,.01,.01,.07,.07,.07),
            (32/63)/2^(0:5), c(2/7,2/7,2/7,1/21,1/21,1/21) )


par(mfrow=c(5,2))
n = 1000;nsim = 100;M = 10;grid = seq(-10,10,length=100)

plot(grid,dnorm(grid),type="l")
x = rnorm(n)
out = whole.thing.fun(x,nsim,grid,M)
plot(grid,out$pred,type="l")
barplot(out$prob)

for(i in 1:14){
     plot(grid,mix.dens.fun(ww[[i]],mu[[i]],sigma[[i]],grid),type="l")
     x = mix.fun(ww[[i]],mu[[i]],sigma[[i]],n)
     hist(x)
     }













