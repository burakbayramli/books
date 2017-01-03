#  Example 11.8 and Figures 11.5 and 11.6

library(quadprog)
dat = read.table("countries.txt",header =T)
dat = dat[,4:13]
n = dim(dat)[1]
N = dim(dat)[2]
R = 100*(dat[2:n,]/dat[1:(n-1),] - 1)
mufree = 1/24
mean_vect_TRUE = apply(R,2,mean)
cov_mat_TRUE = cov(R)

nboot = 250
out = matrix(1,nrow=nboot,ncol=2)
mean_out = matrix(1,nrow = nboot,ncol = dim(dat)[2])
set.seed(998877)
for (iboot in (1:nboot))
{
un = ceiling((n-1)*runif(n-1))
Rboot = R[un,]
mean_vect = apply(Rboot,2,mean)
mean_out[iboot,] = mean_vect
cov_mat = cov(Rboot)
sd_vect = sqrt(diag(cov_mat))
Amat = cbind(rep(1,N),mean_vect) 
muP = seq(0,2.5,length=300)                              
sdP = muP 
weights = matrix(0,nrow=300,ncol=N) 
for (i in 1:length(muP))  
{
bvec = c(1,muP[i])  
result = 
   solve.QP(Dmat=2*cov_mat,dvec=rep(0,N),Amat=Amat,bvec=bvec,meq=2)
sdP[i] = sqrt(result$value)
weights[i,] = result$solution
} 
sharpe =( muP-mufree)/sdP 
ind = (sharpe == max(sharpe)) 
out[iboot,1] = sharpe[ind]
wT = weights[ind,]
sharpe_TRUE = (wT %*% mean_vect_TRUE - mufree) /
   sqrt(wT %*% cov_mat_TRUE %*% wT)
out[iboot,2] = sharpe_TRUE
}
out_Short = out

gp = cbind(rep("estimated",nboot),rep("actual",nboot))

country_names = c(
"Hong Kong",
 "Singapore",
  "Brazil",
  "Argentina",
  "UK",
  "Germany",
   "Canada",
  "France",
  "Japan",
  "US")

qu_out = matrix(1,nrow=10,ncol=2)

postscript("countries_boot_mean_kde.ps",width=6,height=7)  #  Figure 11.6
par(mfrow=c(4,3))
for (i in 1:10)
{
plot(density(mean_out[,i]),main=country_names[i])
qu_out[i,] = quantile(mean_out[,i],c(.025,.975))
}
graphics.off()

round(qu_out,3)

postscript("countries_boot.ps",width=7,height=4)
par(mfrow=c(1,2))
boxplot(out_Short~gp,main="(a) Short Sales Allowed",ylim=c(0,.7))
abline(h=.3681,lwd=3,lty=2)


apply(out_Short,2,mean)


###################  No short sales


out = matrix(1,nrow=nboot,ncol=2)
set.seed(998877)
for (iboot in (1:nboot))
{
un = ceiling((n-1)*runif(n-1))
Rboot = R[un,]


mean_vect = apply(Rboot,2,mean)
cov_mat = cov(Rboot)
sd_vect = sqrt(diag(cov_mat))
Amat = cbind(rep(1,N),mean_vect,diag(1,N)) 
muP = seq(min(mean_vect)+.001,max(mean_vect)-.001,length=300)                              
sdP = muP 
weights = matrix(0,nrow=300,ncol=N) 
for (i in 1:length(muP))  
{
bvec = c(1,muP[i],rep(0,N))  
result = 
   solve.QP(Dmat=2*cov_mat,dvec=rep(0,N),Amat=Amat,bvec=bvec,meq=2)
sdP[i] = sqrt(result$value)
weights[i,] = result$solution
} 
sharpe =( muP-mufree)/sdP 
ind = (sharpe == max(sharpe)) 
out[iboot,1] = sharpe[ind]
wT = weights[ind,]
sharpe_TRUE = (wT %*% mean_vect_TRUE - mufree) /
   sqrt(wT %*% cov_mat_TRUE %*% wT)
out[iboot,2] = sharpe_TRUE
}
out_NoShort = out

boxplot(out_NoShort~gp,main="(b) No Short Sales",ylim=c(0,.7))
abline(h=.3503,lwd=3,lty=2)
graphics.off()

apply(out_NoShort,2,mean)







