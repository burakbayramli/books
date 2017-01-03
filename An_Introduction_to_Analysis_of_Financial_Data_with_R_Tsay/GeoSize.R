"GeoSize" <- function(Si,xre=NULL)
{
# Estimation of geometric distribution for sizes of a price change
# Step 1: Initialize Time Series Globally:
if(length(xre) == 0){
k=0}
else{
xre=as.matrix(xre)
k=dim(xre)[2]
}
GSi <<- Si; Xre <<- xre
# Step 2: Initialize Model Parameters and Bounds:
Mean=mean(Si); P=1/(Mean+1); Ome= log(P/(1-P))
if(k > 0) Ome=c(Ome,rep(1,k))
params = c(omega = Ome)
lowerBounds = c(omega = -10*abs(Ome))
upperBounds = c(omega = 10*abs(Ome))
# Step 3: Set Conditional Distribution Function:
geomDist = function(Si,pp) { 
LL=dgeom(Si-1,pp,log=TRUE)
LL
}
# Step 4: Compose log-Likelihood Function:
geomLLH = function(parm) {
if(length(Xre)==0){
k=0}
else{
k=dim(Xre)[2]
}
om=parm[1]
if(k > 0){
for (i in 1:k){
om=om+parm[1+i]*Xre[,i]
}
}
p1=exp(om)
pp=p1/(1+p1)
llh = -sum(geomDist(GSi,pp))
llh }
print(geomLLH(params))
# Step 5: Estimate Parameters and Compute Numerically Hessian:
fit = nlminb(start = params, objective = geomLLH,
lower = lowerBounds, upper = upperBounds, control = list(trace=3))
cat("Estimates: ",fit$par,"\n")
epsilon = 0.0001 * fit$par
npar=length(params)
Hessian = matrix(0, ncol = npar, nrow = npar)
for (i in 1:npar) {
for (j in 1:npar) {
x1 = x2 = x3 = x4  = fit$par
x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
Hessian[i, j] = (geomLLH(x1)-geomLLH(x2)-geomLLH(x3)+geomLLH(x4))/
(4*epsilon[i]*epsilon[j])
}
}
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
# compute output
est=fit$par

GeoSize <- list(par=est)
}