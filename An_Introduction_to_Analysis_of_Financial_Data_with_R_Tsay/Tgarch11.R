Tgarch11 = function(x,cond.dist="norm")
{
# Estimation of TGARCH(1,1) model with Gaussian or Student-t innovations
# Step 1: Initialize Time Series Globally:
Tx <<- x
# Step 2: Initialize Model Parameters and Bounds:
Meanx = mean(Tx); Varx = var(Tx); S = 1e-6
if(cond.dist=="std"){
params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81, shape=6)
lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S, shape=3)
upperBounds = c(mu = 10*abs(Meanx), omega = 100*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S, shape=30)
}
else{
params = c(mu = Meanx, omega = 0.1*Varx, alpha = 0.1, gam1= 0.02, beta = 0.81)
lowerBounds = c(mu = -10*abs(Meanx), omega = S^2, alpha = S, gam1=S, beta = S)
upperBounds = c(mu = 10*abs(Meanx), omega = 10*Varx, alpha = 1-S, gam1 = 1-S, beta = 1-S)
}
# Step 3: Set Conditional Distribution Function:
garchDist = function(z, hh, cond.dist, nu1) { 
if(cond.dist=="std"){LL=dstd(x = z/hh, nu=nu1)/hh}
else{
LL=dnorm(x = z/hh)/hh }
LL
}
# Step 4: Compose log-Likelihood Function:
garchLLH = function(parm) {
mu = parm[1]; omega = parm[2]; alpha = parm[3]; gam1=parm[4]; beta = parm[5]
shape = 0; 
if(length(parm)==6){
shape=parm[6]
cond.dist="std"
}
else
{cond.dist="norm"}
z = (Tx-mu); Mean = mean(z^2)
zm1=c(0,z[-length(z)])
idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
# Use Filter Representation:
e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
h = filter(e, beta, "r", init = Mean)
hh = sqrt(abs(h))
llh = -sum(log(garchDist(z, hh, cond.dist, shape)))
llh }
# Step 5: Estimate Parameters and Compute Numerically Hessian:
fit = nlminb(start = params, objective = garchLLH,
lower = lowerBounds, upper = upperBounds) ### control = list(trace=3))
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
Hessian[i, j] = (garchLLH(x1)-garchLLH(x2)-garchLLH(x3)+garchLLH(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Log likelihood at MLEs: ","\n")
print(-garchLLH(fit$par))
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
mu = est[1]; omega = est[2]; alpha = est[3]; gam1=est[4]; beta = est[5]
z=(Tx-mu); Mean = mean(z^2)
zm1=c(0,z[-length(z)])
idx=seq(zm1)[zm1 < 0]; z1=rep(0,length(z)); z1[idx]=1
e = omega + alpha * c(Mean, z[-length(z)]^2) + gam1*z1*c(Mean,z[-length(z)]^2)
h = filter(e, beta, "r", init = Mean)
sigma.t = sqrt(abs(h))

Tgarch11 <- list(residuals = z, volatility = sigma.t, par=est)
}