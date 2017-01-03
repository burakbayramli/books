"Igarch" <- function(rtn,include.mean=F,volcnt=F){
# Estimation of a Gaussian IGARCH(1,1) model.
# rtn: return series 
# include.mean: flag for the constant in the mean equation.
# volcnt: flag for the constant term of the volatility equation.
#### default is the RiskMetrics model
#
Idata <<- rtn
Flag <<- c(include.mean,volcnt)
#
Mean=mean(Idata); Var = var(Idata); S = 1e-6
if((volcnt)&&(include.mean)){
params=c(mu = Mean,omega=0.1*Var,beta=0.85)
lowerBounds = c(mu = -10*abs(Mean), omega= S^2, beta= S)
upperBounds = c(mu = 10*abs(Mean), omega = 100*Var, beta = 1-S)
}
if((volcnt)&&(!include.mean)){
params=c(omega=0.1*Var, beta=0.85)
lowerBounds=c(omega=S^2,beta=S)
upperBounds=c(omega=100*Var,beta=1-S)
}
#
if((!volcnt)&&(include.mean)){
params=c(mu = Mean, beta= 0.8)
lowerBounds = c(mu = -10*abs(Mean), beta= S)
upperBounds = c(mu = 10*abs(Mean), beta = 1-S)
}
if((!volcnt)&&(!include.mean)){
params=c(beta=0.85)
lowerBounds=c(beta=S)
upperBounds=c(beta=1-S)
}
# Step 3: set conditional distribution function:
igarchDist = function(z,hh){dnorm(x = z/hh)/hh}
# Step 4: Compose log-likelihood function:
igarchLLH = function(parm){
include.mean=Flag[1]
volcnt=Flag[2]
mu=0; omega = 0
if((include.mean)&&(volcnt)){
my=parm[1]; omega=parm[2]; beta=parm[3]}
if((!include.mean)&&(volcnt)){
omega=parm[1];beta=parm[2]}
if((!include.mean)&&(!volcnt))beta=parm[1]
if((include.mean)&&(!volcnt)){mu=parm[1]; beta=parm[2]}
#
z = (Idata - mu); Meanz = mean(z^2)
e= omega + (1-beta)* c(Meanz, z[-length(Idata)]^2)
h = filter(e, beta, "r", init=Meanz)
hh = sqrt(abs(h))
llh = -sum(log(igarchDist(z, hh)))
llh
}
# Step 5: Estimate Parameters and Compute Numerically Hessian:
fit = nlminb(start = params, objective = igarchLLH,
lower = lowerBounds, upper = upperBounds)
##lower = lowerBounds, upper = upperBounds, control = list(trace=3))
epsilon = 0.0001 * fit$par
cat("Estimates: ",fit$par,"\n")
npar=length(params)
Hessian = matrix(0, ncol = npar, nrow = npar)
for (i in 1:npar) {
for (j in 1:npar) {
x1 = x2 = x3 = x4  = fit$par
x1[i] = x1[i] + epsilon[i]; x1[j] = x1[j] + epsilon[j]
x2[i] = x2[i] + epsilon[i]; x2[j] = x2[j] - epsilon[j]
x3[i] = x3[i] - epsilon[i]; x3[j] = x3[j] + epsilon[j]
x4[i] = x4[i] - epsilon[i]; x4[j] = x4[j] - epsilon[j]
Hessian[i, j] = (igarchLLH(x1)-igarchLLH(x2)-igarchLLH(x3)+igarchLLH(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Maximized log-likehood: ",igarchLLH(fit$par),"\n")
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)

if((include.mean)&&(volcnt)){
mu=fit$par[1]; omega=fit$par[2]; beta = fit$par[3]
}
if((include.mean)&&(!volcnt)){
mu = fit$par[1]; beta = fit$par[2]; omega = 0
}
if((!include.mean)&&(volcnt)){
mu=0; omega=fit$par[1]; beta=fit$par[2]
}
if((!include.mean)&&(!volcnt)){
mu=0; omega=0; beta=fit$par[1]
}
z=Idata-mu; Mz = mean(z^2)
e= omega + (1-beta)*c(Mz,z[-length(z)]^2)
h = filter(e,beta,"r",init=Mz)
vol = sqrt(abs(h))

Igarch <- list(par=fit$par,volatility = vol)
}

