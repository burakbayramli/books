"RMfit" <- function(rtn){
# Estimation of the RiskMetrics special IGARCH(1,1) model.
# rtn: return series 
#
Idata <<- rtn
Var = var(Idata); S = 1e-6
params=c(alpha = 0.9)
lowerBounds = c(alpha= S)
upperBounds = c(alpha = 1-S)
# Step 3: set conditional distribution function:
igarchDist = function(z,hh){dnorm(x = z/hh)/hh}
# Step 4: Compose log-likelihood function:
igarchLLH = function(parm){
alpha=parm[1]
z = Idata; Meanz=mean(z^2)
e= (1-alpha) * c(Meanz, z[-length(Idata)]^2)
h = filter(e, alpha, "r", init=Meanz)
hh = sqrt(abs(h))
llh = -sum(log(igarchDist(z, hh)))
llh
}
print(igarchLLH(params))
# Step 5: Estimate Parameters and Compute Numerically Hessian:
fit = nlminb(start = params, objective = igarchLLH,
lower = lowerBounds, upper = upperBounds, control = list(trace=3))
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
Hessian[i, j] = (igarchLLH(x1)-igarchLLH(x2)-igarchLLH(x3)+igarchLLH(x4))/
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

z=Idata; Mz = mean(z^2); alpha=fit$par[1]
e= (1-alpha)*c(Mz,z[-length(z)]^2)
h = filter(e,alpha, "r", init=Mz)
vol = sqrt(abs(h))

# Step 7: compute VaR and ES based on the estimated results
T=length(h)
Sigma=alpha*h[T]+(1-alpha)*z[T]^2
Vpred=sqrt(Sigma)
tt=matrix(c(T,Vpred),1,2)
colnames(tt) <- c("Orig","Vpred")
cat("\n Volatility prediction:\n")
print(tt)
prob=c(0.95,0.99,0.999)
q1=qnorm(prob)
d1=dnorm(q1); d11=d1/(1-prob)
VaR=q1*Vpred
ES=d11*Vpred
tbl=cbind(prob,VaR,ES)
cat("\n Risk measure based on RiskMetrics:\n")
print(tbl)

RMfit <- list(par=fit$par,volatility = vol)
}

