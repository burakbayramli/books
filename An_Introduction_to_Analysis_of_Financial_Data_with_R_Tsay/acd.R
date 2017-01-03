"acd" <- function(x,order=c(1,1),cond.dist="exp",ini.est=NULL){
# Estimation of ACD models
## psi_t = omega + \sum_{i=1}^r alpha_i x_{t-i} + \sum_{j=1}^s \beta_j psi_{t-j}
## This program is written by Ruey S. Tsay on May 18, 2011.
##
r=order[1]
s=order[2]
ist=max(r,s)+1
# Assign global variables
Xacd <<- x
Acdorder <<- order

# Assign parameters
S=1e-6; Mean=mean(x)
inialpha=NULL; lowalpha=NULL; uppalpha=NULL
if(r > 0){
inialpha=rep(0.1/r,r); lowalpha=rep(S,r); uppalpha=rep(1-S,r)
}
inibeta=NULL; lowbeta=NULL; uppbeta=NULL
if(s > 0){
inibeta=rep(0.8/s,s); lowbeta=rep(S,s); uppbeta=rep(1-S,s)
}

if(cond.dist=="exp"){
lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta)
upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta)
if(length(ini.est)==0){
params=c(omega=Mean,alpha=inialpha,beta=inibeta)
}
else{ 
if(r > 0)inialpha=ini.est[2:(1+r)]
if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta)
}
##
fit = nlminb(start = params, objective = explk,
lower = lowerB, upper = upperB)
##lower = lowerB, upper = upperB, control = list(trace=3))
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
Hessian[i, j] = (explk(x1)-explk(x2)-explk(x3)+explk(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Maximized log-likehood: ",-explk(fit$par),"\n")
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
}

if(cond.dist=="weibull"){
lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta,shape=S)
upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta,shape=10)
if(length(ini.est)==0){
params=c(omega=Mean,alpha=inialpha,beta=inibeta,shape=1)
}
else{ 
if(r > 0)inialpha=ini.est[2:(1+r)]
if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta,shape=ini.est[1+r+s+1])
}
##
fit = nlminb(start = params, objective = weilk,
lower = lowerB, upper = upperB)
##lower = lowerB, upper = upperB, control = list(trace=3))
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
Hessian[i, j] = (weilk(x1)-weilk(x2)-weilk(x3)+weilk(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Maximized log-likehood: ",-weilk(fit$par),"\n")
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
}

if(cond.dist=="gamma"){
lowerB=c(omega=S,alpha=lowalpha,beta=lowbeta,power=S*1000,shape=S)
upperB=c(omega=5*Mean,alpha=uppalpha,beta=uppbeta,power=5,shape=30)
if(length(ini.est)==0){
params=c(omega=Mean,alpha=inialpha,beta=inibeta,power=1,shape=1.2)
}
else{ 
if(r > 0)inialpha=ini.est[2:(1+r)]
if(s > 0)inibeta=ini.est[(1+r+1):(1+r+s)]
params=c(omega=ini.est[1],alpha=inialpha,beta=inibeta,power=ini.est[2+r+s],shape=ini.est[3+r+s])
}
##
fit = nlminb(start = params, objective = gamlk,
lower = lowerB, upper = upperB)
##lower = lowerB, upper = upperB, control = list(trace=3))
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
Hessian[i, j] = (gamlk(x1)-gamlk(x2)-gamlk(x3)+gamlk(x4))/
(4*epsilon[i]*epsilon[j])
}
}
cat("Maximized log-likehood: ",-gamlk(fit$par),"\n")
# Step 6: Create and Print Summary Report:
se.coef = sqrt(diag(solve(Hessian)))
tval = fit$par/se.coef
matcoef = cbind(fit$par, se.coef, tval, 2*(1-pnorm(abs(tval))))
dimnames(matcoef) = list(names(tval), c(" Estimate",
" Std. Error", " t value", "Pr(>|t|)"))
cat("\nCoefficient(s):\n")
printCoefmat(matcoef, digits = 6, signif.stars = TRUE)
}

# compute standardized residuals
par=fit$par
T=length(x)
psit=rep(par[1],(T-ist+1))
if(r > 0){
for (i in 1:r){
psit=psit+par[1+i]*x[(ist-i):(T-i)]
}
}
psi=psit
if(s > 0){
ini=rep(Mean,s)
beta=par[(1+r+1):(1+r+s)]
psi=filter(psit,beta,"r",init=ini)
}
resi=x[ist:T]/psi

acd <- list(estimates=par,Hessian=Hessian,epsilon=resi)
}

explk <- function(par){
x=Xacd
Mean=mean(x)
order=Acdorder
T=length(x)
r=order[1]
s=order[2]
ist=max(r,s)+1
psit=rep(par[1],(T-ist+1))
if(r > 0){
for (i in 1:r){
psit=psit+par[1+i]*x[(ist-i):(T-i)]
}
}
if(s > 0){
ini=rep(Mean,s)
beta=par[(1+r+1):(1+r+s)]
psi=filter(psit,beta,"r",init=ini)
ept=x[ist:T]
explk=-sum(log(dexp(ept/psi)/psi))
}
}

weilk <- function(par){
x=Xacd
order=Acdorder
Mean=mean(x)
T=length(x)
r=order[1]
s=order[2]
ist=max(r,s)+1
psit=rep(par[1],(T-ist+1))
if(r > 0){
for (i in 1:r){
psit=psit+par[1+i]*x[(ist-i):(T-i)]
}
}
if(s > 0){
ini=rep(Mean,s)
beta=par[(1+r+1):(1+r+s)]
psi=filter(psit,beta,"r",init=ini)
}
ept=x[ist:T]/psi
shape=par[r+s+2]
c1=gamma(1+(1/shape))
tmp1=shape*c1^shape*ept^(shape-1)/psi
tmp=tmp1*exp(-(c1*ept)^shape)
weilk = -sum(log(tmp))
}
#
gamlk <- function(par){
x=Xacd
order=Acdorder
Mean=mean(x)
T=length(x)
r=order[1]
s=order[2]
ist=max(r,s)+1
power=par[2+r+s]
shape=par[3+r+s]
c1=gamma(shape)
c2=gamma(shape+(1/power))
lambda=c1/c2
c3=shape*power
psit=rep(par[1],(T-ist+1))
if(r > 0){
for (i in 1:r){
psit=psit+par[1+i]*x[(ist-i):(T-i)]
}
}
psi=psit
if(s > 0){
ini=rep(Mean,s)
beta=par[(1+r+1):(1+r+s)]
psi=filter(psit,beta,"r",init=ini)
}
ept=x[ist:T]
tmp1=log(power/c1)+(c3-1)*log(ept)-c3*log(lambda*psi)
tmp= tmp1-(ept/(lambda*psi))^power
gamlk=-sum(tmp)
}
