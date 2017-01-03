#  Example 16.3

dat = read.csv("capm.csv",header=T)
attach(dat)
n = dim(dat)[1]
EX_R_sp500 = Close.sp500[2:n]/Close.sp500[1:(n-1)] - 1  - Close.tbill[2:n]/(100*253) 
EX_R_msft = Close.msft[2:n]/Close.msft[1:(n-1)] - 1  - Close.tbill[2:n]/(100*253) 
fit = lm(EX_R_msft~EX_R_sp500)
options(digits=3)
summary(fit)

fit_NoInt = lm(EX_R_msft~EX_R_sp500-1)
options(digits=3)
summary(fit_NoInt)