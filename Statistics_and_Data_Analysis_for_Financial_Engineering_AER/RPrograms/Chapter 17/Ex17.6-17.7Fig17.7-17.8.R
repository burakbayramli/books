#  Examples 17.6 and 17.7 and Figures 17.7 and 17.8
#  Uses monthly data from Jan-69 to Dec-98

FF_data = read.table("FamaFrench_mon_69_98.txt",header=T)
attach(FF_data)
library("Ecdat")

data(CRSPmon)
ge = 100*CRSPmon[,1] - RF
ibm = 100*CRSPmon[,2] - RF
mobil = 100*CRSPmon[,3] - RF
stocks=cbind(ge,ibm,mobil)

postscript("FamaFrenchPairs.ps",width=8,height=8)  #  Figure 17.7
pairs(cbind(ge,ibm,mobil,Mkt.RF,SMB,HML))
graphics.off()

fit = lm(cbind(ge,ibm,mobil)~Mkt.RF+SMB+HML)
fit
summary(fit)

cor(fit$residuals)
cor.test(fit$residuals[,1], fit$residuals[,2])
cor.test(fit$residuals[,1], fit$residuals[,3])
cor.test(fit$residuals[,2], fit$residuals[,3])

postscript("FamaFrenchResidualsPairs.ps",width=6,height=5)  #  Figure 17.8
pairs(fit$residuals)
graphics.off()




sigF = as.matrix(var(cbind(Mkt.RF,SMB,HML)))  #  Example 17.7
bbeta = as.matrix(fit$coef)
bbeta = t( bbeta[-1,])
n=dim(CRSPmon)[1]
sigeps = (n-1)/(n-4) * var(as.matrix(fit$resid))
sigeps = diag(diag(as.matrix(sigeps)))


cov_equities = bbeta %*% sigF %*% t(bbeta) + sigeps

cov(cbind(ge,ibm,mobil))

