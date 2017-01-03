# Ex 7.1 and Fig 7.1

data(CRSPday,package="Ecdat")
ge = CRSPday[,4]
ibm = CRSPday[,5]
options(digits=3)
cov(CRSPday[,4:7])
cor(CRSPday[,4:7])

postscript("CRSPday_scatterplot.ps",width=7,height=7) # Fig 7.1

plot(as.data.frame(CRSPday[,4:7]))

graphics.off()


