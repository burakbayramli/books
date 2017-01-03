library("fEcofin")
x = midcapD.ts 
market = 100*as.matrix(x[,22])
x = as.matrix(x[,-c(1,22)])
postscript("midcapDPairs.ps",width=6,height=6) # Fig 7.4
pairs(x[,1:6])
graphics.off()