dat = read.table("bondprices.txt",header=T)

fit = nls(price~1000*exp(-r*maturity),data=dat,start=list(r=.04))