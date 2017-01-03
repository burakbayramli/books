library("fEcofin")


data(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)

par(mfrow=c(4,4))
for (i in 2:15){
plot(nelsonplosser[,i],
  main=names(nelsonplosser)[i])
}

par(mfrow=c(4,4))
for (i in 2:15){
plot(diff(nelsonplosser[,i]),
  main=c("diff",names(nelsonplosser)[i]))
}

par(mfrow=c(4,4))
for (i in 2:15){
plot(diff(log(nelsonplosser[,i])),
  main=c("diff log ",names(nelsonplosser)[i]))
}

n = length(gnp.r)
year = 1909 + (1970-1909)*(0:(n-2))/n

postscript("nelsonPlosser_differences.ps",width=6,height=5)
par(mfrow=c(2,3),cex.lab=1.35)
plot(year,diff(gnp.r),type="b",ylab="differences",main="gnp.r")
plot(year,diff(log(gnp.r)),type="b",ylab="differences",main="log(gnp.r)")
plot(year,diff(sqrt(gnp.r)),type="b",ylab="differences",main="sqrt(gnp.r)")

plot(year,diff(ip),type="b",ylab="differences",main="ip")
plot(year,diff(log(ip)),type="b",ylab="differences",main="log(ip)")
plot(year,diff(sqrt(ip)),type="b",ylab="differences",main="sqrt(ip)")
graphics.off()




