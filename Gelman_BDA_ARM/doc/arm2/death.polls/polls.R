postscript ("c:/books/multilevel/deathpolls.ps", horizontal=TRUE)
par (mar=c(5,5,4,2)+.1)
polls <- matrix (scan("polls.dat"), ncol=5, byrow=TRUE)
support <- polls[,3]/(polls[,3]+polls[,4])
year <-  polls[,1] + (polls[,2]-6)/12
plot (year, support*100, xlab="Year", ylim=c(min(100*support)-1, max(100*support)+1),
      ylab="Percentage support for the death penalty", cex=2, cex.main=2,
      cex.axis=2, cex.lab=2, pch=20)
for (i in 1:nrow(polls))
  lines (rep(year[i],2), 100*(support[i]+c(-1,1)*sqrt(support[i]*(1-support[i])/1000)))
dev.off()
