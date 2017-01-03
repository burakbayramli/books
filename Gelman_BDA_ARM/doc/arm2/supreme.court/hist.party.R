#load scj_party_final2 in zip file

invisible(setwd("c:/Ideal Point Innov"))

postscript("hist.party.ps", height=7, horizontal=T)

hist(sc.sim$sims.list$z.gamma.party, main="", xlab="", cex.axis=1.4, cex.lab=1.5, breaks=20)
#, main="Histogram of Posterior Distribution for Party Predictor"
dev.off()
