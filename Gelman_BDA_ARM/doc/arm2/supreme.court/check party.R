load("D:/scj_run03.05.23.RData")
ideal<-as.vector(round(sc.sim$summary[1:29,1],1))

library(foreign)
party.data<-read.dta("c:/court run/sc_realtimecong_BUGS_party.dta", convert.factors=F)
attach(party.data)



lab<-seq(1:29)
party<-party+runif(length(party),-.1,.1)
plot(party,ideal,type="n")
text(party,ideal,lab)


#mention using party variable and constraining it to 0 is special case of fixing anchor justice
