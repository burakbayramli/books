library("fEcofin")     #  for bmw return data
library("bootstrap")

quantKurt = function(y,p1=0.025,p2=0.25)
{
Q = quantile(y,c(p1,p2,1-p2,1-p1))
(Q[4]-Q[1]) / (Q[3]-Q[2])
}

set.seed("5640")
bca_kurt= bcanon(bmwRet[,2],5000,quantKurt)
bca_kurt$confpoints

