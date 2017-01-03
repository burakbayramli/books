dat = read.table("CreditCard_fromGreene.txt")

  Cardhldr,  Majordrg,    Age,         Income,      Exp_inc,
   Avgexp,    Ownrent,    Selfempl,     Depndt,     Inc_per,
   Cur_add,   Major,      Active  $

dat = scan("CreditCard_fromGreene.txt")
dat2=matrix(dat,ncol=13,byrow=T)
dat2[1:5,]
qqnorm(dat2[,3])
