library ("foreign")
pres <- read.dta ("../doc/gelman/ARM_Data/nes/nes5200_processed_voters_realideo.dta",convert.factors=F)
#str(pres) # show data description
attach(pres)

# data clean up, get only 1992 data and 
# turn prez vote into 1 or 0
yr <- 1992
  ok <- year==yr & presvote<3
  vote <- presvote[ok] - 1
  income <- income[ok]
  
print ('-------------------------------------------------------')
print (vote)
print ('-------------------------------------------------------')
print (income)
  
fit.1 <- glm (vote ~ income, family=binomial(link="logit"))
print (fit.1)
