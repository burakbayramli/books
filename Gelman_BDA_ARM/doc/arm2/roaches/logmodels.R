# log models

roachcounts <- read.csv ("RoachCounts.csv")

# Just keep the data that were measured in round 2 (as with other ipm data)

ipm <- read.csv ("IPM_BASELINE_R2.csv")
ok <- ipm[["hasround2"]]==1
roachcounts <- roachcounts[ok,]

attach.all (ipm[ok,])
# get roach1 and roach2 from another file!

# Quickly look at the data

roachcounts[1:3,]

# rename some variables of interest

count2 <- roachcounts[["roachsum2"]]
count1 <- roachcounts[["roachsum1"]]
exposure2 <- (5-roachcounts[["trapmiss2"]])*roachcounts[["trapdays2"]]/(5*7)
exposure1 <- (5-roachcounts[["trapmiss1"]])*roachcounts[["trapdays1"]]/(5*7)
sqrt.roach2 <- sqrt (roach2)
sqrt.roach1 <- sqrt (roach1)

# check that summary data are consistent with raw data

par (mfrow=c(2,2))
plot(count2/exposure2, roach2)
plot(count1/exposure1, roach1)

# fit a simple poisson regression

M101 <- glm (count2 ~ sqrt.roach1 + treatment + senior, family=poisson, offset=log(exposure2))
display (M101)

# overdispersed

M102 <- glm (count2 ~ sqrt.roach1 + treatment + senior, family=quasipoisson, offset=log(exposure2))
display (M102)

# overdispersed and multilevel

M103 <- lmer (count2 ~ sqrt.roach1 + treatment + senior + (1 | building), family=quasipoisson, offset=log(exposure2))
display (M103)

# prediction with r1=median level (which is 13 for non-senior apts)

beta.hat <- fixef(M103)
x.mid.T <- exp (c (1, sqrt(median(roach1[senior==0])), 1, 0) %*% beta.hat)
x.mid.C <- exp (c (1, sqrt(median(roach1[senior==0])), 0, 0) %*% beta.hat)
diff <- x.mid.T - x.mid.C
ratio <- x.mid.T/x.mid.C

