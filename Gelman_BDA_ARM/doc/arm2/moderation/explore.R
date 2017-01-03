# Load in data for region indicators
# Use "state", an R data file (type ?state from the R command window for info)
#
# Regions:  1=northeast, 2=south, 3=north central, 4=west

if (0){

data (state)                  # "state" is an R data file
region <- c(3,4,4,3,4,4,1,1,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)
south <- region==3

# read in ideo.data and ideo2.data

library(foreign)
ideo.data <- read.S(".Data/ideo.data")
ideo2.data <- read.S(".Data/ideo2.data")

incadv <- function (years){
  ifelse (years<46, .02,
    ifelse (years<66, .02 + .08*(years-46)/(66-46), .10))
}

## Create normal vote
# candidate.effects <- .5 * region effect + home state adv + south effect +
#                     + Catholic effect + candidate ideology effect
# will appear as matrix with 3 columns:  years, states, effects.
# align as matrix with columns of years (48-92) and rows of states (AL-WY)
#
## Create dvfix (corrected for incumbency)
#
# add stuff to ideo.data
#

write (t(cbind(states,round(candidate.effects,4))), "candidate_effects.txt", ncol=13)

normvote <- rep (NA, nrow(ideo.data))
dum1 <- ideo.data[,"dpvote"]
dum2 <- ideo.data[,"stgj"]
dum3 <- (ideo.data[,"year"]-44)/4
for (i in 1:length(normvote))
  normvote[i] <- dum1[i] - candidate.effects[dum2[i],dum3[i]]
dvfix <- ideo.data[,"dv"] - incadv(ideo.data[,"year"])*ideo.data[,"inc"]
ideo.data <- cbind(ideo.data[,1:15], normvote, dvfix)
names(ideo.data) <- c(names(ideo.data)[1:15], "normvote", "dvfix")

# add stuff to ideo2.data

normvote <- rep (NA, nrow(ideo2.data))
dum1 <- ideo2.data[,"dpvote"]
dum2 <- ideo2.data[,"stgj"]
dum3 <- (ideo2.data[,"year"]-44)/4
for (i in 1:length(normvote))
  normvote[i] <- dum1[i] - candidate.effects[dum2[i],dum3[i]]
dvfix <- ideo2.data[,"dv"] - incadv(ideo2.data[,"year"])*ideo2.data[,"i2"]
dvpfix <- ideo2.data[,"dvp"] - incadv(ideo2.data[,"year"]-2)*ideo2.data[,"i1"]
ideo2.data <- cbind(ideo2.data[,1:19], normvote, dvfix, dvpfix)
names(ideo2.data) <- c(names(ideo2.data)[1:19], "normvote", "dvfix", "dvpfix")


# impute normal vote from 2 years earlier:  ideo.data

for (year in seq(62,94,4)){
  yr.cond <- ideo.data[, "year"] == year
  normvote <- rep (NA, sum(yr.cond))
  dpvote <- rep (NA, sum(yr.cond))
  stgj <- ideo.data[yr.cond,"stgj"]
  cd <- ideo.data[yr.cond,"cd"]
  indexes <- (1:nrow(ideo.data))[yr.cond]
  yr2.cond <- ideo.data[, "year"] == year-2
  data2 <- ideo.data[yr2.cond,c("stgj","cd","normvote","dpvote")]
  for (i in 1:sum(yr.cond)){
    cond <- data2[,"stgj"]==stgj[i] & data2[,"cd"]==cd[i]
    if (sum(cond)==1){
      normvote[i] <- data2[cond,c("normvote")]
      dpvote[i] <- data2[cond,c("dpvote")]
    }
  }
  ideo.data[yr.cond,c("normvote")] <- normvote
  ideo.data[yr.cond,c("dpvote")] <- dpvote
}

# impute normal vote from 2 years earlier:  ideo2.data

for (year in seq(62,94,4)){
  yr.cond <- ideo2.data[, "year"] == year
  normvote <- rep (NA, sum(yr.cond))
  dpvote <- rep (NA, sum(yr.cond))
  stgj <- ideo2.data[yr.cond,"stgj"]
  cd <- ideo2.data[yr.cond,"cd"]
  indexes <- (1:nrow(ideo2.data))[yr.cond]
  yr2.cond <- ideo2.data[, "year"] == year-2
  data2 <- ideo2.data[yr2.cond,c("stgj","cd","normvote","dpvote")]
  for (i in 1:sum(yr.cond)){
    cond <- data2[,"stgj"]==stgj[i] & data2[,"cd"]==cd[i]
    if (sum(cond)==1){
      normvote[i] <- data2[cond,c("normvote")]
      dpvote[i] <- data2[cond,c("dpvote")]
    }
  }
  ideo2.data[yr.cond,c("normvote")] <- normvote
  ideo2.data[yr.cond,c("dpvote")] <- dpvote
}

}


plan1 <- function(year)
{
  summ <- year
if (year <= 88){
	yr.cond <- ideo.data[, "year"] == year
	data <- ideo.data[yr.cond,  ]
	deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"inc"]) == 1)
	repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"inc"]) == 1)
	dum <- apply(is.na(data),1,sum)
	ok <- dum==0 & !south[data[,"stgj"]]
#	print (paste (sum(ok), "ideo.data:  data points for year", 1900+year))
	print (c(year,mean(data[ok & deminc.cond,"score1"]),
	  mean(data[ok & repinc.cond,"score1"])))
 
## Raw plot of poole score vs dvp
        plot (data[ok,"dvp"], data[ok,"score1"], type="n",
	  xlab="previous vote", ylab="Poole score")
	dem <- data[,"dvp"] > .5
	text (data[ok,"dvp"], data[ok,"score1"],
	  ifelse (data[ok,"dvp"] > .5, "*", "o"))
	title (paste ("poole score vs dvp,", 1900+year), cex=.4)

## Raw plot of poole score vs normal vote
	plot (data[ok,"normvote"], data[ok,"score1"], type="n",
	  xlab="corrected pres vote", ylab="Poole score")
	text (data[ok,"normvote"], data[ok,"score1"],
	  ifelse (data[ok,"dvp"] > .5, "*", "o"))
	title (paste ("poole score vs pres vote,", 1900+year), cex=.4)

# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")

}
else{
# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")
# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")
# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")
}

# switch to ideo2.data
	yr.cond <- ideo2.data[, "year"] == year
	data <- ideo2.data[yr.cond,  ]
	deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
	repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
	if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
	dum <- apply(is.na(data),1,sum)
	ok <- dum==0 & !south[data[,"stgj"]]
#	print (paste (sum(ok), "ideo2.data:  data points for year", 1900+year))
	print (c(year,mean(data[ok & deminc.cond,"score1"]),
	  mean(data[ok & repinc.cond,"score1"])))

if (year%%10!=2){
## Results of regression of dv on dvp and poole score, for Dem incs only
## Result should be negative.  Also, scatterplot of resid of
## (regression of dv on dvp) vs poole score, for Dem incs.  Neg slope is clear.
	lm1 <- lm(dvfix ~ dvpfix, data = data[deminc.cond & ok,])
	lm2 <- lm(dvfix ~ dvpfix + score1, data = data[deminc.cond & ok,])
	plot (data[deminc.cond & ok,"score1"], lm1$residuals, pch="*",
	  xlab="Poole score", ylab="resid of regression of dv on dvp")
	lm3 <- lsfit (data[deminc.cond & ok, "score1"], lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of dv on dvp, vs poole score,", 1900+year,
	  "\n Democratic incs only"), cex=.4)
}
else
# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")

## Results of regression of dv on normalvote and poole score, for Dem incs only
	lm1 <- lm(dvfix ~ normvote, data = data[deminc.cond & ok,])
	lm2 <- lm(dvfix ~ normvote + score1, data = data[deminc.cond & ok,])
  display (lm2)
  summ <- c (summ, summary(lm2)$coef[3,1:2]*sd(data[deminc.cond&ok,"score1"]))
  print (sum(ok))
  print (sum(deminc.cond&ok))
  print (data[1:10,])
  print (data[1:10,c("dvfix","normvote","score1")])
	plot (data[deminc.cond & ok,"score1"], lm1$residuals, pch="*",
	  xlab="Poole score", ylab="resid of regression of dv on normal vote")
	lm3 <- lsfit (data[deminc.cond & ok, "score1"], lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of dv on pres vote, vs poole score,", 1900+year,
	  "\n Democratic incs only"), cex=.4)

## Results of regression of dv on normalvote and corrected poole score, for Dem incs only
	lm1 <- lm(dvfix ~ normvote, data = data[deminc.cond & ok,])
	lm2 <- lm(dvfix ~ normvote + score1, data = data[deminc.cond & ok,])
	lm4 <- lsfit (data[deminc.cond & ok, "normvote"],
	  data[deminc.cond & ok, "score1"])
	pooleresids <- lm4$residuals
	plot (pooleresids, lm1$residuals, pch="*",
	  xlab="resid of regression of Poole score on normal vote",
	  ylab="resid of regression of dv on normal vote")
	lm3 <- lsfit (pooleresids, lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of dv on pres vote, vs poole resids,", 1900+year,
	  "\n Democratic incs only"), cex=.4)

if (year%%10!=2){
## Results of regression of dv on dvp and poole score, for Rep incs only
	lm1 <- lm(dvfix ~ dvpfix, data = data[repinc.cond & ok,])
	lm2 <- lm(dvfix ~ dvpfix + score1, data = data[repinc.cond & ok,])
	plot (data[repinc.cond & ok,"score1"], -lm1$residuals, pch="o",
	  xlab="Poole score", ylab="resid of regression of -dv on dvp")
	lm3 <- lsfit (data[repinc.cond & ok, "score1"], -lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of -dv on dvp, vs poole score,", 1900+year,
	  "\n Republican incs only"), cex=.4)
}
else
# blank plot
plot (0,0, type="n",axes=F,xlab="",ylab="")

## Results of regression of dv on normalvote and poole score, for Rep incs only
	lm1 <- lm(dvfix ~ normvote, data = data[repinc.cond & ok,])
	lm2 <- lm(dvfix ~ normvote + score1, data = data[repinc.cond & ok,])
  display (lm2)
  summ <- c (summ, summary(lm2)$coef[3,1:2]*sd(data[repinc.cond&ok,"score1"]))
	plot (data[repinc.cond & ok,"score1"], -lm1$residuals, pch="o",
	  xlab="Poole score", ylab="resid of regression of -dv on normal vote")
	lm3 <- lsfit (data[repinc.cond & ok, "score1"], -lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of -dv on pres vote, vs poole score,",1900+year,
	  "\n Republican incs only"), cex=.4)

## Results of regression of dv on normalvote and corrected poole score, for Rep  incs only
	lm1 <- lm(dvfix ~ normvote, data = data[repinc.cond & ok,])
	lm2 <- lm(dvfix ~ normvote + score1, data = data[repinc.cond & ok,])
        lm4 <- lsfit (data[repinc.cond & ok, "normvote"],
	  data[repinc.cond & ok, "score1"])
	pooleresids <- lm4$residuals
	plot (pooleresids, -lm1$residuals, pch="o",
	  xlab="resid of regression of Poole score on normal vote",
	  ylab="resid of regression of -dv on normal vote")
	lm3 <- lsfit (pooleresids, -lm1$residuals)
	lines (10*c(-1,1),lm3$coef[1] + 10*c(-1,1)*lm3$coef["X"])
	title (paste ("resids of -dv on pres vote, vs poole resids,",1900+year,
	  "\n Republican incs only"), cex=.4)
  return (summ)
}


plan2 <- function(year)
{

# switch to ideo2.data
	yr.cond <- ideo2.data[, "year"] == year
	data <- ideo2.data[yr.cond,  ]
	deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
	repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
	if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
attach.all(data)
  ok <- !is.na(deminc.cond) & !is.na(repinc.cond) &!is.na(score1) & !is.na(dv) & !is.na(normvote) & !south[data[,"stgj"]]


## Results of regression of dv on normalvote and poole score, for Dem incs only

        attach.all (data)
        plot (score1[deminc.cond&ok], dv[deminc.cond&ok], xlab="(liberal)                  ideology                (moderate)", ylab="vote share in 1994", cex.axis=1.5, cex.lab=1.5, pch="x", cex.main=1.5, main="Democrats running for reelection in 1994", xlim=c(-1,0))
       lm0 <-  lm(dv ~ score1, data = data[deminc.cond & ok,])
        abline (lm0$coef[1], lm0$coef[2])
#       plot (normvote[deminc.cond&ok], score1[deminc.cond&ok], xlab="normal vote in disctrict", ylab="ideology score", cex.axis=1.5, cex.lab=1.5, pch="D")
 
        lm1 <- lm(dv ~ normvote, data = data[deminc.cond & ok,])
        plot (score1[deminc.cond&ok], lm1$residuals, pch="x",
              cex.axis=1.5, cex.lab=1.5, xlab="(liberal)                  ideology                (moderate)", ylab="vote in 1994 compared to expected", xlim=c(-1,0))
        resids <- lm1$residuals
	lm3 <- lm (resids ~ score1[deminc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])


## Results of regression of dv on normalvote and poole score, for Rep incs only
        plot (score1[repinc.cond&ok], 1-dv[repinc.cond&ok], xlab="(moderate)              ideology             (conservative)", ylab="vote share in 1994", cex.axis=1.5, cex.lab=1.5, pch="o", cex.main=1.5, main="Republicans running for reelection in 1994")
       lm0 <-  lm(dv ~ score1, data = data[repinc.cond & ok,])
        abline (1-lm0$coef[1], -lm0$coef[2])
#        plot (normvote[repinc.cond&ok], score1[repinc.cond&ok], xlab="normal vote in disctrict", ylab="ideology score", cex.axis=1.5, cex.lab=1.5, pch="R")
	lm1 <- lm(dv ~ normvote, data = data[repinc.cond & ok,])
        plot (score1[repinc.cond&ok], -lm1$residuals, pch="o",
              cex.axis=1.5, cex.lab=1.5, xlab="(moderate)              ideology             (conservative)", ylab="vote in 1994 compared to expected")
        resids <- -lm1$residuals
	lm3 <- lm (resids ~ score1[repinc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])
}

plan3 <- function(year)
{

# switch to ideo2.data
	yr.cond <- ideo2.data[, "year"] == year
	data <- ideo2.data[yr.cond,  ]
	deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
	repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
	if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
	dum <- apply(is.na(data),1,sum)
	ok <- dum==0 & !south[data[,"stgj"]]


## Results of regression of dv on normalvote and poole score, for Dem incs only

        attach.all (data)
        keep <- (deminc.cond|repinc.cond)&ok
        plot (score1[deminc.cond&ok], dv[deminc.cond&ok], xlab="(liberal)                  ideology                (moderate)", ylab="vote share in 1994", cex.axis=1.5, cex.lab=1.5, pch="x", cex.main=1.5, main="Democrats running for reelection in 1994", xlim=c(-1,0))
       lm0 <-  lm(dv ~ score1, data = data[deminc.cond & ok,])
        abline (lm0$coef[1], lm0$coef[2])
#       plot (normvote[deminc.cond&ok], score1[deminc.cond&ok], xlab="normal vote in disctrict", ylab="ideology score", cex.axis=1.5, cex.lab=1.5, pch="D")
 
        lm1 <- lm(dv ~ normvote, data = data[deminc.cond & ok,])
        plot (score1[deminc.cond&ok], lm1$residuals, pch="x",
              cex.axis=1.5, cex.lab=1.5, xlab=
              "(liberal)                  ideology                (moderate)", ylab="vote in 1994 compared to expected", xlim=c(-1,0))
        resids <- lm1$residuals
	lm3 <- lm (resids ~ score1[deminc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])


## Results of regression of dv on normalvote and poole score, for Rep incs only
        plot (score1[repinc.cond&ok], 1-dv[repinc.cond&ok], xlab="(moderate)              ideology             (conservative)", ylab="vote share in 1994", cex.axis=1.5, cex.lab=1.5, pch="o", cex.main=1.5, main="Republicans running for reelection in 1994")
       lm0 <-  lm(dv ~ score1, data = data[repinc.cond & ok,])
        abline (1-lm0$coef[1], -lm0$coef[2])
#        plot (normvote[repinc.cond&ok], score1[repinc.cond&ok], xlab="normal vote in disctrict", ylab="ideology score", cex.axis=1.5, cex.lab=1.5, pch="R")
	lm1 <- lm(dv ~ normvote, data = data[repinc.cond & ok,])
        plot (score1[repinc.cond&ok], -lm1$residuals, pch="o",
              cex.axis=1.5, cex.lab=1.5, xlab="(moderate)              ideology             (conservative)", ylab="vote in 1994 compared to expected")
        resids <- -lm1$residuals
	lm3 <- lm (resids ~ score1[repinc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])
}

plan4 <- function(year)
{

# switch to ideo2.data
	yr.cond <- ideo2.data[, "year"] == year
	data <- ideo2.data[yr.cond,  ]
	deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
	repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
	if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
	dum <- apply(is.na(data),1,sum)
	ok <- dum==0 & !south[data[,"stgj"]]


## Results of regression of dv on normalvote and poole score, for Dem incs only

        attach.all (data)
        keep <- (deminc.cond|repinc.cond)&ok
 
        lm1 <- lm(dv ~ dvp, data = data[deminc.cond & ok,])
        plot (score1[deminc.cond&ok], lm1$residuals, pch="x",
              cex.axis=1.2, cex.lab=1.2, xlab=
"(liberal)                  ideology                (moderate)",
              ylab="residual given 1992 vote", xlim=c(-1,0), cex.main=1.2,main="Democrats running for reeelection in 1994")
        resids <- lm1$residuals
	lm3 <- lm (resids ~ score1[deminc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])


## Results of regression of dv on normalvote and poole score, for Rep incs only
	lm1 <- lm(dv ~ dvp, data = data[repinc.cond & ok,])
        plot (score1[repinc.cond&ok], -lm1$residuals, pch="o",
              cex.axis=1.2, cex.lab=1.2, xlab=
"(moderate)              ideology             (conservative)", ylab="residual given 1992 vote", cex.main=1.2,main="Republicans running for reelection in 1994")
        resids <- -lm1$residuals
	lm3 <- lm (resids ~ score1[repinc.cond&ok])
	abline (lm3$coef[1], lm3$coef[2])
}

if (0){

postscript ("test1.ps", horizontal=T)
par (mfrow=c(3,3))
years <- seq(60,94,2)
#okyears <- years[years%%10 !=2]
#okyears <- years[years!=92]
summ <- NULL
for (year in years) summ <- rbind (summ, plan1(year))
dev.off()

postscript ("ests.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
yr <- summ[,1] + 1900
est <- (summ[,2]+summ[,4])/2
se <- sqrt(summ[,3]^2+summ[,4]^2)/2
plot (range (yr), range(est+se,est-se),type="n", cex=2, cex.lab=2, cex.axis=2, xlab="year", ylab="Estimated benefit from being a moderate", xaxt="n", yaxt="n")
axis (1, seq(1960, 2000, 10), cex.axis=2)
axis (2, 0:4, paste(0:4,"\%"), cex.axis=2)
abline (0, 0, lwd=.5, col="gray")
points (yr, est, pch=20)
for (i in 1:length(yr)){
  lines (rep(yr[i],2), est[i]+c(-1,1)*se[i], lwd=.5)
}
dev.off()

}

postscript ("liberals.ps", horizontal=T, height=7.5)
par (mar=c(5,5,4,2)+.1)
par (mfcol=c(2,2))
plan2 (94)
dev.off()

postscript ("extremists.ps", horizontal=T, height=6)
par (mar=c(5,5,4,2)+.1)
par (mfcol=c(2,1))
plan3 (94)
dev.off()

postscript ("bias.ps", horizontal=T, height=4)
par (mar=c(5,5,4,2)+.1)
par (mfcol=c(1,2))
plan4 (94)
dev.off()

postscript ("discontinuity.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
year <- 94
yr.cond <- ideo2.data[, "year"] == year
data <- ideo2.data[yr.cond,  ]
deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
dum <- apply(is.na(data),1,sum)
ok <- dum==0 & !south[data[,"stgj"]]
attach.all (data)
plot (1-dvp, score1, xlab="Republican's vote share", ylab=
"(liberal)           ideology score       (conservative)",
      cex=2, cex.lab=2, cex.axis=2, type="n")
points (1-dvp[deminc.cond], score1[deminc.cond], pch="x")
points (1-dvp[repinc.cond], score1[repinc.cond], pch="o")
dev.off()

postscript ("ideology.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
year <- 94
yr.cond <- ideo2.data[, "year"] == year
data <- ideo2.data[yr.cond,  ]
deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
dum <- apply(is.na(data),1,sum)
ok <- dum==0 & !south[data[,"stgj"]]
attach.all (data)
plot (normvote, score1, xlab="normal vote in district", ylab=
"(liberal)           ideology score       (conservative)",
      cex=2, cex.lab=2, cex.axis=2, type="n")
points (normvote[deminc.cond], score1[deminc.cond], pch="x")
points (normvote[repinc.cond], score1[repinc.cond], pch="o")
dev.off()

postscript ("ideology2.ps", horizontal=T)
par (mar=c(5,5,4,2)+.1)
year <- 94
yr.cond <- ideo2.data[, "year"] == year
data <- ideo2.data[yr.cond,  ]
deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
dum <- apply(is.na(data),1,sum)
ok <- dum==0 & !south[data[,"stgj"]]
attach.all (data)
plot (1-normvote, score1, xlab="normal vote in district", ylab=
"(liberal)           ideology score       (conservative)",
      cex=2, cex.lab=2, cex.axis=2, type="n")
points (1-normvote[deminc.cond], score1[deminc.cond], pch="x")
points (1-normvote[repinc.cond], score1[repinc.cond], pch="o")
dev.off()


if (0){
  
ideo3.data <- ideo2.data.save
for (year in seq(64,92,4)){
  yr.cond <- ideo2.data[, "year"] == year
  normvote <- rep (NA, sum(yr.cond))
  dpvote <- rep (NA, sum(yr.cond))
  stgj <- ideo2.data[yr.cond,"stgj"]
  cd <- ideo2.data[yr.cond,"cd"]
  indexes <- (1:nrow(ideo2.data))[yr.cond]
  yr2.cond <- ideo2.data[, "year"] == year-4
  data2 <- ideo2.data[yr2.cond,c("stgj","cd","normvote","dpvote")]
  for (i in 1:sum(yr.cond)){
    cond <- data2[,"stgj"]==stgj[i] & data2[,"cd"]==cd[i]
    if (sum(cond)==1){
      normvote[i] <- data2[cond,c("normvote")]
      dpvote[i] <- data2[cond,c("dpvote")]
    }
  }
  ideo3.data[yr.cond,c("normvote")] <- normvote
  ideo3.data[yr.cond,c("dpvote")] <- dpvote
}

# ideo2.data.save <- ideo2.data
# ideo2.data <- ideo3.data
}
