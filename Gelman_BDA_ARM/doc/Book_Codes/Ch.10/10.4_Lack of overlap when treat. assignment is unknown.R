## Regression discontinuity & ignorability

## Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/ideo2.dat

library ("arm")
ideo2 <- read.table ("ideo2.dat")
candidate.effects <- read.table ("candidate_effects.dat", row.names=1)

# Simple correction for incumbency advantage
incadv <- function (years){
  ifelse (years<46, .02,
    ifelse (years<66, .02 + .08*(years-46)/(66-46), .10))
}

# Regions of the country
region <- c(3,4,4,3,4,4,1,1,3,3,4,4,2,2,2,2,3,3,1,1,1,2,2,3,2,4,2,4,1,1,4,1,3,2,2,3,4,1,1,3,2,3,3,4,1,3,4,1,2,4)
south <- region==3


# add stuff to ideo2

normvote <- rep (NA, nrow(ideo2))
dum1 <- ideo2[,"dpvote"]
dum2 <- ideo2[,"stgj"]
dum3 <- (ideo2[,"year"]-44)/4
for (i in 1:length(normvote)){
  normvote[i] <- dum1[i] - candidate.effects[dum2[i],dum3[i]]
}
dvfix <- ideo2[,"dv"] - incadv(ideo2[,"year"])*ideo2[,"i2"]
dvpfix <- ideo2[,"dvp"] - incadv(ideo2[,"year"]-2)*ideo2[,"i1"]
ideo2 <- cbind(ideo2[,1:19], normvote, dvfix, dvpfix)
names(ideo2) <- c(names(ideo2)[1:19], "normvote", "dvfix", "dvpfix")

# impute normal vote from 2 years earlier:  ideo2

for (year in seq(62,94,4)){
  yr.cond <- ideo2[, "year"] == year
  normvote <- rep (NA, sum(yr.cond))
  dpvote <- rep (NA, sum(yr.cond))
  stgj <- ideo2[yr.cond,"stgj"]
  cd <- ideo2[yr.cond,"cd"]
  indexes <- (1:nrow(ideo2))[yr.cond]
  yr2.cond <- ideo2[, "year"] == year-2
  data2 <- ideo2[yr2.cond,c("stgj","cd","normvote","dpvote")]
  for (i in 1:sum(yr.cond)){
    cond <- data2[,"stgj"]==stgj[i] & data2[,"cd"]==cd[i]
    if (sum(cond)==1){
      normvote[i] <- data2[cond,c("normvote")]
      dpvote[i] <- data2[cond,c("dpvote")]
    }
  }
  ideo2[yr.cond,c("normvote")] <- normvote
  ideo2[yr.cond,c("dpvote")] <- dpvote
}

year <- 94
yr.cond <- ideo2[, "year"] == year
data <- ideo2[yr.cond,  ]
deminc.cond <- (data[, "dvp"] > 0.5) & (abs(data[,"i2"]) == 1)
repinc.cond <- (data[, "dvp"] < 0.5) & (abs(data[,"i2"]) == 1)
#
# fudge for 1992, 1994
if (year>=92) data[,"occup"] <- rep(0,nrow(data))
#
dum <- apply(is.na(data),1,sum)
ok <- dum==0 & !south[data[,"stgj"]]
attach.all (data)

 # Plot figure 10.8

postscript ("discontinuity.ps", horizontal=TRUE)
par (mar=c(5,5,4,2)+.1)
plot (1-dvp, score1, xlab="Republican's vote share", ylab=
"(liberal)           ideology score       (conservative)",
      cex=2, cex.lab=2, cex.axis=2, type="n")
points (1-dvp[deminc.cond], score1[deminc.cond], pch="x")
points (1-dvp[repinc.cond], score1[repinc.cond], pch="o")
dev.off ()

 # regression discontinuity analysis

x <- 1 - dvp
party <- ifelse (dvp<.5, 1, 0)

## Regression in the area near the discontinuity

overlap <- (deminc.cond | repinc.cond) & dvp>.45 & dvp<.55
fit.1 <- lm (score1 ~ party + x, subset=overlap)
display (fit.1)

## Regression fit to all data

incs <- (deminc.cond | repinc.cond)

fit.2 <- lm (score1 ~ party + x, subset=incs)
display (fit.2)

## Regression with interactions

fit.3 <- lm (score1 ~ party + x + party:x, subset=incs)
display (fit.3)

## Reparametrized regression

z <- x - 0.5
fit.4 <- lm (score1 ~ party + I(z*(party==0)) + I(z*(party==1)), subset=incs)
display (fit.4)
