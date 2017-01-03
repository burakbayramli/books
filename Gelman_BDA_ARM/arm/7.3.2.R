congress <- vector ("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- paste ("../doc/gelman/arm2/cong3/", year, ".asc", sep="")
  data.year <- matrix (scan (file), byrow=TRUE, ncol=5)
  data.year <- cbind (rep(year, nrow(data.year)), data.year)
  congress[[i]] <- data.year
}

i86 <- (1986-1896)/2 + 1
cong86 <- congress[[i86]]
cong88 <- congress[[i86+1]]
cong90 <- congress[[i86+2]]

v86 <- cong86[,5]/(cong86[,5]+cong86[,6])
bad86 <- cong86[,5]==-9 | cong86[,6]==-9
v86[bad86] <- NA
contested86 <- v86>.1 & v86<.9
inc86 <- cong86[,4]

v88 <- cong88[,5]/(cong88[,5]+cong88[,6])
bad88 <- cong88[,5]==-9 | cong88[,6]==-9
v88[bad88] <- NA
contested88 <- v88>.1 & v88<.9
inc88 <- cong88[,4]

v86.adjusted <- ifelse (v86<.1, .25, ifelse (v86>.9, .75, v86))
vote.86 <- v86.adjusted[contested88]
vote.88 <- v88[contested88]
incumbency.88 <- inc88[contested88]

fit.88 <- lm (vote.88 ~ vote.86 + incumbency.88)
print (summary(fit.88))

