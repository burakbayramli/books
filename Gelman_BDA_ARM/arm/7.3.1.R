congress <- vector ("list", 49)
for (i in 1:49){
  year <- 1896 + 2*(i-1)
  file <- paste ("../doc/gelman/arm2/cong3/", year, ".asc", sep="")
  data.year <- matrix (scan (file), byrow=TRUE, ncol=5)
  data.year <- cbind (rep(year, nrow(data.year)), data.year)
  congress[[i]] <- data.year
}

print (congress[[47]])

i86 <- (1986-1896)/2 + 1
cong88 <- congress[[i86+1]]

v88 <- cong88[,5]/(cong88[,5]+cong88[,6])
bad88 <- cong88[,5]==-9 | cong88[,6]==-9
v88[bad88] <- NA
contested88 <- v88>.1 & v88<.9
inc88 <- cong88[,4]

v88.hist <- ifelse (v88<.1, .0001, ifelse (v88>.9, .9999, v88))
hist (v88.hist, breaks=seq(0,1,.05),
  xlab="Democratic share of the two-party vote", ylab="", yaxt="n",
  cex.axis=2, cex.lab=2, cex.main=2, main="Congressional elections in 1988")
