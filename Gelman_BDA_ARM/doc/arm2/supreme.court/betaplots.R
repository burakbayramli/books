load("d:/scj_run03.05.23.RData")

par(mfrow=c(3,1), pty="s")

attach(sc.sim$sims.list)
summ<-as.data.frame(sc.sim$summary)
attach(summ)
plot(apply(z.beta2,2,median), apply(z.beta2,2,sd), pch=Rhat)



    library(foreign)
    term<-read.dta("c:/term.dta")
    year<-term
    stupid<-cbind(year+runif(nrow(year),-.1,.1),z.beta2[1,])
    plot(stupid, pch=".", xlab="Supreme Court Term", ylab="Ideological Position")

#main="Draws from Standardized Difficulty and Ideal Point Posterior Distributions",
    #lines(lowess(stupid))

    #stupid2<-cbind(year,z.beta2[2,])
    #plot(stupid2, pch=".")

    lines (c(1954,1970),rep(median(z.alpha[,1]),2), col=2)
    lines (c(1953,1970),rep(median(z.alpha[,2]),2), col=2)
    lines (c(1953,1975),rep(median(z.alpha[,3]),2), col=2)
    lines (c(1958,1980),rep(median(z.alpha[,4]),2), col=2)
    lines (c(1967,1990),rep(median(z.alpha[,5]),2), col=2)
    lines (c(1956,1989),rep(median(z.alpha[,6]),2), col=2)
    lines (c(1961,1992),rep(median(z.alpha[,7]),2), col=2)
    lines (c(1953,1968),rep(median(z.alpha[,8]),2), col=2)
    lines (c(1953,1966),rep(median(z.alpha[,9]),2), col=2)
    lines (c(1953,1961),rep(median(z.alpha[,10]),2), col=2)
    lines (c(1956,1961),rep(median(z.alpha[,11]),2), col=2)
    lines (c(1953,1958),rep(median(z.alpha[,12]),2), col=2)
    lines (c(1953,1956),rep(median(z.alpha[,13]),2), col=2)
    lines (c(1965,1968),rep(median(z.alpha[,14]),2), col=2)
    lines (c(1962,1964),rep(median(z.alpha[,15]),2), col=2)
    lines (c(1953,1955),rep(median(z.alpha[,16]),2), col=2)
    points (c(1953,1953),rep(median(z.alpha[,17]),2), col=2)
    lines (c(1969,1985),rep(median(z.alpha[,18]),2), col=2)
    lines (c(1969,1993),rep(median(z.alpha[,19]),2), col=2)
    lines (c(1971,1986),rep(median(z.alpha[,20]),2), col=2)
    lines (c(1971,2000),rep(median(z.alpha[,21]),2), col=2)
    lines (c(1975,2000),rep(median(z.alpha[,22]),2), col=2)
    lines (c(1981,2000),rep(median(z.alpha[,23]),2), col=2)
    lines (c(1986,2000),rep(median(z.alpha[,24]),2), col=2)
    lines (c(1987,2000),rep(median(z.alpha[,25]),2), col=2)
    lines (c(1990,2000),rep(median(z.alpha[,26]),2), col=2)
    lines (c(1991,2000),rep(median(z.alpha[,27]),2), col=2)
    lines (c(1993,2000),rep(median(z.alpha[,28]),2), col=2)
    lines (c(1994,2000),rep(median(z.alpha[,29]),2), col=2)



stupid2<-cbind(year+runif(nrow(year),-.1,.1),z.beta1[1,])
plot(stupid2, pch=".", xlab="Supreme Court Term", ylab="Discrimination", 
cex.main=2)
