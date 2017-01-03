# testPortfolioOptim.r
#

# load tseries library
library(tseries)

?portfolio.optim

methods(portfolio.optim)
# notice how the function is not displayed
portfolio.optim.default
# use getS3method to display function
getS3method("portfolio.optim","default")
getS3method("portfolio.optim","ts")

# use lab7 example data to test functions
lab7.df = read.csv("C:/classes/econ424/fall2008/econ424lab7returns.csv",
                  stringsAsFactors=F)
colnames(lab7.df)
ret.mat = as.matrix(lab7.df[,-1])

# estimate mu and Sigma
mu.hat = colMeans(ret.mat)
cov.hat = var(ret.mat)
mu.hat
cov.hat

# compute efficient portfolio with target return equal to mean
# of msft using portfolio function
ep = efficient.portfolio(mu.hat, cov.hat, mu.hat[1])
ep
# compute tangency port
tp = tangency.portfolio(mu.hat, cov.hat, 0.005)
tp
# compute global min port
gp = globalMin.portfolio(mu.hat, cov.hat)
gp

# compute efficient frontier
ef = efficient.frontier(mu.hat, cov.hat, 
                        alpha.min=0, alpha.max=1)


# compute efficient portfolio using portfolio.optim
ep2 = portfolio.optim(ret.mat, pm = mu.hat[1], shorts=T)
names(ep2)
ep2$pm
ep2$ps
ep2$pw

# compute efficient portfolio not allowing short sales
ep2.ns = portfolio.optim(ret.mat, pm = mu.hat[1], shorts=F)
ep2.ns$pm
ep2.ns$ps
ep2.ns$pw

# compute tangency portfolio
tp2 = portfolio.optim(ret.mat, pm = tp$er, shorts=T)
tp2$pm
tp2$ps
tp2$pw

# compute efficient frontier with short sales

results.mat = matrix(0, 20, 6)
colnames(results.mat) = c("er","sd",colnames(ret.mat))
targetRet.vec = seq(from=gp$er, to=max(mu.hat), length=20)
for (i in 1:20) {
	tmp.p = portfolio.optim(ret.mat, pm = targetRet.vec[i], 
                              shorts=F)
	results.mat[i,] = c(tmp.p$pm,tmp.p$ps,tmp.p$pw)
} 

# plot efficient frontier
plot(ef$sd, ef$er, type="b", col="blue", main="Efficient Frontier",
     xlab="portfolio sd", ylab="portfolio er")
points(results.mat[,"sd"], results.mat[,"er"], type="b",
       col="red")
legend(x="topleft", legend=c("Short sales", "No short sales"),
       lty=c(1,1), pch=c("o","o"), col=c("blue","red"))
     



